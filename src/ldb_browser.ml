open Core.Std

let main tail max_ticks dbpath () =
  let buf = Bytes.create 24 in
  let nb_read = ref 0 in
  let f = if tail then LevelDB.rev_iter else LevelDB.iter in
  let db = LevelDB.open_db dbpath in
  let ts_offset = ref Int64.zero in
  try
    f (fun ts data ->
        String.blit ts 0 buf 0 8;
        String.blit data 0 buf 8 16;
        let tick = Tick.IO.Bytes.read_tdts buf 0 in
        let rec_num = !nb_read in
        let price  = Int64.to_float tick#p *. 1e-8 in
        let vol = tick#v in
        let side = match tick#side with `Buy -> 0 | `Sell -> 1 in
        let ts =
          if rec_num = 0 then Int64.zero else begin
            let open Int64 in (tick#ts - !ts_offset) / Int64.of_float 1e5 end in
        ts_offset := tick#ts;
        Format.(fprintf std_formatter "%Ld %.2f %Ld %d@." ts price vol side);
        incr nb_read;
        Option.value_map max_ticks
          ~default:true ~f:(fun max_ticks -> not (!nb_read = max_ticks))
      ) db;
    LevelDB.close db
  with _ -> LevelDB.close db

let command =
  let spec =
    let open Command.Spec in
    empty
    +> flag "-tail" no_arg ~doc:" Show latest records"
    +> flag "-n" (optional int) ~doc:"n Number of ticks to display (default: all)"
    +> anon ("db" %: string)
  in
  Command.basic ~summary:"Browser for tick files" spec main

let () = Command.run command


