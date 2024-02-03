MODULE arithpromote ;

IMPORT SYSTEM ;
FROM libc IMPORT exit, printf ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteLn ;


PROCEDURE assert (computed, result: CARDINAL; message: ARRAY OF CHAR) ;
BEGIN
   IF computed # result
   THEN
      printf (message, computed, result) ;
      exit (1)
   END
END assert ;


PROCEDURE testCardinal ;
VAR
   c64: SYSTEM.CARDINAL64 ;
   c32: SYSTEM.CARDINAL32 ;
   c16: SYSTEM.CARDINAL32 ;
   c8 : SYSTEM.CARDINAL8 ;
BEGIN
   c8 := 7 ;
   c16 := 7000H ;
   c32 := 7 ;
   c64 := 0000000100000000H ;
(*
   assert (c16 + c8, 7007H, "addition between CARDINAL16 and CARDINAL8 fails: %d # %d\n") ;
   c64 := 0000000100000000H ;
*)
(*
   IF c64 + c8 # 0000000100000007H
   THEN
      printf ("failure when adding 0000000100000000H + 7\n");
      exit (1)
   END
*)
(*
   IF c64 + c32 # 0000000100000007H
   THEN
      printf ("failure when adding 0000000100000000H + 7\n");
      exit (1)
   END
*)
   c64 := 16 * c64 + c32;  (* Should fail here.  *)
   c64 := c32 + c64 ;
END testCardinal ;


BEGIN
   testCardinal
END arithpromote.
