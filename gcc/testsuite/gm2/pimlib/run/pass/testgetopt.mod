MODULE testgetopt ;

FROM libc IMPORT printf, exit ;
FROM GetOpt IMPORT InitLongOptions, KillLongOptions, AddLongOption,
                   GetOptLong, PtrToInteger, LongOptions ;
FROM DynamicStrings IMPORT String, InitString, string ;
IMPORT UnixArgs ;
FROM Storage IMPORT ALLOCATE ;
FROM SYSTEM IMPORT ADR ;


(*
   Assert -
*)

PROCEDURE Assert (condition: BOOLEAN) ;
BEGIN
   IF NOT condition
   THEN
      printf ("assert failed, condition is false\n") ;
      exit (1)
   END
END Assert ;



(*
   test -
*)

PROCEDURE test ;
VAR
   result   : INTEGER ;
   optstring: String ;
   i, val   : INTEGER ;
   ch       : CHAR ;
BEGIN
   longopts := AddLongOption (longopts, 0, InitString ('help'), 0, result, 0) ;
   longopts := AddLongOption (longopts, 1, InitString ('dir'), 1, result, 0) ;
   longopts := AddLongOption (longopts, 2, NIL, 0, result, 0) ;
   optstring := InitString ('hd:') ;
   i := 1 ;
   REPEAT
      val := GetOptLong (UnixArgs.GetArgC (), UnixArgs.GetArgV (),
                         optstring, longopts, i) ;
      IF val = 0
      THEN
         printf ("long option detected, result = %d, val = %d, index i = %d, optstring = %s\n",
                 result, val, i, string (optstring))
      ELSIF val > 0
      THEN
         ch := VAL (CHAR, val) ;
         CASE ch OF

         'h': printf ("short option 'h' seen\n")

         ELSE
            printf ("unknown short option '%c' seen\n", ch)
         END
      ELSE
         printf ("unknown long option\n")
      END ;
      INC (i)
   UNTIL val <= 0
END test ;


VAR
   longopts: LongOptions ;
BEGIN
   longopts := InitLongOptions () ;
   test ;
   longopts := KillLongOptions (longopts)
END testgetopt.
