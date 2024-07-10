MODULE testalloa ;

FROM libc IMPORT printf, exit ;
FROM Builtins IMPORT alloca ;
FROM SYSTEM IMPORT ADR, ADDRESS ;


(*
   assert -
*)

PROCEDURE assert (value: BOOLEAN; message: ARRAY OF CHAR) ;
BEGIN
   IF NOT value
   THEN
      printf ("test failed: %s\n", ADR (message)) ;
      code := 1
   END
END assert ;


(*
   test -
*)

PROCEDURE test ;
VAR
   ptr: ADDRESS ;
BEGIN
   ptr := alloca (10) ;
   assert (ptr # NIL, "alloca (10) # NIL")
END test ;


VAR
   code: INTEGER ;
BEGIN
   code := 0 ;
   test ;
   IF code = 0
   THEN
      printf ("all tests pass\n")
   ELSE
      printf ("some tests failed\n")
   END ;
   exit (code)
END testalloa.
