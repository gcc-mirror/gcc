IMPLEMENTATION MODULE testisnormal ;

FROM libc IMPORT printf, exit ;
FROM Builtins IMPORT isnormal ;
FROM SYSTEM IMPORT ADR ;


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
   result: INTEGER ;
BEGIN
   result := isnormal (1.0) ;
   printf ("isnormal (1.0) = %d\n", result) ;
   assert (result = 1, "isnormal (1.0) # 1")
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
END testisnormal.
