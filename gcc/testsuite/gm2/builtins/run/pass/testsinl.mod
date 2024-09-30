IMPLEMENTATION MODULE testsinl ;

FROM libc IMPORT printf ;
FROM Builtins IMPORT sinl ;


(*
   test -
*)

PROCEDURE test ;
VAR
   result: LONGREAL ;
BEGIN
   result := sinl (3.14) ;
   printf ("sinl (3.14) = %lg\n", result) ;
END test ;


BEGIN
   test ;
   printf ("all tests pass\n")
END testsinl.
