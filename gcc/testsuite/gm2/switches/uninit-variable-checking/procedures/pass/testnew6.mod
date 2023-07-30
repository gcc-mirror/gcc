MODULE testnew6 ;

FROM Storage IMPORT ALLOCATE ;

TYPE
   ptr = POINTER TO RECORD
                       a, b: CARDINAL ;
                    END ;

(*
   test -
*)

PROCEDURE test (p: ptr) ;
BEGIN
   NEW (p) ;
   WITH p^ DO
      a := 1 ;
   END
END test ;


VAR
   n: ptr ;
BEGIN
   test (n)
END testnew6.
