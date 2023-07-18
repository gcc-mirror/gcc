MODULE testnew5 ;

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
      (* user forgets to assign b.  *)
      IF b = 2
      THEN
      END
   END
END test ;


VAR
   n: ptr ;
BEGIN
   test (n)
END testnew5.
