MODULE testnew2 ;

FROM Storage IMPORT ALLOCATE ;

TYPE
   ptr = POINTER TO RECORD
                       a, b: CARDINAL ;
                    END ;

(*
   test -
*)

PROCEDURE test ;
VAR
   p: ptr ;
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


BEGIN
   test
END testnew2.
