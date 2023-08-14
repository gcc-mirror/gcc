MODULE testnew4 ;

FROM Storage IMPORT ALLOCATE ;
FROM SYSTEM IMPORT ADR ;

TYPE
   ptr = POINTER TO rec ;
   rec = RECORD
             a, b: CARDINAL ;
         END ;

(*
   test -
*)

PROCEDURE test ;
VAR
   p: ptr ;
   r: rec ;
BEGIN
   p := ADR (r) ;
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
END testnew4.
