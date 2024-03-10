MODULE testwithptr ;

FROM SYSTEM IMPORT ADR ;

TYPE
   PtrToVec =  POINTER TO Vec ;
   Vec = RECORD
            x, y: CARDINAL ;
         END ;


(*
   test -
*)

PROCEDURE test ;
VAR
   p: PtrToVec ;
   v: Vec ;
BEGIN
   p := ADR (v) ;
   WITH p^ DO
      x := 1 ;
      y := 2
   END ;
   IF p^.y = 2
   THEN
   END
END test ;


BEGIN
   test
END testwithptr.
