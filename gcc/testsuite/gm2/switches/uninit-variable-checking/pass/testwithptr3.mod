MODULE testwithptr3 ;

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
   v := Vec {1, 2} ;
   IF p^.y = 2
   THEN
   END
END test ;


BEGIN
   test
END testwithptr3.
