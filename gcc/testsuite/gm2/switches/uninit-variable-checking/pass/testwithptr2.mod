MODULE testwithptr2 ;

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
   p^ := Vec {1, 2} ;
   IF p^.y = 2
   THEN
   END
END test ;


BEGIN
   test
END testwithptr2.
