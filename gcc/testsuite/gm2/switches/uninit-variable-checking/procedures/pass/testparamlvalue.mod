MODULE testparamlvalue ;

FROM SYSTEM IMPORT ADR ;

TYPE
   PtrToVec = POINTER TO Vec ;
   Vec = RECORD
            x, y: INTEGER ;
         END ;


PROCEDURE test (VAR p: PtrToVec) ;
BEGIN
   IF p^.x = 1
   THEN
   END
END test ;


VAR
   q: PtrToVec ;
   v: Vec ;
BEGIN
   q := ADR (v) ;
   test (q)
END testparamlvalue.
