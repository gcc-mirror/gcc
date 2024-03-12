MODULE assignparam2 ;

FROM SYSTEM IMPORT ADR ;
FROM Storage IMPORT ALLOCATE ;

TYPE
   PtrToVec = POINTER TO Vec ;
   Vec = RECORD
            x, y: INTEGER ;
         END ;


PROCEDURE test (p: PtrToVec) ;
VAR
   s: PtrToVec ;
BEGIN
   NEW (s) ;
   s^ := p^ ;
   IF s^.x = 1
   THEN
   END
END test ;


VAR
   q: PtrToVec ;
   w: Vec ;
BEGIN
   q := ADR (w) ;
   test (q)
END assignparam2.
