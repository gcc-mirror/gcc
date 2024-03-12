MODULE assignparam ;

FROM SYSTEM IMPORT ADR ;

TYPE
   PtrToVec = POINTER TO Vec ;
   Vec = RECORD
            x, y: INTEGER ;
         END ;


PROCEDURE test (p: PtrToVec) ;
VAR
   s: PtrToVec ;
   v: Vec ;
BEGIN
   s := ADR (v) ;
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
END assignparam.
