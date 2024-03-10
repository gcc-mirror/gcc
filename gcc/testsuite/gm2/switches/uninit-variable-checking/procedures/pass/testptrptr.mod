MODULE testptrptr ;

FROM SYSTEM IMPORT ADR ;

TYPE
   PtrToPtrToVec = POINTER TO PtrToVec ;
   PtrToVec = POINTER TO Vec ;
   Vec = RECORD
            x, y: INTEGER ;
         END ;


PROCEDURE test ;
VAR
   vec: Vec ;
   ptr: PtrToVec ;
   ptrptr: PtrToPtrToVec ;
BEGIN
   ptr := ADR (vec) ;
   ptrptr := ADR (ptr) ;
   WITH ptrptr^^ DO
      x := 1
   END
END test ;


BEGIN
   test
END testptrptr.
