MODULE testdispose3 ;

FROM Storage IMPORT DEALLOCATE ;

TYPE
   PtrToVec = POINTER TO RECORD
                            x, y: INTEGER ;
                         END ;


PROCEDURE test (VAR ptr: PtrToVec) ;
BEGIN
   DISPOSE (ptr) ;
   IF ptr^.x = 1
   THEN
   END
END test ;


VAR
   p: PtrToVec ;
BEGIN
   test (p)
END testdispose3.
