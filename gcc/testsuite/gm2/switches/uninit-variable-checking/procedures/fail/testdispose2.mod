MODULE testdispose2 ;

FROM Storage IMPORT DEALLOCATE ;

TYPE
   PtrToVec = POINTER TO RECORD
                            x, y: INTEGER ;
                         END ;


PROCEDURE test (ptr: PtrToVec) ;
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
END testdispose2.
