MODULE testdispose ;

FROM Storage IMPORT DEALLOCATE ;

TYPE
   PtrToVec = POINTER TO RECORD
                            x, y: INTEGER ;
                         END ;


PROCEDURE test ;
VAR
   ptr: PtrToVec ;
BEGIN
   DISPOSE (ptr) ;
   IF ptr^.x = 1
   THEN
   END
END test ;


BEGIN
   test
END testdispose.
