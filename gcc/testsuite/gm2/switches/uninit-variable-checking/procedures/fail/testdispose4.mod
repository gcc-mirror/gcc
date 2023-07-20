MODULE testdispose4 ;

FROM Storage IMPORT DEALLOCATE ;

TYPE
   PtrToCard = POINTER TO CARDINAL ;


PROCEDURE test (VAR ptr: PtrToCard) ;
BEGIN
   DISPOSE (ptr) ;
   IF ptr^ = 1
   THEN
   END
END test ;


VAR
   p: PtrToCard ;
BEGIN
   test (p)
END testdispose4.
