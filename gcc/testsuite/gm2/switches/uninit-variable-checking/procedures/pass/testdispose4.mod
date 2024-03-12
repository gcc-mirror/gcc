MODULE testdispose4 ;

FROM Storage IMPORT ALLOCATE ;

TYPE
   PtrToCard = POINTER TO CARDINAL ;


PROCEDURE test (VAR ptr: PtrToCard) ;
BEGIN
   IF ptr^ = 1
   THEN
   END
END test ;


VAR
   p: PtrToCard ;
BEGIN
   NEW (p) ;
   test (p)
END testdispose4.
