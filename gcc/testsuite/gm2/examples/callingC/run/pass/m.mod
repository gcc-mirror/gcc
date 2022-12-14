MODULE m ;

IMPORT c ;
FROM SYSTEM IMPORT THROW, ADDRESS ;

PROCEDURE foo (file : ARRAY OF CHAR) : ADDRESS ;
BEGIN
   IF c.bar(file, 'rb')
   THEN
   END ;
   RETURN NIL ;
END foo ;


VAR
   s: ADDRESS ;
BEGIN
   s := foo('test.bmp')
END m.
