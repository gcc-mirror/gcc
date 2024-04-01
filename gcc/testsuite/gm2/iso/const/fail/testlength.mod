MODULE testlength ;

PROCEDURE bar (a: ARRAY OF CHAR) ;
CONST
   foo = LENGTH (a) ;
BEGIN
END bar ;

BEGIN
   bar ("hello")
END testlength.
