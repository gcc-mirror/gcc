MODULE longint4 ;

FROM libc IMPORT exit ;


PROCEDURE myfunc (x: LONGREAL) : LONGREAL ;
BEGIN
     RETURN x
END myfunc ;


PROCEDURE test ;
VAR
     res : LONGREAL ;
BEGIN
     res := myfunc (1.0)
END test ;

BEGIN
   test
END longint4.
