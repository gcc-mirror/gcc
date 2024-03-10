MODULE longint8 ;

FROM libc IMPORT exit ;
FROM Builtins IMPORT log10l ;


PROCEDURE myfunc (x: LONGREAL) : LONGREAL ;
     VAR
	  y: LONGREAL ;
BEGIN
     y := log10l (x) ;
     RETURN y
END myfunc ;


PROCEDURE test ;
VAR
     res : LONGREAL ;
BEGIN
     res := myfunc (1.0)
END test ;

BEGIN
   test
END longint8.
