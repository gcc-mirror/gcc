MODULE longint5 ;

FROM libc IMPORT exit ;


PROCEDURE myfunc (x: LONGREAL) : LONGREAL ;
     VAR
	  y: LONGREAL ;
BEGIN
     y := 1.2 ;
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
END longint5.
