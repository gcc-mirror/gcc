MODULE uninitptr2 ;

PROCEDURE foo ;
VAR
   p: POINTER TO CHAR ;
BEGIN
   p^ := 'a'
END foo ;

BEGIN
   foo
END uninitptr2.
