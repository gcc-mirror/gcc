MODULE uninitptr ;

VAR
   p: POINTER TO CHAR ;
BEGIN
   p^ := 'a'
END uninitptr.
