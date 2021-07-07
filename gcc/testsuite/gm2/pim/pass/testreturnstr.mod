MODULE testreturnstr ;

FROM InOut IMPORT WriteString, WriteLn ;

TYPE
   teststr = ARRAY [0..9] OF CHAR;

PROCEDURE test() : teststr ;
VAR
   f: teststr ;
BEGIN
   f := "test" ;
   RETURN( f )
END test;

BEGIN
   WriteString('test = "') ;
   WriteString(test()) ;
   WriteString('"') ;
   WriteLn
END testreturnstr.
