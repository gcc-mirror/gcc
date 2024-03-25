MODULE shorttest ;

FROM ShortStr IMPORT RealToStr ;
FROM STextIO IMPORT WriteString, WriteLn ;
FROM ShortMath IMPORT pi ;

VAR
   buf: ARRAY [0..30] OF CHAR ;
BEGIN
   WriteString ("pi = ") ;
   RealToStr (pi, buf) ;
   WriteString (buf) ; WriteLn
END shorttest.
