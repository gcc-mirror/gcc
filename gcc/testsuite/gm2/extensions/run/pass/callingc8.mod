MODULE callingc8 ;

FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrLen ;

VAR
   tinyarray: ARRAY [0..1] OF CHAR ;
BEGIN
   tinyarray := "ab"
END callingc8.
