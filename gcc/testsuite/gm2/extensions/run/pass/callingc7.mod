MODULE callingc7 ;

FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrLen ;

VAR
   tinyarray: ARRAY [0..1] OF CHAR ;
BEGIN
   tinyarray := "b"
END callingc7.
