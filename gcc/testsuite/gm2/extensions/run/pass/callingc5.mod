MODULE callingc5 ;

FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrLen ;

VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   a := "a"
END callingc5.
