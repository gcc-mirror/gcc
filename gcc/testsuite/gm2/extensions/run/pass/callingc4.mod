MODULE callingc4 ;

FROM libc IMPORT printf, exit ;
FROM StrLib IMPORT StrLen ;

VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   a := "\n"
END callingc4.
