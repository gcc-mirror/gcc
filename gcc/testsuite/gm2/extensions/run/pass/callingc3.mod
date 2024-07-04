MODULE callingc3 ;

FROM libc IMPORT exit ;
FROM StrLib IMPORT StrLen ;

VAR
   a: ARRAY [0..1] OF CHAR ;
BEGIN
   IF StrLen ("\n") # 2
   THEN
      exit (1)
   END
END callingc3.
