MODULE goodprocint3 ;

FROM StrIO IMPORT WriteString, WriteLn ;

PROCEDURE PassValue (x: CARDINAL) ;
BEGIN
END PassValue ;

VAR
   i: INTEGER ;
BEGIN
   i := 42 ;
   WriteString ('the value is: ') ; PassValue (i) ; WriteLn
END goodprocint3.
