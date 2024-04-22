MODULE badprocint2 ;

FROM StrIO IMPORT WriteString, WriteLn ;

PROCEDURE PassRef (VAR x: CARDINAL) ;
BEGIN
END PassRef ;

VAR
   i: INTEGER ;
BEGIN
   i := 42 ;
   WriteString ('the value is: ') ; PassRef (i) ; WriteLn
END badprocint2.
