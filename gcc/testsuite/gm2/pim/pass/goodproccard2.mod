MODULE badproccard2 ;

FROM StrIO IMPORT WriteString, WriteLn ;

PROCEDURE func () : INTEGER ;
BEGIN
   RETURN 42
END func ;

PROCEDURE PassValue (x: CARDINAL) ;
BEGIN
END PassValue ;

BEGIN
   WriteString ('the value is: ') ; PassValue (func ()) ; WriteLn
END badproccard2.
