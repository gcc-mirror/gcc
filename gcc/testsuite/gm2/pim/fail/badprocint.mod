MODULE badprocint ;

FROM StrIO IMPORT WriteString, WriteLn ;

PROCEDURE func () : INTEGER ;
BEGIN
   RETURN 42
END func ;

PROCEDURE PassRef (VAR x: CARDINAL) ;
BEGIN
END PassRef ;


BEGIN
   WriteString ('the value is: ') ; PassRef (func ()) ; WriteLn
END badprocint.
