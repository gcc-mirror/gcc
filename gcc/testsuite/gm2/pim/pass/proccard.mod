MODULE proccard ;

FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;

PROCEDURE func () : CARDINAL ;
BEGIN
   RETURN 42
END func ;

BEGIN
   WriteString ('the value is: ') ; WriteCard (VAL (CARDINAL, func), 5) ; WriteLn
END proccard.
