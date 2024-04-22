MODULE badprocbool ;

FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;

PROCEDURE Func () : BOOLEAN ;
BEGIN
   RETURN TRUE
END Func ;

BEGIN
   WriteString ('the value is: ') ; WriteCard (Func (), 5) ; WriteLn
END badprocbool.
