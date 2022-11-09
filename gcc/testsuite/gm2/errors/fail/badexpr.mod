MODULE badexpr ;

FROM SYSTEM IMPORT ADDRESS ;

VAR
    i: CARDINAL;
    a: ADDRESS ;
BEGIN
    a := NIL;
    WHILE i < a DO
        INC (i)
    END;
END badexpr.
