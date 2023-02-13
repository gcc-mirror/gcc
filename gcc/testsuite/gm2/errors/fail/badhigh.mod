MODULE badhigh;
VAR
    i, high : CARDINAL;
BEGIN
    high := 10;
    WHILE i < HIGH DO
        INC(i);
    END;
END badhigh.