MODULE badset7 ;

FROM SYSTEM IMPORT WORD ;

PROCEDURE func () : WORD ;
BEGIN
   RETURN WORD (0)
END func ;

VAR
   b: BITSET ;
BEGIN
   b := func () - {6..31}
END badset7.
