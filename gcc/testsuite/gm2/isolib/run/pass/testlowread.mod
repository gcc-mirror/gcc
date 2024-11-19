MODULE testlowread ;

FROM LowReal IMPORT places ;
FROM STextIO IMPORT WriteString, WriteLn ;
FROM SWholeIO IMPORT WriteCard ;

BEGIN
   WriteString ('value of places = ') ; WriteCard (places, 0) ; WriteLn
END testlowread.
