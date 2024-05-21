MODULE testwritereal ;

FROM STextIO IMPORT WriteString, WriteLn ;
FROM SRealIO IMPORT WriteFloat ;
FROM RealMath IMPORT pi ;

BEGIN
   WriteString ('value of pi = ') ; WriteFloat (pi, 0, 0) ; WriteLn
END testwritereal.
