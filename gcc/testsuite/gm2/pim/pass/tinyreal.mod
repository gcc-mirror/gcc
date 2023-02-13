MODULE tinyreal ;

FROM InOut IMPORT WriteLn, WriteString ;
FROM FpuIO IMPORT WriteReal, ReadReal ;

VAR
   time: REAL;
BEGIN
   WriteString('Input value ');
   ReadReal(time) ;
   WriteReal(time,7,5); WriteLn ;
END tinyreal.
