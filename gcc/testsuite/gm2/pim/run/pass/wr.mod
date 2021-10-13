MODULE wr;

IMPORT FIO;

FROM StrIO IMPORT WriteString, WriteLn, ReadString;
FROM StrLib IMPORT StrEqual;
FROM NumberIO IMPORT WriteInt,WriteCard;


PROCEDURE Overall;
VAR
   in,out : CARDINAL;
   fnum1 : FIO.File;
BEGIN
   fnum1 := FIO.OpenToWrite('results.dat');
   FOR out :=1 TO 9 DO
      FIO.WriteCardinal(fnum1,out);
      FIO.WriteLine(fnum1);
   END ; (* outer for *)
   FIO.Close(fnum1)
END Overall;


BEGIN (*main program*)
   Overall
END wr.
