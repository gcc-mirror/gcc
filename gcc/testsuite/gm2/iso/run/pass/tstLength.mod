MODULE tstLength;

FROM M2RTS IMPORT Length ;
FROM StrIO IMPORT WriteLn, WriteString ;
FROM NumberIO IMPORT WriteCard ;
FROM libc IMPORT exit ;

VAR
   s: ARRAY [1..5] OF CHAR;
BEGIN
   s := "What?";

   WriteString("LENGTH(s) reports ");
   WriteCard(LENGTH(s), 4); WriteLn;

   WriteString("Length(s) reports ");
   WriteCard(Length(s), 4); WriteLn;
   IF LENGTH(s)#Length(s)
   THEN
      exit(1)
   END
END tstLength.
