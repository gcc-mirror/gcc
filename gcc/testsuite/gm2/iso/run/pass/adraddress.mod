MODULE adraddress;

FROM SYSTEM IMPORT ADR, ADDRESS ;
IMPORT STextIO;
FROM libc IMPORT exit, printf ;


PROCEDURE assert (b: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf("%s:%d:assert failed\n", ADR(f), l) ;
      r := 1
   END
END assert ;


TYPE
   NamePtr = POINTER TO ARRAY [0..99] OF CHAR;
   NamePtrPtr = POINTER TO NamePtr;

VAR
   strp  : ARRAY [0..9] OF ADDRESS;
   name  : ARRAY [0..99] OF CHAR;
   namep : NamePtr;
   namepp: NamePtrPtr;
   r     : INTEGER ;
BEGIN
   name := "test";
   strp[0] := ADR(name);
   namepp := ADR(strp[0]);
   assert(strp[0]#namepp, __FILE__, __LINE__) ;
   namep := namepp^;
   assert(strp[0]=namep, __FILE__, __LINE__) ;
   assert(ADR(name)=namep, __FILE__, __LINE__) ;

   STextIO.WriteString(namep^); STextIO.WriteLn;
   IF r#0
   THEN
      exit(r)
   END
END adraddress.
