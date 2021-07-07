IMPLEMENTATION MODULE testcse50 ;


FROM SYSTEM IMPORT ADR ;
FROM libc IMPORT getenv ;
FROM ASCII IMPORT nul ;
FROM StrLib IMPORT StrCopy ;


PROCEDURE GetEnvironment (Env: ARRAY OF CHAR; VAR a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   High,
   i   : CARDINAL ;
   Addr: POINTER TO CHAR ;
BEGIN
   i := 0 ;
   High := HIGH(a) ;
   Addr := getenv(ADR(Env)) ;
   WHILE (i<High) AND (Addr#NIL) AND (Addr^#nul) DO
      a[i] := Addr^ ;
      INC(Addr) ;
      INC(i)
   END ;
   IF i<High
   THEN
      a[i] := nul
   END ;
   RETURN( Addr#NIL )
END GetEnvironment ;

VAR
   a: ARRAY [0..10] OF CHAR ;
BEGIN
   IF GetEnvironment('foobar', a)
   THEN
   END
END testcse50.
