MODULE arith1 ;

IMPORT SYSTEM ;
FROM libc IMPORT exit, printf ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteLn ;


PROCEDURE assert (computed, result: CARDINAL; message: ARRAY OF CHAR) ;
BEGIN
   IF computed # result
   THEN
      printf (message, computed, result) ;
      exit (1)
   END
END assert ;


PROCEDURE testCardinal ;
VAR
   c64: SYSTEM.CARDINAL64 ;
   c32: SYSTEM.CARDINAL32 ;
   c16: SYSTEM.CARDINAL32 ;
   c8 : SYSTEM.CARDINAL8 ;
BEGIN
   c8 := 7 ;
   c16 := 7000H ;
   c32 := 7 ;
   c64 := 0000000100000000H ;
   c16 := c16 + c8 ;
END testCardinal ;


BEGIN
   testCardinal
END arith1.
