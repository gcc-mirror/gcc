MODULE testfiobinary ;

(* Simple test to stress FIO.WriteCardinal.  *)

FROM FIO IMPORT WriteCardinal, File, OpenToRead, OpenToWrite, Close, ReadNBytes, IsNoError, ReadCardinal ;
FROM libc IMPORT exit, printf ;


CONST
   OutputName = "binary.bin" ;
   Debugging = TRUE ;


PROCEDURE Check (bool: BOOLEAN) ;
BEGIN
   IF NOT bool
   THEN
      printf ("check assert failed\n");
      exit (1)
   END
END Check ;


PROCEDURE Write (f: File; card: CARDINAL) ;
BEGIN
   WriteCardinal (f, card)
END Write ;


PROCEDURE Read (f: File; card: CARDINAL) ;
VAR
   value: CARDINAL ;
BEGIN
   value := ReadCardinal (f) ;
   IF value # card
   THEN
      printf ("Read failed to read cardinal value, expecting %d and read %d\n",
              card, value) ;
      exit (2)
   END
END Read ;


PROCEDURE CreateBinary ;
VAR
   f: File ;
BEGIN
   f := OpenToWrite (OutputName) ;
   Check (IsNoError (f)) ;
   IF SIZE (CARDINAL) >= 4
   THEN
      Write (f, 012345678H)
   END ;
   Write (f, 0) ;
   Write (f, 1) ;
   Write (f, 2) ;
   Write (f, 3) ;
   Write (f, 1000) ;
   Write (f, 1024) ;
   Write (f, 32767) ;
   Close (f)
END CreateBinary ;


PROCEDURE CheckBinary ;
VAR
   f: File ;
BEGIN
   f := OpenToRead (OutputName) ;
   Check (IsNoError (f)) ;
   IF SIZE (CARDINAL) >= 4
   THEN
      Read (f, 012345678H)
   END ;
   Read (f, 0) ;
   Read (f, 1) ;
   Read (f, 2) ;
   Read (f, 3) ;
   Read (f, 1000) ;
   Read (f, 1024) ;
   Read (f, 32767) ;
   Close (f)
END CheckBinary ;


BEGIN
   CreateBinary ;
   CheckBinary
END testfiobinary.
