MODULE testreadint ;

FROM ChanConsts IMPORT OpenResults, old, read, write ;
FROM IOChan IMPORT ChanId ;
FROM StdChans IMPORT StdOutChan ;
IMPORT StreamFile ;
FROM TextIO IMPORT SkipLine, WriteLn, WriteString ;
FROM WholeIO IMPORT ReadCard, ReadInt, WriteCard, WriteInt ;
FROM libc IMPORT printf, exit ;


CONST
   TestFileName = "testdata" ;


PROCEDURE Assert (condition: BOOLEAN; name, result: ARRAY OF CHAR) ;
BEGIN
   IF NOT condition
   THEN
      code := 1 ;
      printf ("assert failed, procedure: %s failed to read number: %s\n", name, result)
   END
END Assert ;


PROCEDURE StressReadInt ;
VAR
   in,
   out   : ChanId ;
   result: OpenResults ;
   int   : INTEGER ;
   card  : CARDINAL ;
BEGIN
   (* Create a new file and use WriteCard to populate the file.  *)
   printf ("creating test file: %s\n", TestFileName) ;

   StreamFile.Open (out, TestFileName, write+old, result);
   IF result = opened
   THEN
      WriteString (out, ' ') ;
      WriteCard (out, 123, 3) ;
      WriteLn (out) ;
      WriteCard (out, 456, 3) ;
      WriteLn (out) ;
      StreamFile.Close (out)
   ELSE
      printf ("unable to create: %s\n", TestFileName) ;
      exit (1)
   END ;

   (* Now attempt to read the data using ReadCard.  *)
   printf ("reading test file using ReadCard: %s\n", TestFileName) ;
   StreamFile.Open (in, TestFileName, read, result) ;
   IF result = opened
   THEN
      ReadCard (in, card) ;
      printf ("first cardinal: %d\n", card) ;
      Assert (card = 123, "ReadCard", "123") ;
      SkipLine (in) ;
      ReadCard (in, card) ;
      printf ("second cardinal: %d\n", card) ;
      Assert (card = 456, "ReadCard", "456") ;
      StreamFile.Close (in)
   END ;

   (* Now attempt to read the data using ReadInt.  *)
   printf ("reading test file using ReadInt: %s\n", TestFileName) ;
   StreamFile.Open (in, TestFileName, read, result) ;
   IF result = opened
   THEN
      ReadInt (in, int) ;
      printf ("first integer: %d\n", int) ;
      Assert (int = 123, "ReadInt", "123") ;
      SkipLine (in) ;
      ReadInt (in, int) ;
      printf ("second integer: %d\n", int) ;
      Assert (int = 456, "ReadInt", "456") ;
      StreamFile.Close (in)
   END
END StressReadInt ;


VAR
   code: INTEGER ;
BEGIN
   code := 0 ;
   StressReadInt ;
   exit (code)
END testreadint.
