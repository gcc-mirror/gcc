MODULE seqappend ;

FROM ChanConsts IMPORT OpenResults, old, raw, write, read ;
FROM IOChan IMPORT ChanId, RawWrite, RawRead ;
FROM SYSTEM IMPORT ADR;
FROM libc IMPORT exit, printf ;
FROM StrLib IMPORT StrEqual ;
IMPORT StreamFile;
IMPORT SeqFile;


PROCEDURE stress ;
VAR
   cid   : ChanId ;
   res   : OpenResults;
   str   : ARRAY [0..20] OF CHAR ;
   actual: CARDINAL ;
   code  : INTEGER ;
BEGIN
   code := 0 ;
   str := '0123456789' ;

   (* Open file and create data.  *)
   StreamFile.Open (cid, 'testdata', write+old+raw, res) ;
   IF res = opened
   THEN
      (* Now write data creating new contents.  *)
      RawWrite (cid, ADR (str), 10) ;
      StreamFile.Close(cid)
   ELSE
      printf ("failed to open file for write\n") ;
      code := 2
   END ;

   str := 'abcdefghij' ;
   (* Now attempt to append the alphabetic str.  *)
   SeqFile.OpenAppend (cid, 'testdata', write+old+raw, res) ;
   IF res = opened
   THEN
      RawWrite (cid, ADR (str), 10);
      SeqFile.Close (cid)
   ELSE
      printf ("failed to open file for append\n") ;
      code := 3
   END ;

   (* And now test the file for the appended data.  *)
   StreamFile.Open (cid, 'testdata', read+raw, res) ;
   IF res = opened
   THEN
      (* Now check the new contents for the appended data.  *)
      RawRead (cid, ADR (str), 20, actual) ;
      IF actual # 20
      THEN
         printf ("short read occurred: %d...\n", actual) ;
         code := 5
      END ;
      StreamFile.Close (cid) ;
      str[20] := 0C ;
      IF StrEqual (str, '0123456789abcdefghij')
      THEN
         printf ("append test passed\n")
      ELSE
         printf ("append test failed\n") ;
         code := 1
      END
   ELSE
      printf ("failed to open result file\n") ;
      code := 4
   END ;
   exit (code)
END stress ;


BEGIN
   stress
END seqappend.
