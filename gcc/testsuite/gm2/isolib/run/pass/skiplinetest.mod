MODULE skiplinetest ;

FROM ChanConsts IMPORT OpenResults, old, read, write;
FROM IOChan IMPORT ChanId;
FROM StdChans IMPORT StdOutChan ;
IMPORT StreamFile;
FROM TextIO IMPORT ReadString, SkipLine, WriteLn, WriteString;
FROM StrLib IMPORT StrEqual ;
FROM libc IMPORT exit, printf ;


PROCEDURE StressSkip ;
VAR
   stdout,
   cid     : ChanId;
   filename: ARRAY [0..20] OF CHAR ;
   str     : ARRAY [0..79] OF CHAR ;
   result  : OpenResults;
BEGIN
   stdout := StdOutChan();
   filename := 'testdata';

   StreamFile.Open (cid, filename, write+old, result) ;
   IF result = opened
   THEN
      WriteString (cid, '# line1');
      WriteLn (cid);
      WriteString (cid, '# line2');
      WriteLn (cid) ;
      StreamFile.Close (cid);
   END ;

   StreamFile.Open (cid, filename, read, result) ;
   IF result = opened
   THEN
      SkipLine (cid);
      ReadString (cid, str);
      IF NOT StrEqual (str, '# line2')
      THEN
         printf ("ReadString failed, read %s\n", str) ;
         exit (1)
      END ;
      WriteString (stdout, str);
      WriteLn (stdout) ;
      StreamFile.Close (cid)
   END
END StressSkip ;


BEGIN
   StressSkip
END skiplinetest.