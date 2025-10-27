MODULE testwritechar ;

IMPORT CHAR ;
FROM FIO IMPORT StdOut ;

BEGIN
   CHAR.Write (StdOut, 'h') ;
   CHAR.Write (StdOut, 'e') ;
   CHAR.Write (StdOut, 'l') ;
   CHAR.Write (StdOut, 'l') ;
   CHAR.Write (StdOut, 'o') ;
   CHAR.WriteLn (StdOut)
END testwritechar.
