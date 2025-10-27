MODULE testwrite ;

IMPORT ARRAYOFCHAR ;
FROM FIO IMPORT StdOut ;

BEGIN
   ARRAYOFCHAR.Write (StdOut, "hello world") ; ARRAYOFCHAR.WriteLn (StdOut)
END testwrite.
