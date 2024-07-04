MODULE testchar ;

FROM FIO IMPORT File, OpenToWrite, OpenToRead,
                Close, WriteChar, ReadChar, IsNoError ;

FROM libc IMPORT printf, exit ;


(*
   createFile -
*)

PROCEDURE createFile ;
VAR
   fo: File ;
   ch: CHAR ;
BEGIN
   fo := OpenToWrite ("test.txt") ;
   FOR ch := MIN (CHAR) TO MAX (CHAR) DO
      WriteChar (fo, ch) ;
      IF NOT IsNoError (fo)
      THEN
         printf ("failure to write: %c\n", ch);
         exit (1)
      END
   END ;
   Close (fo)
END createFile ;


(*
   readFile -
*)

PROCEDURE readFile ;
VAR
   fi    : File ;
   ch, in: CHAR ;
BEGIN
   fi := OpenToRead ("test.txt") ;
   FOR ch := MIN (CHAR) TO MAX (CHAR) DO
      in := ReadChar (fi) ;
      IF NOT IsNoError (fi)
      THEN
         printf ("failure to read: %c\n", ch);
         exit (1)
      END ;
      IF ch # in
      THEN
         printf ("failure to verify: %c\n", ch);
         exit (1)
      END
   END ;
   Close (fi)
END readFile ;


(*
   init -
*)

PROCEDURE init ;
BEGIN
   createFile ;
   readFile
END init ;


BEGIN
   init
END testchar.
