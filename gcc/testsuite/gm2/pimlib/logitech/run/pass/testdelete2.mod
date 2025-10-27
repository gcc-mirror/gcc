MODULE testdelete2 ;

(* A test module to test file creation and deletion using log
   libraries.  *)


IMPORT FIO, SFIO, DynamicStrings, StringFileSysOp,
       FileSysOp, FileSystem, StrLib ;

FROM libc IMPORT printf, exit ;
FROM FormatStrings IMPORT Sprintf1 ;


CONST
   MaxFile = 10 ;

VAR
   files: ARRAY [0..MaxFile] OF FileSystem.File ;


PROCEDURE Assert (condition: BOOLEAN; line: CARDINAL) ;
BEGIN
   IF NOT condition
   THEN
      printf ("%s:%d: assert failed\n", __FILE__, line) ;
      exit (1)
   END
END Assert ;


(*
   CreateFiles - create MaxFile files saving the file handle
                 into files.
*)

PROCEDURE CreateFiles ;
VAR
   i   : CARDINAL ;
   name: ARRAY [0..10] OF CHAR ;
   ch  : CHAR ;
BEGIN
   FOR i := 1 TO HIGH (files) DO
      StrLib.StrCopy ('file', name) ;
      ch := CHR (ORD ('0')+i-1) ;
      name[4] := ch ;
      name[5] := 0C ;
      FileSystem.Lookup (files[i], name, TRUE) ;
      FileSystem.WriteString (files[i], "some text inside file ") ;
      FileSystem.WriteChar (files[i], ch) ;
      FileSystem.WriteString (files[i], "\n") ;
      FileSystem.Close (files[i])
   END
END CreateFiles ;


(*
   DeleteFiles - delete every file in files.
*)

PROCEDURE DeleteFiles ;
VAR
   i   : CARDINAL ;
   name: ARRAY [0..10] OF CHAR ;
   s   : DynamicStrings.String ;
   ch  : CHAR ;
BEGIN
   (* Open the files first.  *)
   FOR i := 1 TO HIGH (files) DO
      StrLib.StrCopy ('file', name) ;
      ch := CHR (ORD ('0')+i-1) ;
      name[4] := ch ;
      name[5] := 0C ;
      FileSystem.Lookup (files[i], name, FALSE) ;
      Assert (FileSysOp.Exists (name), __LINE__) ;
      Assert (FileSysOp.IsFile (name), __LINE__)
   END ;
   (* Now delete them.  *)
   FOR i := 1 TO HIGH (files) DO
      s := FileSystem.GetFileName (files[i]) ;
      Assert (StringFileSysOp.Exists (s), __LINE__) ;
      Assert (StringFileSysOp.IsFile (s), __LINE__) ;
      Assert (StringFileSysOp.Unlink (s), __LINE__) ;
      Assert (NOT StringFileSysOp.Exists (s), __LINE__) ;
      FileSystem.Close (files[i]) ;
      s := DynamicStrings.KillString (s)
   END
END DeleteFiles ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   CreateFiles ;
   DeleteFiles ;
   printf ("all tests passed\n")
END Init ;


BEGIN
   Init
END testdelete2.
