MODULE testdelete2 ;

(* A test module to test file creation and deletion using ISO
   libraries.  *)


IMPORT DynamicStrings, StringFileSysOp,
       FileSysOp, SeqFile, TextIO, Strings,
       IOChanUtils ;

FROM libc IMPORT printf, exit ;
FROM FormatStrings IMPORT Sprintf1 ;


CONST
   MaxFile = 10 ;

VAR
   files: ARRAY [0..MaxFile] OF SeqFile.ChanId ;


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
   res : SeqFile.OpenResults ;
BEGIN
   FOR i := 1 TO HIGH (files) DO
      Strings.Assign ('file', name) ;
      ch := CHR (ORD ('0')+i-1) ;
      name[4] := ch ;
      name[5] := 0C ;
      SeqFile.OpenWrite (files[i], name,
                         SeqFile.text+SeqFile.write, res) ;
      TextIO.WriteString (files[i], "some text inside file ") ;
      TextIO.WriteLn (files[i]) ;
      SeqFile.Close (files[i])
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
   res : SeqFile.OpenResults ;
BEGIN
   (* Open the files first.  *)
   FOR i := 1 TO HIGH (files) DO
      Strings.Assign ('file', name) ;
      ch := CHR (ORD ('0')+i-1) ;
      name[4] := ch ;
      name[5] := 0C ;
      SeqFile.OpenRead (files[i], name, SeqFile.text, res) ;
      Assert (FileSysOp.Exists (name), __LINE__) ;
      Assert (FileSysOp.IsFile (name), __LINE__)
   END ;
   (* Now delete them.  *)
   FOR i := 1 TO HIGH (files) DO
      s := IOChanUtils.GetFileName (files[i]) ;
      Assert (StringFileSysOp.Exists (s), __LINE__) ;
      Assert (StringFileSysOp.IsFile (s), __LINE__) ;
      Assert (StringFileSysOp.Unlink (s), __LINE__) ;
      Assert (NOT StringFileSysOp.Exists (s), __LINE__) ;
      SeqFile.Close (files[i]) ;
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
