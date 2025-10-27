MODULE testdelete ;

(* A test module to test file creation and deletion using base
   PIM libraries.  *)


IMPORT FIO, SFIO, DynamicStrings, StringFileSysOp ;
FROM libc IMPORT printf, exit ;
FROM FormatStrings IMPORT Sprintf1 ;


CONST
   MaxFile = 10 ;

VAR
   files: ARRAY [0..MaxFile] OF FIO.File ;


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
   i: CARDINAL ;
   s: DynamicStrings.String ;
BEGIN
   FOR i := 1 TO HIGH (files) DO
      s := DynamicStrings.InitString ("file%03d") ;
      s := Sprintf1 (s, i) ;
      files[i] := SFIO.OpenToWrite (s) ;
      s := DynamicStrings.KillString (s) ;
      s := DynamicStrings.InitString ("some text inside file %d\n") ;
      s := Sprintf1 (s, i) ;
      s := DynamicStrings.KillString (SFIO.WriteS (files[i], s)) ;
      FIO.Close (files[i])
   END
END CreateFiles ;


(*
   DeleteFiles - delete every file in files.
*)

PROCEDURE DeleteFiles ;
VAR
   i: CARDINAL ;
   s: DynamicStrings.String ;
BEGIN
   (* Open the files first.  *)
   FOR i := 1 TO HIGH (files) DO
      s := DynamicStrings.InitString ("file%03d") ;
      s := Sprintf1 (s, i) ;
      files[i] := SFIO.OpenToRead (s) ;
      Assert (StringFileSysOp.Exists (s), __LINE__) ;
      Assert (StringFileSysOp.IsFile (s), __LINE__) ;
      s := DynamicStrings.KillString (s)
   END ;
   (* Now delete them.  *)
   FOR i := 1 TO HIGH (files) DO
      s := SFIO.GetFileName (files[i]) ;
      Assert (StringFileSysOp.Exists (s), __LINE__) ;
      Assert (StringFileSysOp.IsFile (s), __LINE__) ;
      Assert (StringFileSysOp.Unlink (s), __LINE__) ;
      Assert (NOT StringFileSysOp.Exists (s), __LINE__) ;
      FIO.Close (files[i]) ;
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
END testdelete.
