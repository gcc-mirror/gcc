(* M2Preprocess.mod provides a mechanism to invoke the C preprocessor.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2Preprocess ;


FROM SYSTEM IMPORT WORD ;

FROM DynamicStrings IMPORT string, InitString, Mark, KillString, EqualArray, InitStringCharStar,
                           Dup, ConCat, ConCatChar, RIndex, Slice ;

FROM choosetemp IMPORT make_temp_file ;
FROM pexecute IMPORT pexecute ;
FROM libc IMPORT system, exit, unlink, printf, atexit ;
FROM Lists IMPORT List, InitList, KillList, IncludeItemIntoList, ForeachItemInListDo ;
FROM FIO IMPORT StdErr, StdOut ;
FROM M2Printf IMPORT fprintf1 ;
FROM M2Options IMPORT Verbose, CppCommandLine ;
FROM NameKey IMPORT Name, MakeKey, KeyToCharStar, makekey ;


VAR
   ListOfFiles: List ;


(*
   OnExitDelete -
*)

PROCEDURE OnExitDelete (filename: String) : String ;
BEGIN
   IncludeItemIntoList (ListOfFiles, makekey (filename)) ;
   RETURN filename
END OnExitDelete ;


(*
   RemoveFile - removes a single file, s.
*)

PROCEDURE RemoveFile (w: WORD) ;
VAR
   n: Name ;
BEGIN
   n := w ;
   IF unlink (KeyToCharStar (n)) # 0
   THEN
   END
END RemoveFile ;


(*
   RemoveFiles -
*)

PROCEDURE RemoveFiles () : INTEGER ;
BEGIN
   ForeachItemInListDo (ListOfFiles, RemoveFile) ;
   RETURN 0
END RemoveFiles ;


(*
   PreprocessModule - preprocess a file, filename, returning the new filename
                      of the preprocessed file.
                      Preprocessing will only occur if requested by the user.
                      If no preprocessing was requested then filename is returned.
                      If preprocessing occurs then a temporary file is created
                      and its name is returned.
                      All temporary files will be deleted when the compiler exits.
*)

PROCEDURE PreprocessModule (filename: String) : String ;
VAR
   tempfile,
   command,
   commandLine: String ;
BEGIN
   command := CppCommandLine () ;
   IF EqualArray (command, '')
   THEN
      RETURN filename
   ELSE
      tempfile := InitStringCharStar (make_temp_file (KeyToCharStar (MakeKey('cpp')))) ;
      commandLine := Dup (command) ;
      commandLine := ConCat (ConCat (ConCat (ConCatChar (Dup (commandLine), ' '), filename),
                                     Mark (InitString(' -o '))),
                             tempfile) ;
(*  use pexecute in the future
      res := pexecute(string(Slice(commandLine, 0, Index(commandLine, ' ', 0))), etc etc );
*)
      (* for now we'll use system *)
      IF Verbose
      THEN
         fprintf1 (StdOut, "preprocess: %s\n", commandLine)
      END ;
      IF system (string (commandLine)) # 0
      THEN
         fprintf1 (StdErr, 'C preprocessor failed when preprocessing %s\n', filename) ;
         exit (1)
      END ;
      commandLine := KillString (commandLine) ;
      RETURN OnExitDelete (tempfile)
   END
END PreprocessModule ;


BEGIN
   InitList (ListOfFiles) ;
   IF atexit (RemoveFiles) # 0
   THEN
      HALT
   END
END M2Preprocess.
