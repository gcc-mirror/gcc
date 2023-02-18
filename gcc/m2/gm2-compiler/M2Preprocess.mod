(* M2Preprocess.mod provides a mechanism to invoke the C preprocessor.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
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
                           Dup, ConCat, ConCatChar, RIndex, Slice, Length ;

FROM choosetemp IMPORT make_temp_file ;
FROM pexecute IMPORT pexecute ;
FROM libc IMPORT system, exit, unlink, printf, atexit ;
FROM Lists IMPORT List, InitList, KillList, IncludeItemIntoList, ForeachItemInListDo ;
FROM FIO IMPORT StdErr, StdOut ;
FROM M2Printf IMPORT fprintf1 ;
FROM M2Options IMPORT Verbose, PPonly, GetObj, GetMD, GetMMD, GetMQ,
                      CppCommandLine, SaveTemps, GetSaveTempsDir, GetDumpDir ;
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
   Return the filename with no path.
*)

PROCEDURE GetFileName (Path: String) : String ;
VAR
   fstart: INTEGER ;
BEGIN
   fstart := RIndex(Path, '/', 0) ;
   IF fstart=-1
   THEN
      fstart := 0
   ELSE
      fstart := fstart + 1
   END ;
   RETURN Dup (Slice(Path, fstart, Length (Path)))
END GetFileName ;


(*
   Return basename.
*)

(*
PROCEDURE BaseName (Path: String) : String ;
VAR
   ext,
   basename: INTEGER ;
BEGIN
   basename := RIndex(Path, '/', 0) ;
   IF basename=-1
   THEN
      basename := 0
   ELSE
      basename := basename + 1
   END ;
   ext := RIndex(Path, '.', 0) ;
   IF ext=-1
   THEN
      ext := 0
   END ;
   RETURN Dup (Slice(Path, basename, ext))
END BaseName ;
*)

(*
   MakeSaveTempsFileName - return a temporary file like 
   "./filename.{def,mod}.m2i" in the CWD unless SaveTempsDir = obj,
   when we put it in the dumpdir if that is specified (or fallback to '.'
   if not).
   We have to keep the original extension because that disambiguates .def
   and .mod files (otherwise, we'd need two 'preprocessed' extensions).
*)

PROCEDURE MakeSaveTempsFileName (filename: String) : String ;
VAR
   NewName,
   DumpDir,
   NewDir : String ;
BEGIN
   NewName := ConCat (GetFileName (filename), InitString ('.m2i')) ;
   NewDir := GetSaveTempsDir () ;
   DumpDir := GetDumpDir () ;
(*   IF Verbose
   THEN
      fprintf1 (StdOut, "newname: %s", NewName) ;
      fprintf1 (StdOut, " NewDir: %s", NewDir) ;
      fprintf1 (StdOut, " DumpDir: %s\n", DumpDir)
   END ;
*)
   IF (NewDir#NIL) AND EqualArray (NewDir, 'obj') AND (DumpDir#NIL)
   THEN
      RETURN Dup (ConCat (DumpDir, NewName))
   ELSE
      RETURN Dup (ConCat (InitString ('./'), NewName))
   END ;
END MakeSaveTempsFileName ;


(*
   PreprocessModule - preprocess a file, filename, returning the new filename
                      of the preprocessed file.
                      Preprocessing will only occur if requested by the user.
                      If no preprocessing was requested then filename is returned.
                      If preprocessing occurs then a temporary file is created
                      and its name is returned.
                      All temporary files will be deleted when the compiler exits.
*)

PROCEDURE PreprocessModule (filename: String; isMain: BOOLEAN) : String ;
VAR
   tempfile,
   command,
   commandLine: String ;
BEGIN
   command := CppCommandLine () ;
   IF (command = NIL) OR EqualArray (command, '')
   THEN
      RETURN Dup (filename)
   ELSE
      commandLine := Dup (command) ;
      tempfile := NIL ;
      (* We support MD and MMD for the main file only, at present.  *)
      IF isMain OR PPonly
      THEN
         IF GetMD () # NIL
         THEN
            tempfile := ConCat( Mark (InitString(' -MD ')),
                                InitStringCharStar (GetMD ()))
         ELSIF GetMMD () # NIL
         THEN
            tempfile := ConCat( Mark (InitString(' -MMD ')),
                                InitStringCharStar (GetMMD ()))
         END ;
         IF tempfile#NIL
         THEN
            commandLine := ConCat (Dup (commandLine), Dup (tempfile)) ;
            (* We can only add MQ if we already have an MD/MMD.  *)
            IF GetMQ () # NIL
            THEN
               tempfile := ConCat( Mark (InitString(' -MQ ')),
                                 InitStringCharStar (GetMQ ())) ;
               commandLine := ConCat (Dup (commandLine), Dup (tempfile))
            END ;
         END ;
      END ;
      (* The output file depends on whether we are in stand-alone PP mode, and
         if an output file is specified.  *)
      tempfile := NIL ;
      IF PPonly
      THEN
         IF GetObj () # NIL
         THEN
           tempfile := InitStringCharStar (GetObj ())
         END ;
      ELSIF SaveTemps
      THEN
         tempfile := MakeSaveTempsFileName (filename)
      ELSE
         tempfile := InitStringCharStar (make_temp_file (KeyToCharStar (MakeKey('.m2i'))))
      END ;
      commandLine := ConCat (ConCatChar (Dup (commandLine), ' '), filename) ;
      IF tempfile # NIL
      THEN
         commandLine := ConCat (ConCat (Dup (commandLine),
                                        Mark (InitString(' -o '))), tempfile) ;
      END ;
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
      IF SaveTemps
      THEN
         RETURN tempfile
      ELSE
         RETURN OnExitDelete (tempfile)
      END
   END
END PreprocessModule ;


BEGIN
   InitList (ListOfFiles) ;
   IF atexit (RemoveFiles) # 0
   THEN
      HALT
   END
END M2Preprocess.
