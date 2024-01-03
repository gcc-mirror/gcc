(* Copyright (C) 2015-2024 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE mcPreprocess ;


FROM SYSTEM IMPORT ADDRESS ;

FROM DynamicStrings IMPORT string, InitString, Mark, KillString, EqualArray, InitStringCharStar,
                           Dup, ConCat, ConCatChar, RIndex, Slice ;

FROM libc IMPORT system, exit, unlink, printf ;
FROM alists IMPORT alist, initList, killList, includeItemIntoList, foreachItemInListDo ;
FROM M2RTS IMPORT InstallTerminationProcedure ;
FROM FIO IMPORT StdErr, StdOut ;
FROM mcPrintf IMPORT fprintf1 ;
FROM mcOptions IMPORT getVerbose, getCppCommandLine ;


VAR
   listOfFiles: alist ;


(*
   makeTempFile -
*)

PROCEDURE makeTempFile (ext: String) : String ;
BEGIN
   RETURN ConCat (InitString ('/tmp/mctemp.'), ext)
END makeTempFile ;


(*
   onExitDelete -
*)

PROCEDURE onExitDelete (filename: String) : String ;
BEGIN
   includeItemIntoList (listOfFiles, Dup (filename)) ;
   RETURN filename
END onExitDelete ;


(*
   removeFile - removes a single file, s.
*)

PROCEDURE removeFile (a: ADDRESS) ;
VAR
   s: String ;
BEGIN
   s := a ;
   IF unlink (string (s))#0
   THEN
   END
END removeFile ;


(*
   removeFiles -
*)

PROCEDURE removeFiles ;
BEGIN
   foreachItemInListDo (listOfFiles, removeFile)
END removeFiles ;


(*
   preprocessModule - preprocess a file, filename, returning the new filename
                      of the preprocessed file.
                      Preprocessing will only occur if requested by the user.
                      If no preprocessing was requested then filename is returned.
                      If preprocessing occurs then a temporary file is created
                      and its name is returned.
                      All temporary files will be deleted when the compiler exits.
*)

PROCEDURE preprocessModule (filename: String) : String ;
VAR
   tempfile,
   command,
   commandLine: String ;
   pos        : CARDINAL ;
BEGIN
   command := getCppCommandLine () ;
   IF EqualArray (command, '')
   THEN
      RETURN filename
   ELSE
      tempfile := InitStringCharStar (makeTempFile (InitString ('cpp'))) ;
      commandLine := Dup (command) ;
      commandLine := ConCat (ConCat (ConCat (ConCatChar (Dup (commandLine), ' '), filename),
                                     Mark (InitString(' -o '))),
                             tempfile) ;
      IF getVerbose ()
      THEN
         fprintf1 (StdOut, "%s\n", commandLine)
      END ;
      IF system (string (commandLine))#0
      THEN
         fprintf1(StdErr, 'C preprocessor failed when preprocessing %s\n', filename) ;
         exit(1)
      END ;
      commandLine := KillString (commandLine) ;
      RETURN onExitDelete (tempfile)
   END
END preprocessModule ;


BEGIN
   listOfFiles := initList () ;
   IF NOT InstallTerminationProcedure (removeFiles)
   THEN
      HALT
   END
END mcPreprocess.
