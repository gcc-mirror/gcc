(* Copyright (C) 2015-2025 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE mcSearch ;


FROM SFIO IMPORT Exists ;
FROM mcFileName IMPORT calculateFileName ;

FROM DynamicStrings IMPORT InitString, InitStringChar,
                           KillString, ConCat, ConCatChar, Index, Slice,
                           Add, EqualArray, Dup, Mark,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;


CONST
   Directory    =   '/' ;

VAR
   Def, Mod,
   UserPath,
   InitialPath: String ;

(*
#define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#define Dup(X) DupDB(X, __FILE__, __LINE__)
#define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
*)


(*
   doDSdbEnter -
*)

PROCEDURE doDSdbEnter ;
BEGIN
   PushAllocation
END doDSdbEnter ;


(*
   doDSdbExit -
*)

PROCEDURE doDSdbExit (s: String) ;
BEGIN
   s := PopAllocationExemption (TRUE, s)
END doDSdbExit ;


(*
   DSdbEnter -
*)

PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;


(*
   DSdbExit -
*)

PROCEDURE DSdbExit (s: String) ;
BEGIN
END DSdbExit ;


(*
#define DSdbEnter doDSdbEnter
#define DSdbExit  doDSdbExit
*)


(*
   prependSearchPath - prepends a new path to the initial search path.
*)

PROCEDURE prependSearchPath (path: String) ;
BEGIN
   DSdbEnter ;
   IF EqualArray (UserPath, '')
   THEN
      UserPath := KillString (UserPath) ;
      UserPath := Dup (path)
   ELSE
      UserPath := ConCat (ConCatChar (UserPath, ':'), path)
   END ;
   DSdbExit (UserPath)
END prependSearchPath ;


(*
   findSourceFile - attempts to locate the source file FileName.
                    If a file is found then TRUE is returned otherwise
                    FALSE is returned.
                    The parameter fullPath is set indicating the
                    absolute location of source FileName.
                    fullPath will be totally overwritten and should
                    not be initialized by InitString before this function
                    is called.
                    fullPath is set to NIL if this function returns FALSE.
                    findSourceFile sets fullPath to a new string if successful.
                    The string, FileName, is not altered.
*)

PROCEDURE findSourceFile (FileName: String;
                          VAR fullPath: String) : BOOLEAN ;
VAR
   completeSearchPath: String ;
   start, end        : INTEGER ;
   newpath           : String ;
BEGIN
   IF EqualArray (UserPath, '')
   THEN
      IF EqualArray (InitialPath, '')
      THEN
         completeSearchPath := InitString ('.')
      ELSE
         completeSearchPath := Dup (InitialPath)
      END
   ELSE
      completeSearchPath := ConCat (ConCatChar (Dup (UserPath), ':'), InitialPath)
   END ;
   start := 0 ;
   end   := Index (completeSearchPath, ':', CARDINAL (start)) ;
   REPEAT
      IF end=-1
      THEN
         end := 0
      END ;
      newpath := Slice (completeSearchPath, start, end) ;
      IF EqualArray (newpath, '.')
      THEN
         newpath := KillString (newpath) ;
         newpath := Dup (FileName)
      ELSE
         newpath := ConCat (ConCatChar (newpath, Directory), FileName)
      END ;
      IF Exists (newpath)
      THEN
         fullPath := newpath ;
         completeSearchPath := KillString (completeSearchPath) ;
         RETURN TRUE
      END ;
      newpath := KillString (newpath) ;
      IF end#0
      THEN
         start := end+1 ;
         end   := Index (completeSearchPath, ':', CARDINAL (start))
      END
   UNTIL end=0 ;

   fullPath := NIL ;
   newpath := KillString (newpath) ;
   completeSearchPath :=  KillString (completeSearchPath) ;
   RETURN FALSE
END findSourceFile ;


(*
   findSourceDefFile - attempts to find the definition module for
                       a module, stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and fullPath is set to NIL.
*)

PROCEDURE findSourceDefFile (stem: String; VAR fullPath: String) : BOOLEAN ;
VAR
   f: String ;
BEGIN
   IF Def#NIL
   THEN
      f := calculateFileName (stem, Def) ;
      IF findSourceFile (f, fullPath)
      THEN
         RETURN TRUE
      END ;
      f := KillString (f)
   END ;
   (* and try the GNU Modula-2 default extension *)
   f := calculateFileName (stem, Mark (InitString ('def'))) ;
   RETURN findSourceFile (f, fullPath)
END findSourceDefFile ;


(*
   findSourceModFile - attempts to find the implementation module for
                       a module, stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and fullPath is set to NIL.
*)

PROCEDURE findSourceModFile (stem: String; VAR fullPath: String) : BOOLEAN ;
VAR
   f: String ;
BEGIN
   IF Mod#NIL
   THEN
      f := calculateFileName (stem, Mod) ;
      IF findSourceFile (f, fullPath)
      THEN
         RETURN TRUE
      END ;
      f := KillString (f)
   END ;
   (* and try the GNU Modula-2 default extension *)
   f := calculateFileName (stem, Mark (InitString ('mod'))) ;
   RETURN findSourceFile (f, fullPath)
END findSourceModFile ;


(*
   setDefExtension - sets the default extension for definition modules to, ext.
                     The string, ext, should be deallocated by the caller at
                     an appropriate time.
*)

PROCEDURE setDefExtension (ext: String) ;
BEGIN
   Def := KillString (Def) ;
   Def := Dup (ext)
END setDefExtension ;


(*
   setModExtension - sets the default extension for implementation and program
                     modules to, ext. The string, ext, should be deallocated
                     by the caller at an appropriate time.
*)

PROCEDURE setModExtension (ext: String) ;
BEGIN
   Mod := KillString (Mod) ;
   Mod := Dup (ext)
END setModExtension ;


(*
   initSearchPath - assigns the search path to Path.
                    The string Path may take the form:

                    Path           ::= IndividualPath { ":" IndividualPath }
                    IndividualPath ::= "." | DirectoryPath
                    DirectoryPath  ::= [ "/" ] Name { "/" Name }
                    Name           ::= Letter { (Letter | Number) }
                    Letter         ::= A..Z | a..z
                    Number         ::= 0..9
*)

PROCEDURE initSearchPath (path: String) ;
BEGIN
   IF InitialPath#NIL
   THEN
      InitialPath := KillString (InitialPath)
   END ;
   InitialPath := path
END initSearchPath ;


(*
   Init - initializes the search path.
*)

PROCEDURE Init ;
BEGIN
   UserPath    := InitString ('') ;
   InitialPath := InitStringChar ('.') ;
   Def := NIL ;
   Mod := NIL
END Init ;


BEGIN
   Init
END mcSearch.
