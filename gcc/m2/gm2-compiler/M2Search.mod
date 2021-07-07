(* M2Search.mod provides a mechanism to search selected directories.

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

IMPLEMENTATION MODULE M2Search ;


FROM SFIO IMPORT Exists ;
FROM M2FileName IMPORT CalculateFileName ;

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
   s := PopAllocationExemption(TRUE, s)
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
   PrependSearchPath - prepends a new path to the initial search path.
*)

PROCEDURE PrependSearchPath (path: String) ;
BEGIN
   DSdbEnter ;
   IF EqualArray(UserPath, '')
   THEN
      UserPath := KillString(UserPath) ;
      UserPath := Dup(path)
   ELSE
      UserPath := ConCat(ConCatChar(UserPath, ':'), path)
   END ;
   DSdbExit(UserPath)
END PrependSearchPath ;


(*
   FindSourceFile - attempts to locate the source file FileName.
                    If a file is found then TRUE is returned otherwise
                    FALSE is returned.
                    The parameter FullPath is set indicating the
                    absolute location of source FileName.
                    FullPath will be totally overwritten and should
                    not be initialized by InitString before this function
                    is called.
                    FullPath is set to NIL if this function returns FALSE.
                    FindSourceFile sets FullPath to a new string if successful.
                    The string, FileName, is not altered.
*)

PROCEDURE FindSourceFile (FileName: String;
                          VAR FullPath: String) : BOOLEAN ;
VAR
   CompleteSearchPath: String ;
   start, end        : INTEGER ;
   newpath           : String ;
BEGIN
   IF EqualArray(UserPath, '')
   THEN
      IF EqualArray(InitialPath, '')
      THEN
         CompleteSearchPath := InitString('.')
      ELSE
         CompleteSearchPath := Dup(InitialPath)
      END
   ELSE
      CompleteSearchPath := ConCat(ConCatChar(Dup(UserPath), ':'), InitialPath)
   END ;
   start := 0 ;
   end   := Index(CompleteSearchPath, ':', CARDINAL(start)) ;
   REPEAT
      IF end=-1
      THEN
         end := 0
      END ;
      newpath := Slice(CompleteSearchPath, start, end) ;
      IF EqualArray(newpath, '.')
      THEN
         newpath := KillString(newpath) ;
         newpath := Dup(FileName)
      ELSE
         newpath := ConCat(ConCatChar(newpath, Directory), FileName)
      END ;
      IF Exists(newpath)
      THEN
         FullPath := newpath ;
         CompleteSearchPath := KillString(CompleteSearchPath) ;
         RETURN( TRUE )
      END ;
      newpath := KillString(newpath) ;
      IF end#0
      THEN
         start := end+1 ;
         end   := Index(CompleteSearchPath, ':', CARDINAL(start))
      END
   UNTIL end=0 ;

   FullPath := NIL ;
   newpath := KillString(newpath) ;
   CompleteSearchPath :=  KillString(CompleteSearchPath) ;
   RETURN( FALSE )
END FindSourceFile ;


(*
   FindSourceDefFile - attempts to find the definition module for
                       a module, Stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and FullPath is set to NIL.
*)

PROCEDURE FindSourceDefFile (Stem: String; VAR FullPath: String) : BOOLEAN ;
VAR
   f: String ;
BEGIN
   IF Def#NIL
   THEN
      f := CalculateFileName(Stem, Def) ;
      IF FindSourceFile(f, FullPath)
      THEN
         RETURN( TRUE )
      END ;
      f := KillString(f)
   END ;
   (* and try the GNU Modula-2 default extension *)
   f := CalculateFileName(Stem, Mark(InitString('def'))) ;
   RETURN( FindSourceFile(f, FullPath) )
END FindSourceDefFile ;


(*
   FindSourceModFile - attempts to find the implementation module for
                       a module, Stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and FullPath is set to NIL.
*)

PROCEDURE FindSourceModFile (Stem: String; VAR FullPath: String) : BOOLEAN ;
VAR
   f: String ;
BEGIN
   IF Mod#NIL
   THEN
      f := CalculateFileName(Stem, Mod) ;
      IF FindSourceFile(f, FullPath)
      THEN
         RETURN( TRUE )
      END ;
      f := KillString(f)
   END ;
   (* and try the GNU Modula-2 default extension *)
   f := CalculateFileName(Stem, Mark(InitString('mod'))) ;
   RETURN( FindSourceFile(f, FullPath) )
END FindSourceModFile ;


(*
   SetDefExtension - sets the default extension for definition modules to, ext.
                     The string, ext, should be deallocated by the caller at
                     an appropriate time.
*)

PROCEDURE SetDefExtension (ext: String) ;
BEGIN
   Def := KillString(Def) ;
   Def := Dup(ext)
END SetDefExtension ;


(*
   SetModExtension - sets the default extension for implementation and program
                     modules to, ext. The string, ext, should be deallocated
                     by the caller at an appropriate time.
*)

PROCEDURE SetModExtension (ext: String) ;
BEGIN
   Mod := KillString(Mod) ;
   Mod := Dup(ext)
END SetModExtension ;


(*
   InitSearchPath - assigns the search path to Path.
                    The string Path may take the form:

                    Path           ::= IndividualPath { ":" IndividualPath }
                    IndividualPath ::= "." | DirectoryPath
                    DirectoryPath  ::= [ "/" ] Name { "/" Name }
                    Name           ::= Letter { (Letter | Number) }
                    Letter         ::= A..Z | a..z
                    Number         ::= 0..9
*)

PROCEDURE InitSearchPath (Path: String) ;
BEGIN
   IF InitialPath#NIL
   THEN
      InitialPath := KillString(InitialPath)
   END ;
   InitialPath := Path
END InitSearchPath ;


(*
   Init - initializes the search path.
*)

PROCEDURE Init ;
BEGIN
   UserPath    := InitString('') ;
   InitialPath := InitStringChar('.') ;
   Def := NIL ;
   Mod := NIL
END Init ;


BEGIN
   Init
END M2Search.
