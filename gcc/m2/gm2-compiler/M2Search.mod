(* M2Search.mod provides a mechanism to search selected directories.

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

IMPLEMENTATION MODULE M2Search ;


FROM M2FileName IMPORT CalculateFileName ;
FROM Assertion IMPORT Assert ;
FROM PathName IMPORT FindNamedPathFile ;

FROM DynamicStrings IMPORT InitString, InitStringChar,
                           KillString, ConCat, ConCatChar, Index, Slice,
                           Add, EqualArray, Dup, Mark,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;


CONST
   GarbageDebugging = FALSE ;
   DefaultDefExt    = '.def' ;
   DefaultModExt    = '.mod' ;

VAR
   Def, Mod: String ;

(* Internal garbage collection debugging routines.  *)

(*
#define InitString(X) InitStringDB(X, __FILE__, __LINE__)
#define InitStringCharStar(X) InitStringCharStarDB(X, __FILE__, __LINE__)
#define InitStringChar(X) InitStringCharDB(X, __FILE__, __LINE__)
#define Mult(X,Y) MultDB(X, Y, __FILE__, __LINE__)
#define Dup(X) DupDB(X, __FILE__, __LINE__)
#define Slice(X,Y,Z) SliceDB(X, Y, Z, __FILE__, __LINE__)
*)


(*
   doDSdbEnter - called when compiled with -fcpp to enable runtime garbage
                 collection debugging.
*)

(*
PROCEDURE doDSdbEnter ;
BEGIN
   PushAllocation
END doDSdbEnter ;
*)


(*
   doDSdbExit - called when compiled with -fcpp to enable runtime garbage
                collection debugging.  The parameter string s is exempt from
                garbage collection analysis.
*)

(*
PROCEDURE doDSdbExit (s: String) ;
BEGIN
   (* Check to see whether no strings have been lost since the PushAllocation.  *)
   Assert (PopAllocationExemption (TRUE, s) = s)
END doDSdbExit ;
*)


(*
   DSdbEnter - dummy nop entry code which the preprocessor replaces by
               doDSsbEnter when debugging garbage collection at runtime.
*)

(*
PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;
*)

(*
   DSdbExit - dummy nop exit code which the preprocessor replaces by
              doDSsbExit when debugging garbage collection at runtime.
*)

(*
PROCEDURE DSdbExit (s: String) ;
BEGIN
   IF GarbageDebugging
   THEN
      Assert (s # NIL)
   END
END DSdbExit ;
*)


(*
#define DSdbEnter doDSdbEnter
#define DSdbExit  doDSdbExit
*)


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
                    The string FileName is not altered.
*)

PROCEDURE FindSourceFile (FileName: String;
                          VAR FullPath, named: String) : BOOLEAN ;
BEGIN
   FullPath := FindNamedPathFile (FileName, named) ;
   RETURN FullPath # NIL
END FindSourceFile ;


(*
   FindSourceDefFile - attempts to find the definition module for
                       a module, Stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and FullPath is set to NIL.
*)

PROCEDURE FindSourceDefFile (Stem: String; VAR FullPath, named: String) : BOOLEAN ;
VAR
   f: String ;
BEGIN
   IF Def # NIL
   THEN
      f := CalculateFileName (Stem, Def) ;
      IF FindSourceFile (f, FullPath, named)
      THEN
         RETURN TRUE
      END ;
      f := KillString (f)
   END ;
   (* Try the GNU Modula-2 default extension.  *)
   f := CalculateFileName (Stem, Mark (InitString (DefaultDefExt))) ;
   RETURN FindSourceFile (f, FullPath, named)
END FindSourceDefFile ;


(*
   FindSourceModFile - attempts to find the implementation module for
                       a module, Stem. If successful it returns
                       the full path and returns TRUE. If unsuccessful
                       then FALSE is returned and FullPath is set to NIL.
*)

PROCEDURE FindSourceModFile (Stem: String; VAR FullPath, named: String) : BOOLEAN ;
VAR
   f: String ;
BEGIN
   IF Mod#NIL
   THEN
      f := CalculateFileName (Stem, Mod) ;
      IF FindSourceFile (f, FullPath, named)
      THEN
         RETURN TRUE
      END ;
      f := KillString (f)
   END ;
   (* Try the GNU Modula-2 default extension.  *)
   f := CalculateFileName (Stem, Mark (InitString (DefaultModExt))) ;
   RETURN FindSourceFile (f, FullPath, named)
END FindSourceModFile ;


(*
   SetDefExtension - sets the default extension for definition modules to, ext.
                     The string, ext, should be deallocated by the caller at
                     an appropriate time.
*)

PROCEDURE SetDefExtension (ext: String) ;
BEGIN
   Def := KillString (Def) ;
   Def := Dup (ext)
END SetDefExtension ;


(*
   SetModExtension - sets the default extension for implementation and program
                     modules to, ext. The string, ext, should be deallocated
                     by the caller at an appropriate time.
*)

PROCEDURE SetModExtension (ext: String) ;
BEGIN
   Mod := KillString (Mod) ;
   Mod := Dup (ext)
END SetModExtension ;


(*
   Init - initializes the def and mod default string names to NIL.
*)

PROCEDURE Init ;
BEGIN
   Def := NIL ;
   Mod := NIL
END Init ;


BEGIN
   Init
END M2Search.
