(* M2Scaffold.mod declare and create scaffold entities.

Copyright (C) 2022 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Scaffold ;

FROM SymbolTable IMPORT NulSym, MakeProcedure, PutFunction,
                        PutPublic, PutCtor, PutParam, IsProcedure,
                        MakeConstant, PutExtern, MakeArray, PutArray,
                        MakeSubrange, PutSubrange,
                        MakeSubscript, PutSubscript, PutArraySubscript,
                        MakeVar, PutVar, MakeProcedureCtorExtern,
                        GetMainModule,
                        GetSymName, StartScope, EndScope ;

FROM NameKey IMPORT NulName, Name, MakeKey, makekey, KeyToCharStar ;
FROM M2Base IMPORT Integer, Cardinal ;
FROM M2System IMPORT Address ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM Assertion IMPORT Assert ;
FROM Lists IMPORT List, InitList, IncludeItemIntoList, NoOfItemsInList, GetItemFromList ;
FROM M2MetaError IMPORT MetaErrorT0 ;

FROM SFIO IMPORT OpenToWrite, WriteS, ReadS, OpenToRead, Exists ;
FROM FIO IMPORT File, EOF, IsNoError, Close ;
FROM M2Options IMPORT GetUselist ;
FROM M2Base IMPORT Proc ;
FROM M2Quads IMPORT PushTFtok, PushTtok, BuildDesignatorArray, BuildAssignment ;

FROM DynamicStrings IMPORT String, InitString, KillString, ConCat, RemoveWhitePrefix,
                    EqualArray, Mark, Assign, Fin, InitStringChar, Length, Slice, Equal,
                    RemoveComment, string ;

CONST
   Comment = '#'  ; (* Comment leader      *)

VAR
   ctorModules,
   ctorGlobals  : List ;
   ctorArray,
   ctorArrayType: CARDINAL ;


(* The dynamic scaffold takes the form:

static void _M2_init (int argc, char *argv[], char *envp[])
{
  M2RTS_ConstructModules (module_name, argc, argv, envp);
}


static void _M2_finish (int argc, char *argv[], char *envp[])
{
  M2RTS_Terminate ();
  M2RTS_DeconstructModules (module_name, argc, argv, envp);
}


int
main (int argc, char *argv[], char *envp[])
{
  init (argc, argv, envp);
  finish ();
  return (0);
}  *)


(*
   DeclareCtorArrayType - declare an ARRAY [0..high] OF PROC which will
                          be used to reference every module ctor.
*)

PROCEDURE DeclareCtorArrayType (tokenno: CARDINAL; high: CARDINAL) : CARDINAL ;
VAR
   subscript,
   subrange : CARDINAL ;
BEGIN
   (* ctorArrayType = ARRAY [0..n] OF PROC ;  *)
   ctorArrayType := MakeArray (tokenno, MakeKey ('ctorGlobalType')) ;
   PutArray (ctorArrayType, Proc) ;
   subrange := MakeSubrange (tokenno, NulName) ;
   PutSubrange (subrange,
                MakeConstant (tokenno, 0),
                MakeConstant (tokenno, high),
                Cardinal) ;
   subscript := MakeSubscript () ;
   PutSubscript (subscript, subrange) ;
   PutArraySubscript (ctorArrayType, subscript) ;
   RETURN ctorArrayType
END DeclareCtorArrayType ;


(*
   DeclareCtorGlobal - declare the ctorArray variable.
*)

PROCEDURE DeclareCtorGlobal (tokenno: CARDINAL) ;
VAR
   n: CARDINAL ;
BEGIN
   n := NoOfItemsInList (ctorGlobals) ;
   ctorArrayType := DeclareCtorArrayType (tokenno, n) ;
   ctorArray := MakeVar (tokenno, MakeKey ('_M2_ctorArray')) ;
   PutVar (ctorArray, ctorArrayType)
END DeclareCtorGlobal ;


(*
   PopulateCtorArray - assign each element of the ctorArray to the external module ctor.
                       This is only used to force the linker to pull in the ctors from
                       a library.
*)

PROCEDURE PopulateCtorArray (tok: CARDINAL) ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList (ctorModules) ;
   i := 1 ;
   WHILE i <= n DO
      PushTFtok (ctorArray, ctorArrayType, tok) ;
      PushTtok (MakeConstant (tok, i), tok) ;
      BuildDesignatorArray ;
      PushTtok (GetItemFromList (ctorModules, i), tok) ;
      BuildAssignment (tok) ;
      INC (i)
   END
END PopulateCtorArray ;


(*
   ReadModules - populate ctorGlobals with the modules specified by -fuselist=filename.
*)

PROCEDURE ReadModules (filename: String) ;
VAR
   f: File ;
   s: String ;
BEGIN
   InitList (ctorGlobals) ;
   f := OpenToRead (filename) ;
   WHILE NOT EOF (f) DO
      s := ReadS (f) ;
      s := RemoveComment (RemoveWhitePrefix (s), Comment) ;
      IF (NOT Equal (Mark (InitStringChar (Comment)),
                     Mark (Slice (s, 0, Length (Mark (InitStringChar (Comment)))-1)))) AND
         (NOT EqualArray (s, ''))
      THEN
         IncludeItemIntoList (ctorGlobals, makekey (string (s)))
      END ;
      s := KillString (s)
   END ;
   Close (f)
END ReadModules ;


(*
   CreateCtorList - uses GetUselist as the filename and then reads the list of modules.
*)

PROCEDURE CreateCtorList (tok: CARDINAL) : BOOLEAN ;
VAR
   filename: String ;
BEGIN
   filename := GetUselist () ;
   IF filename = NIL
   THEN
      RETURN FALSE
   ELSE
      IF Exists (filename)
      THEN
         ReadModules (filename)
      ELSE
         MetaErrorT0 (tok,
                      '{%E}the filename specified by the -fuselist= option does not exist') ;
         RETURN FALSE
      END
   END ;
   RETURN TRUE
END CreateCtorList ;


(*
   DeclareCtorModuleExtern - declare an extern _M2_modulename_ctor procedure for each module.
*)

PROCEDURE DeclareCtorModuleExtern (tokenno: CARDINAL) ;
VAR
   name: Name ;
   n, i: CARDINAL ;
BEGIN
   InitList (ctorModules) ;
   i := 1 ;
   n := NoOfItemsInList (ctorGlobals) ;
   WHILE i <= n DO
      name := GetItemFromList (ctorGlobals, i) ;
      IF name # GetSymName (GetMainModule ())
      THEN
         IncludeItemIntoList (ctorModules, MakeProcedureCtorExtern (tokenno, name))
      END ;
      INC (i)
   END
END DeclareCtorModuleExtern ;


(*
   DeclareScaffoldFunctions - declare main, _M2_init,_M2_finish
                              and _M2_link to the modula-2
                              front end.
*)

PROCEDURE DeclareScaffoldFunctions (tokenno: CARDINAL) ;
BEGIN
   IF CreateCtorList (tokenno)
   THEN
      DeclareCtorGlobal (tokenno) ;
      DeclareCtorModuleExtern (tokenno) ;
      linkFunction := MakeProcedure (tokenno, MakeKey ("_M2_link"))
   END ;

   mainFunction := MakeProcedure (tokenno, MakeKey ("main")) ;
   StartScope (mainFunction) ;
   PutFunction (mainFunction, Integer) ;
   DeclareArgEnvParams (tokenno, mainFunction) ;
   PutPublic (mainFunction, TRUE) ;
   EndScope ;

   initFunction := MakeProcedure (tokenno, MakeKey ("_M2_init")) ;
   DeclareArgEnvParams (tokenno, initFunction) ;

   finiFunction := MakeProcedure (tokenno, MakeKey ("_M2_finish")) ;
   DeclareArgEnvParams (tokenno, finiFunction)
END DeclareScaffoldFunctions ;


(*
   DeclareArgEnvParams - declares (int argc, void *argv, void *envp)
*)

PROCEDURE DeclareArgEnvParams (tokno: CARDINAL; proc: CARDINAL) ;
BEGIN
   Assert (IsProcedure (proc)) ;
   StartScope (proc) ;
   Assert (PutParam (tokno, proc, 1, MakeKey ("argc"), Integer, FALSE)) ;
   Assert (PutParam (tokno, proc, 2, MakeKey ("argv"), Address, FALSE)) ;
   Assert (PutParam (tokno, proc, 3, MakeKey ("envp"), Address, FALSE)) ;
   EndScope
END DeclareArgEnvParams ;


(*
   DeclareScaffold - declare scaffold related entities.
*)

PROCEDURE DeclareScaffold (tokno: CARDINAL) ;
BEGIN
   DeclareScaffoldFunctions (tokno)
END DeclareScaffold ;


BEGIN
   finiFunction := NulSym ;
   initFunction := NulSym ;
   mainFunction := NulSym ;
   linkFunction := NulSym ;
   ctorGlobals := NIL ;
   ctorModules := NIL
END M2Scaffold.
