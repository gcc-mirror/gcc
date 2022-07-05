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
                        GetMainModule, GetModuleCtors, MakeDefImp,
                        PutModuleCtorExtern,
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
FROM M2Options IMPORT GetUselist, ScaffoldStatic ;
FROM M2Base IMPORT Proc ;

FROM M2Quads IMPORT PushTFtok, PushTtok, PushT, BuildDesignatorArray, BuildAssignment,
                    BuildProcedureCall ;

FROM M2Batch IMPORT IsModuleKnown, Get ;

FROM DynamicStrings IMPORT String, InitString, KillString, ConCat, RemoveWhitePrefix,
                    EqualArray, Mark, Assign, Fin, InitStringChar, Length, Slice, Equal,
                    RemoveComment, string ;

CONST
   Comment = '#'  ; (* Comment leader      *)

VAR
   uselistModules,
   ctorModules,
   ctorGlobals   : List ;
   ctorArray,
   ctorArrayType : CARDINAL ;


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
   ForeachModuleCallInit - is only called when -fscaffold-static is enabled.
                           precondition: the module list will be ordered.
                           postcondition: foreach module in the application universe
                                             call _M2_module_init (argc, argv, envp);
*)

PROCEDURE ForeachModuleCallInit (tok: CARDINAL; argc, argv, envp: CARDINAL) ;
VAR
   module    : CARDINAL ;
   i, n      : CARDINAL ;
   ctor, init,
   fini, dep : CARDINAL ;
BEGIN
   i := 1 ;
   n := NoOfItemsInList (uselistModules) ;
   WHILE i <= n DO
      module := GetItemFromList (uselistModules, i) ;
      IF module # NulSym
      THEN
         GetModuleCtors (module, ctor, init, fini, dep) ;
         IF init # NulSym
         THEN
            PushTtok (init, tok) ;
            PushTtok (argc, tok) ;
            PushTtok (argv, tok) ;
            PushTtok (envp, tok) ;
            PushT (3) ;
            BuildProcedureCall (tok)
         END
      END ;
      INC (i)
   END
END ForeachModuleCallInit ;


(*
   ForeachModuleCallFinish - precondition: the module list will be ordered.
                             postcondition: foreach module in the application universe
                                               call _M2_module_finish (argc, argv, envp);
*)

PROCEDURE ForeachModuleCallFinish (tok: CARDINAL; argc, argv, envp: CARDINAL) ;
VAR
   module    : CARDINAL ;
   i         : CARDINAL ;
   ctor, init,
   fini, dep : CARDINAL ;
BEGIN
   i := NoOfItemsInList (uselistModules) ;
   WHILE i >= 1 DO
      module := GetItemFromList (uselistModules, i) ;
      IF module # NulSym
      THEN
         GetModuleCtors (module, ctor, init, fini, dep) ;
         IF fini # NulSym
         THEN
            PushTtok (fini, tok) ;
            PushTtok (argc, tok) ;
            PushTtok (argv, tok) ;
            PushTtok (envp, tok) ;
            PushT (3) ;
            BuildProcedureCall (tok)
         END
      END ;
      DEC (i)
   END
END ForeachModuleCallFinish ;


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
   LookupModuleSym - returns a defimp module.  It looks up an existing
                     module and if this does not exist creates a new one.
*)

PROCEDURE LookupModuleSym (tok: CARDINAL; name: Name) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := Get (name) ;
   IF sym = NulSym
   THEN
      sym := MakeDefImp (tok, name)
   END ;
   IF sym # GetMainModule ()
   THEN
      PutModuleCtorExtern (tok, sym)
   END ;
   RETURN sym
END LookupModuleSym ;


(*
   ReadModules - populate ctorGlobals with the modules specified by -fuselist=filename.
*)

PROCEDURE ReadModules (tok: CARDINAL; filename: String) ;
VAR
   f   : File ;
   s   : String ;
   name: Name ;
BEGIN
   InitList (ctorGlobals) ;
   InitList (uselistModules) ;
   f := OpenToRead (filename) ;
   WHILE NOT EOF (f) DO
      s := ReadS (f) ;
      s := RemoveComment (RemoveWhitePrefix (s), Comment) ;
      IF (NOT Equal (Mark (InitStringChar (Comment)),
                     Mark (Slice (s, 0, Length (Mark (InitStringChar (Comment)))-1)))) AND
         (NOT EqualArray (s, ''))
      THEN
         name := makekey (string (s)) ;
         IncludeItemIntoList (ctorGlobals, name) ;
         IncludeItemIntoList (uselistModules, LookupModuleSym (tok, name))
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
         ReadModules (tok, filename)
      ELSE
         MetaErrorT0 (tok,
                      '{%E}the filename specified by the -fuselist= option does not exist') ;
         RETURN FALSE
      END
   END ;
   RETURN TRUE
END CreateCtorList ;


(*
   DeclareModuleExtern - declare the extern _M2_modulename_ctor, _M2_modulename_init,
                         _M2_modulename_fini, _M2_modulename_dep for each external module.
*)

PROCEDURE DeclareModuleExtern (tokenno: CARDINAL) ;
VAR
   init,
   fini,
   dep,
   ctor,
   module: CARDINAL ;
   n, i : CARDINAL ;
BEGIN
   InitList (ctorModules) ;
   i := 1 ;
   n := NoOfItemsInList (uselistModules) ;
   WHILE i <= n DO
      module := GetItemFromList (uselistModules, i) ;
      IF module # GetMainModule ()
      THEN
         PutModuleCtorExtern (tokenno, module)
      END ;
      GetModuleCtors (module, ctor, init, fini, dep) ;
      IncludeItemIntoList (ctorModules, ctor) ;
      INC (i)
   END
END DeclareModuleExtern ;


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
      DeclareModuleExtern (tokenno) ;
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
   ctorModules := NIL ;
   uselistModules := NIL
END M2Scaffold.
