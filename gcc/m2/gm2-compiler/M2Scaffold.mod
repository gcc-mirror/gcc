(* M2Scaffold.mod declare and create scaffold entities.

Copyright (C) 2022-2025 Free Software Foundation, Inc.
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

FROM SymbolTable IMPORT NulSym, ProcedureKind, MakeProcedure, PutFunction,
                        PutPublic, PutCtor, PutParam, IsProcedure,
                        MakeConstant, PutExtern, MakeArray, PutArray,
                        MakeSubrange, PutSubrange,
                        MakeSubscript, PutSubscript, PutArraySubscript,
                        MakeVar, PutVar, MakeProcedureCtorExtern,
                        PutMonoName,
                        GetMainModule, GetModuleCtors, MakeDefImp,
                        PutModuleCtorExtern, IsDefinitionForC,
                        ForeachModuleDo, IsDefImp, IsModule,
                        IsModuleBuiltin, IsImport, IsImportStatement,
                        GetSymName, StartScope, EndScope,
                        GetModuleDefImportStatementList,
                        GetModuleModImportStatementList,
                        GetImportModule, GetImportStatementList,
                        PutLibName,
                        PutProcedureDeclaredTok, PutProcedureParametersDefined,
                        PutProcedureDefined ;

FROM NameKey IMPORT NulName, Name, MakeKey, makekey, KeyToCharStar ;
FROM M2Base IMPORT Integer, Cardinal ;
FROM M2System IMPORT Address ;
FROM M2LexBuf IMPORT GetTokenNo, BuiltinTokenNo ;
FROM Assertion IMPORT Assert ;
FROM Lists IMPORT List, InitList, IncludeItemIntoList, NoOfItemsInList, GetItemFromList, KillList, IsItemInList ;
FROM M2MetaError IMPORT MetaErrorT0, MetaErrorStringT0 ;
FROM M2Search IMPORT FindSourceDefFile ;

FROM SFIO IMPORT OpenToWrite, WriteS, ReadS, OpenToRead, Exists ;
FROM FIO IMPORT File, EOF, IsNoError, Close ;
FROM FormatStrings IMPORT Sprintf1 ;

FROM M2Options IMPORT GetUselist, ScaffoldStatic, ScaffoldDynamic, GenModuleList,
                      GetGenModuleFilename, GetUselistFilename, GetUselist, cflag,
                      SharedFlag, WholeProgram ;

FROM M2Base IMPORT Proc ;

FROM M2Quads IMPORT PushTFtok, PushTtok, PushT, BuildDesignatorArray, BuildAssignment,
                    BuildProcedureCall ;

FROM M2Batch IMPORT IsModuleKnown, Get ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3, printf4 ;
FROM FormatStrings IMPORT HandleEscape ;

FROM DynamicStrings IMPORT String, InitString, KillString, ConCat, RemoveWhitePrefix,
                    EqualArray, Mark, Assign, Fin, InitStringChar, Length, Slice, Equal,
                    RemoveComment, string, InitStringCharStar ;

FROM M2Graph IMPORT Graph, InitGraph, KillGraph, AddDependent, SortGraph ;


CONST
   Comment   = '#'  ; (* Comment leader      *)
   Debugging = FALSE ;

VAR
   uselistModules,
   ctorModules,
   ctorGlobals   : List ;
   ctorArrayType : CARDINAL ;
   initialized   : BOOLEAN ;


(* The dynamic scaffold takes the form:

static void _M2_init (int argc, char *argv[], char *envp[])
{
  M2RTS_ConstructModules (module_name, libname, overrideliborder, argc, argv, envp);
}


static void _M2_fini (int argc, char *argv[], char *envp[])
{
  M2RTS_Terminate ();
  M2RTS_DeconstructModules (module_name, libname, argc, argv, envp);
}


int
main (int argc, char *argv[], char *envp[])
{
  init (argc, argv, envp);
  fini (argc, argv, envp);
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
                                               call _M2_module_fini (argc, argv, envp);
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
   sym     : CARDINAL ;
   FileName,
   LibName : String ;
BEGIN
   sym := Get (name) ;
   IF sym = NulSym
   THEN
      LibName := NIL ;
      FileName := NIL ;
      IF FindSourceDefFile (InitStringCharStar (KeyToCharStar (name)),
                            FileName, LibName)
      THEN
         sym := MakeDefImp (tok, name) ;
         PutLibName (sym, makekey (string (LibName))) ;
         IF sym # GetMainModule ()
         THEN
            PutModuleCtorExtern (tok, sym, NOT WholeProgram)
         END
      ELSE
         MetaErrorStringT0 (tok,
                            Sprintf1 (InitString ('the definition module file for {%%1a} cannot be found'),
                                      name))
      END
   END ;
   RETURN sym
END LookupModuleSym ;


(*
   addDependentStatement -
*)

PROCEDURE addDependentStatement (graph: Graph; moduleSym: CARDINAL; list: List) ;
VAR
   n1, n2: Name ;
   import,
   depmod,
   i, n  : CARDINAL ;
BEGIN
   n := NoOfItemsInList (list) ;
   i := 1 ;
   WHILE i <= n DO
      import := GetItemFromList (list, i) ;
      Assert (IsImport (import)) ;
      depmod := GetImportModule (import) ;
      AddDependent (graph, moduleSym, depmod) ;
      IF Debugging
      THEN
         n1 := GetSymName (moduleSym) ;
         n2 := GetSymName (depmod) ;
         printf2 ("AddDependent (%a, %a)\n",
                  n1, n2)
      END ;
      INC (i)
   END
END addDependentStatement ;


(*
   addDependentImport - adds dependent imports of moduleSym into the graph.
*)

PROCEDURE addDependentImport (graph: Graph; moduleSym: CARDINAL; importList: List) ;
VAR
   stmt,
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList (importList) ;
   i := 1 ;
   WHILE i <= n DO
      stmt := GetItemFromList (importList, i) ;
      Assert (IsImportStatement (stmt)) ;
      addDependentStatement (graph, moduleSym, GetImportStatementList (stmt)) ;
      INC (i)
   END
END addDependentImport ;


(*
   TopologicallySortList - topologically sort the list based on import graph.
                           A new list is returned.
*)

PROCEDURE TopologicallySortList (list: List; topModule: CARDINAL) : List ;
VAR
   graph    : Graph ;
   i, n     : CARDINAL ;
   moduleSym: CARDINAL ;
BEGIN
   graph := InitGraph () ;
   n := NoOfItemsInList (list) ;
   i := 1 ;
   WHILE i <= n DO
      moduleSym := GetItemFromList (uselistModules, i) ;
      addDependentImport (graph, moduleSym, GetModuleDefImportStatementList (moduleSym)) ;
      addDependentImport (graph, moduleSym, GetModuleModImportStatementList (moduleSym)) ;
      INC (i) ;
   END ;
   (* Ensure that topModule is also in the graph.  *)
   IF NOT IsItemInList (list, topModule)
   THEN
      addDependentImport (graph, topModule, GetModuleDefImportStatementList (topModule)) ;
      addDependentImport (graph, topModule, GetModuleModImportStatementList (topModule))
   END ;
   RETURN SortGraph (graph, topModule)
END TopologicallySortList ;


(*
   AddEntry - adds an entry to the ctorGlobals and uselistModules.
*)

PROCEDURE AddEntry (tok: CARDINAL; name: Name) ;
BEGIN
   IF ctorGlobals # NIL
   THEN
      IncludeItemIntoList (ctorGlobals, name)
   END ;
   IncludeItemIntoList (uselistModules, LookupModuleSym (tok, name))
END AddEntry ;


(*
   ReadModules - populate ctorGlobals with the modules specified by -fuse-list=filename.
*)

PROCEDURE ReadModules (tok: CARDINAL; filename: String) ;
VAR
   f: File ;
   s: String ;
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
         AddEntry (tok, makekey (string (s)))
      END ;
      s := KillString (s)
   END ;
   Close (f)
END ReadModules ;


VAR
   ctorTok: CARDINAL ;


(*
   AddModuleToCtor - adds moduleSym to the uselistModules and
                     sets all modules ctors as extern.
*)

PROCEDURE AddModuleToCtor (moduleSym: CARDINAL) ;
BEGIN
   IF IsModule (moduleSym) OR (NOT IsDefinitionForC (moduleSym))
   THEN
      IF (moduleSym # GetMainModule ()) AND (NOT IsModuleBuiltin (moduleSym))
      THEN
         PutModuleCtorExtern (ctorTok, moduleSym, NOT WholeProgram) ;
         IncludeItemIntoList (uselistModules, moduleSym)
      END
   END
END AddModuleToCtor ;


(*
   WriteList - writes the list to GetGenModuleFilename
               providing the filename is not NIL and not '-'.
*)

PROCEDURE WriteList (tok: CARDINAL; list: List) ;
VAR
   fo       : File ;
   name     : Name ;
   moduleSym: CARDINAL ;
   i, n     : CARDINAL ;
   s        : String ;
BEGIN
   IF (GetGenModuleFilename () # NIL) AND (NOT EqualArray (GetGenModuleFilename (), '-'))
   THEN
      fo := OpenToWrite (GetGenModuleFilename ()) ;
      IF IsNoError (fo)
      THEN
         i := 1 ;
         n := NoOfItemsInList (list) ;
         WHILE i <= n DO
            moduleSym := GetItemFromList (list, i) ;
            name := GetSymName (moduleSym) ;
            s := InitStringCharStar (KeyToCharStar (name)) ;
            s := ConCat (s, Mark (InitString ('\n'))) ;
            s := HandleEscape (s) ;
            s := WriteS (fo, s) ;
            s := KillString (s) ;
            INC (i)
         END ;
         Close (fo)
      ELSE
         s := InitString ("unable to create file containing ctor module list: ") ;
         s := ConCat (s, GetGenModuleFilename ()) ;
         MetaErrorStringT0 (tok, s)
      END
   END
END WriteList ;


(*
   CreateCtorListFromImports - if GenModuleList then populate
                               the ctor list from all modules which are
                               not FOR 'C'.
*)

PROCEDURE CreateCtorListFromImports (tok: CARDINAL) : BOOLEAN ;
VAR
   newlist: List ;
   i, n   : CARDINAL ;
BEGIN
   IF GenModuleList
   THEN
      InitList (uselistModules) ;
      ctorTok := tok ;
      ForeachModuleDo (AddModuleToCtor) ;
      newlist := TopologicallySortList (uselistModules, GetMainModule ()) ;
      KillList (uselistModules) ;
      uselistModules := newlist ;
      (* Now create the ctorGlobals using uselistModules and retain the same order.  *)
      InitList (ctorGlobals) ;
      i := 1 ;
      n := NoOfItemsInList (uselistModules) ;
      WHILE i <= n DO
         IncludeItemIntoList (ctorGlobals, GetSymName (GetItemFromList (uselistModules, i))) ;
         INC (i)
      END ;
      WriteList (tok, uselistModules) ;
      RETURN TRUE
   END ;
   RETURN FALSE
END CreateCtorListFromImports ;


(*
   CreateCtorList - uses GetUselistFilename and then reads the list of modules.
*)

PROCEDURE CreateCtorList (tok: CARDINAL) : BOOLEAN ;
VAR
   filename: String ;
BEGIN
   IF GetUselist ()
   THEN
      filename := GetUselistFilename () ;
      IF filename # NIL
      THEN
         IF Exists (filename)
         THEN
            ReadModules (tok, filename) ;
            RETURN TRUE
         ELSE
            IF NOT EqualArray (filename, '-')
            THEN
               MetaErrorT0 (tok,
                            '{%E}the filename specified by the -fuse-list= option does not exist') ;
            END
         END
      END ;
      RETURN FALSE
   ELSE
      RETURN CreateCtorListFromImports (tok)
   END
END CreateCtorList ;


(*
   DeclareModuleExtern - declare the extern _M2_modulename_ctor, _M2_modulename_init,
                         _M2_modulename_fini, _M2_modulename_dep for each external module.
*)

PROCEDURE DeclareModuleExtern (tokenno: CARDINAL) ;
VAR
   n1    : Name ;
   init,
   fini,
   dep,
   ctor,
   module: CARDINAL ;
   n, i  : CARDINAL ;
BEGIN
   InitList (ctorModules) ;
   i := 1 ;
   n := NoOfItemsInList (uselistModules) ;
   WHILE i <= n DO
      module := GetItemFromList (uselistModules, i) ;
      IF module # GetMainModule ()
      THEN
         PutModuleCtorExtern (tokenno, module, NOT WholeProgram)
      END ;
      GetModuleCtors (module, ctor, init, fini, dep) ;
      IncludeItemIntoList (ctorModules, ctor) ;
      IF Debugging
      THEN
         n1 := GetSymName (module) ;
         printf1 ("%a_ctor added to ctorModules\n", n1)
      END ;
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
      linkFunction := MakeProcedure (tokenno, MakeKey ("_M2_link")) ;
      PutMonoName (linkFunction, TRUE) ;
      PutProcedureDefined (linkFunction, ProperProcedure) ;
   ELSIF ScaffoldDynamic AND (NOT cflag)
   THEN
      MetaErrorT0 (tokenno,
                   '{%O}dynamic linking enabled but no module ctor list has been created, hint use -fuse-list=filename or -fgen-module-list=-')
   END ;

   initFunction := MakeProcedure (tokenno, MakeKey ("_M2_init")) ;
   PutMonoName (initFunction, TRUE) ;
   PutProcedureDefined (initFunction, ProperProcedure) ;
   finiFunction := MakeProcedure (tokenno, MakeKey ("_M2_fini")) ;
   PutMonoName (finiFunction, TRUE) ;
   PutProcedureDefined (initFunction, ProperProcedure) ;
   IF SharedFlag
   THEN
      PutCtor (initFunction, TRUE) ;
      PutCtor (finiFunction, TRUE)
   ELSE
      DeclareArgEnvParams (tokenno, initFunction) ;
      DeclareArgEnvParams (tokenno, finiFunction) ;

      mainFunction := MakeProcedure (tokenno, MakeKey ("main")) ;
      PutMonoName (mainFunction, TRUE) ;
      StartScope (mainFunction) ;
      PutFunction (BuiltinTokenNo, mainFunction, ProperProcedure, Integer) ;
      DeclareArgEnvParams (tokenno, mainFunction) ;
      PutPublic (mainFunction, TRUE) ;
      PutProcedureDefined (mainFunction, ProperProcedure) ;
      EndScope
   END
END DeclareScaffoldFunctions ;


(*
   DeclareArgEnvParams - declares (int argc, void *argv, void *envp)
*)

PROCEDURE DeclareArgEnvParams (tokno: CARDINAL; proc: CARDINAL) ;
BEGIN
   Assert (IsProcedure (proc)) ;
   StartScope (proc) ;
   Assert (PutParam (tokno, proc, ProperProcedure, 1, MakeKey ("argc"), Integer, FALSE, tokno)) ;
   Assert (PutParam (tokno, proc, ProperProcedure, 2, MakeKey ("argv"), Address, FALSE, tokno)) ;
   Assert (PutParam (tokno, proc, ProperProcedure, 3, MakeKey ("envp"), Address, FALSE, tokno)) ;
   PutProcedureParametersDefined (proc, ProperProcedure) ;
   PutProcedureDeclaredTok (proc, ProperProcedure, tokno) ;
   EndScope
END DeclareArgEnvParams ;


(*
   DeclareScaffold - declare scaffold related entities.
*)

PROCEDURE DeclareScaffold (tokno: CARDINAL) ;
BEGIN
   IF NOT initialized
   THEN
      initialized := TRUE ;
      DeclareScaffoldFunctions (tokno)
   END
END DeclareScaffold ;


BEGIN
   initialized := FALSE ;
   finiFunction := NulSym ;
   initFunction := NulSym ;
   mainFunction := NulSym ;
   linkFunction := NulSym ;
   ctorArray := NulSym ;
   ctorGlobals := NIL ;
   ctorModules := NIL ;
   uselistModules := NIL
END M2Scaffold.
