(* P1SymBuild.mod pass 1 symbol creation.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE P1SymBuild ;


FROM ASCII IMPORT nul ;
FROM NameKey IMPORT Name, WriteKey, MakeKey, KeyToCharStar, NulName ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2LexBuf IMPORT GetFileName, GetTokenNo, UnknownTokenNo ;
FROM M2MetaError IMPORT MetaErrorString2, MetaError0, MetaError1, MetaError2, MetaErrorT1, MetaErrorT2 ;
FROM DynamicStrings IMPORT String, Slice, InitString, KillString, EqualCharStar, RIndex, Mark, ConCat ;
FROM M2Printf IMPORT printf0, printf1, printf2 ;
FROM M2Options IMPORT Iso ;

FROM M2Reserved IMPORT ImportTok, ExportTok, QualifiedTok, UnQualifiedTok,
                       NulTok, VarTok, ArrayTok, BuiltinTok, InlineTok ;

FROM FifoQueue IMPORT PutEnumerationIntoFifoQueue ;
FROM P0SymBuild IMPORT EnterBlock, LeaveBlock ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr,
                        AppendModuleOnImportStatement,
                        AppendModuleImportStatement,
                        MakeImportStatement, MakeImport,

                        StartScope, EndScope, PseudoScope,
                        GetScope, GetCurrentScope,
                        IsDeclaredIn,
                        SetCurrentModule, SetFileModule,
                        MakeInnerModule,
                        MakeEnumeration, MakeSubrange,
                        MakeVar, MakeType, PutType,
                        MakeHiddenType,
                        PutMode,
                        PutFieldEnumeration, PutSubrange, PutVar,
                        IsDefImp, IsModule, IsInnerModule, IsType,
                        GetCurrentModule,
                        AddSymToModuleScope,
                        AddNameToImportList,
                        GetSym, RequestSym, IsUnknown, RenameSym,
                        GetFromOuterModule,
                        GetExported, IsExported,
                        GetLocalSym,
                        PutImported, PutIncludedByDefinition,
                        PutExported, PutExportQualified, PutExportUnQualified,
                        TryMoveUndeclaredSymToInnerModule,
                        PutDefinitionForC,
                        IsDefinitionForC,
                        PutDoesNeedExportList, PutDoesNotNeedExportList,
                        DoesNotNeedExportList,
                        MakeProcedure,
                        PutFunction, PutParam, PutVarParam,
                        GetNthParam,
                        IsProcedure, IsConstString,
                        MakePointer, PutPointer,
                        MakeRecord, PutFieldRecord,
                        MakeArray,
                        MakeSubscript, PutSubscript,
                        PutArray, GetType, IsArray,
                        IsProcType, MakeProcType,
                        PutProcTypeVarParam, PutProcTypeParam,
                        PutProcedureBuiltin, PutProcedureInline,
                        GetSymName,
                        ResolveImports, PutDeclared,
                        MakeError, MakeErrorS,
                        DisplayTrees ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource,
                    LookupModule, LookupOuterModule ;

FROM M2Quads IMPORT PushT, PopT, PushTF, PopTF, OperandT, PopN, OperandTok,
                    PopTtok, PushTtok, PushTFtok, PopTFtok ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

CONST
   Debugging = FALSE ;

VAR
   importStatementCount: CARDINAL ;


(*
   CheckFileName - checks to see that the module name matches the file name.
*)

(*
PROCEDURE CheckFileName (tok: CARDINAL; name: Name; ModuleType: ARRAY OF CHAR) ;
VAR
   ext,
   basename: INTEGER ;
   s,
   FileName: String ;
BEGIN
   FileName := GetFileName() ;
   basename := RIndex(FileName, '/', 0) ;
   IF basename=-1
   THEN
      basename := 0
   END ;
   ext := RIndex(FileName, '.', 0) ;
   IF ext=-1
   THEN
      ext := 0
   END ;
   FileName := Slice(FileName, basename, ext) ;
   IF EqualCharStar(FileName, KeyToCharStar(name))
   THEN
      FileName := KillString(FileName)
   ELSE
      s := ConCat (InitString (ModuleType),
                   Mark (InitString (" module name {%1Ea} is inconsistant with the filename {%F{%2a}}"))) ;
      MetaErrorString2 (s, MakeError (tok, name), MakeErrorS (tok, FileName))
   END
END CheckFileName ;
*)


(*
   StartBuildDefinitionModule - Creates a definition module and starts
                                a new scope.

                                he Stack is expected:

                                Entry                 Exit

                         Ptr ->
                                +------------+
                                | NameStart  |                       <- Ptr
                                |------------|        +------------+
                                | NulName/"C"|        | NameStart  |
                                |------------|        |------------|
*)

PROCEDURE P1StartBuildDefinitionModule ;
VAR
   name     : Name ;
   language,
   ModuleSym: CARDINAL ;
BEGIN
   importStatementCount := 0 ;
   PopT(name) ;
   (* CheckFileName(name, 'definition') ; *)
   ModuleSym := MakeDefinitionSource(GetTokenNo(), name) ;
   PutDoesNotNeedExportList(ModuleSym) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingDefinitionModule()) ;
   PopT(language) ;
   IF (language#NulSym) AND IsConstString(language)
   THEN
      IF GetSymName(language)=MakeKey('C')
      THEN
         PutDefinitionForC(ModuleSym)
      ELSIF GetSymName(language)=NulName
      THEN
         MetaError0 ('{%E}currently a non modula-2 definition module can only be declared as DEFINITION FOR {%k"C"}')
      ELSE
         MetaError1 ('unknown definition module language {%1Ea}, currently a non modula-2 definition module can only be declared as DEFINITION FOR {%k"C"}', language)
      END
   END ;
   PushT(name) ;
   EnterBlock(name)
END P1StartBuildDefinitionModule ;


(*
   EndBuildDefinitionModule - Destroys the definition module scope and
                              checks for correct name.

                              The Stack is expected:

                              Entry                 Exit

                       Ptr ->
                              +------------+        +-----------+
                              | NameEnd    |        |           |
                              |------------|        |-----------|
                              | NameStart  |        |           | <- Ptr
                              |------------|        |-----------|
*)

PROCEDURE P1EndBuildDefinitionModule ;
VAR
   start    : CARDINAL ;
   NameStart,
   NameEnd  : Name ;
BEGIN
   Assert(CompilingDefinitionModule()) ;
   EndScope ;
   PopTtok(NameStart, start) ;
   PopT(NameEnd) ;
   IF Debugging
   THEN
      printf0('pass 1: ') ;
      DisplayTrees(GetCurrentModule())
   END ;
   IF NameStart#NameEnd
   THEN
      MetaError1 ('inconsistant definition module name {%1Wa}', MakeError (start, NameStart))
   END ;
   LeaveBlock
END P1EndBuildDefinitionModule ;


(*
   StartBuildImplementationModule - Creates an implementation module and starts
                                    a new scope.

                                    The Stack is expected:

                                    Entry                 Exit

                             Ptr ->                                     <- Ptr
                                    +------------+        +-----------+
                                    | NameStart  |        | NameStart |
                                    |------------|        |-----------|

*)

PROCEDURE P1StartBuildImplementationModule ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   importStatementCount := 0 ;
   PopTtok (name, tok) ;
   (* CheckFileName(name, 'implementation') ; *)
   ModuleSym := MakeImplementationSource (tok, name) ;
   SetCurrentModule (ModuleSym) ;
   SetFileModule (ModuleSym) ;
   StartScope (ModuleSym) ;
   IF NOT IsDefImp (ModuleSym)
   THEN
      MetaError1 ('cannot find corresponding definition module for {%1Ea}', ModuleSym)
   END ;
   Assert (CompilingImplementationModule()) ;
   PushTtok (name, tok) ;
   EnterBlock (name)
END P1StartBuildImplementationModule ;


(*
   EndBuildImplementationModule - Destroys the implementation module scope and
                                  checks for correct name.

                                  The Stack is expected:

                                  Entry                 Exit

                           Ptr ->
                                  +------------+        +-----------+
                                  | NameEnd    |        |           |
                                  |------------|        |-----------|
                                  | NameStart  |        |           | <- Ptr
                                  |------------|        |-----------|
*)

PROCEDURE P1EndBuildImplementationModule ;
VAR
   start, end: CARDINAL ;
   NameStart,
   NameEnd   : Name ;
BEGIN
   ResolveImports ;
   Assert(CompilingImplementationModule()) ;
   EndScope ;
   PopTtok(NameStart, start) ;
   PopTtok(NameEnd, end) ;
   IF NameStart#NameEnd
   THEN
      MetaErrorT1 (end,
                   'inconsistant implementation module name {%1Wa}', MakeError (start, NameStart))
   END ;
   LeaveBlock
END P1EndBuildImplementationModule ;


(*
   StartBuildProgramModule - Creates a program module and starts
                             a new scope.

                             The Stack is expected:

                             Entry                 Exit

                      Ptr ->                                     <- Ptr
                             +------------+        +-----------+
                             | NameStart  |        | NameStart |
                             |------------|        |-----------|

*)

PROCEDURE P1StartBuildProgramModule ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   importStatementCount := 0 ;
   PopTtok(name, tok) ;
   (* CheckFileName(name, 'main') ; *)
   ModuleSym := MakeProgramSource(tok, name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   IF (NOT CompilingProgramModule()) OR IsDefImp(ModuleSym)
   THEN
      MetaErrorT1 (tok,
                   'module {%1Ea} has a corresponding DEFINITION MODULE but no IMPLEMENTATION keyword in the main module', ModuleSym)
   END ;
   PushTtok(name, tok) ;
   EnterBlock(name)
END P1StartBuildProgramModule ;


(*
   EndBuildProgramModule - Destroys the program module scope and
                           checks for correct name.

                           The Stack is expected:

                           Entry                 Exit

                    Ptr ->
                           +------------+        +-----------+
                           | NameEnd    |        |           |
                           |------------|        |-----------|
                           | NameStart  |        |           | <- Ptr
                           |------------|        |-----------|
*)

PROCEDURE P1EndBuildProgramModule ;
VAR
   start,
   end      : CARDINAL ;
   NameStart,
   NameEnd  : Name ;
BEGIN
   ResolveImports ;
   Assert(CompilingProgramModule()) ;
   EndScope ;
   PopTtok(NameStart, start) ;
   PopTtok(NameEnd, end) ;
   IF Debugging
   THEN
      printf0('pass 1: ') ;
      DisplayTrees(GetCurrentModule())
   END ;
   IF NameStart#NameEnd
   THEN
      MetaErrorT1 (end,
                   'inconsistant program module name {%1Wa}', MakeError (start, NameStart))
   END ;
   LeaveBlock
END P1EndBuildProgramModule ;


(*
   StartBuildInnerModule - Creates an Inner module and starts
                           a new scope.

                           The Stack is expected:

                           Entry                 Exit

                    Ptr ->                                     <- Ptr
                           +------------+        +-----------+
                           | NameStart  |        | NameStart |
                           |------------|        |-----------|

*)

PROCEDURE StartBuildInnerModule ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopTtok(name, tok) ;
   ModuleSym := GetSym(name) ;
   Assert(ModuleSym#NulSym) ;
   StartScope(ModuleSym) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushTtok(name, tok) ;
   EnterBlock(name)
END StartBuildInnerModule ;


(*
   EndBuildInnerModule - Destroys the Inner module scope and
                         checks for correct name.

                         The Stack is expected:

                         Entry                 Exit

                  Ptr ->
                         +------------+        +-----------+
                         | NameEnd    |        |           |
                         |------------|        |-----------|
                         | NameStart  |        |           | <- Ptr
                         |------------|        |-----------|
*)

PROCEDURE EndBuildInnerModule ;
VAR
   start, end: CARDINAL ;
   NameStart,
   NameEnd   : Name ;
BEGIN
   EndScope ;
   PopTtok(NameStart, start) ;
   PopTtok(NameEnd, end) ;
   IF NameStart#NameEnd
   THEN
      MetaErrorT1 (end,
                   'inconsistant inner module name {%1Wa}', MakeError (start, NameStart))
   END ;
   LeaveBlock
END EndBuildInnerModule ;


(*
   BuildImportOuterModule - Builds imported identifiers into an outer module
                            from a definition module.

                            The Stack is expected:

                            Entry           OR    Entry

                     Ptr ->                Ptr ->
                            +------------+        +-----------+
                            | #          |        | #         |
                            |------------|        |-----------|
                            | Id1        |        | Id1       |
                            |------------|        |-----------|
                            .            .        .           .
                            .            .        .           .
                            .            .        .           .
                            |------------|        |-----------|
                            | Id#        |        | Id#       |
                            |------------|        |-----------|
                            | ImportTok  |        | Ident     |
                            |------------|        |-----------|

                            IMPORT Id1, .. Id# ;  FROM Ident IMPORT Id1 .. Id# ;


                            Exit

                            All above stack discarded
*)

PROCEDURE BuildImportOuterModule (definition: BOOLEAN) ;
VAR
   Sym, ModSym,
   i, n       : CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   IF OperandT(n+1)=ImportTok
   THEN
      (* Ident list contains Module Names *)
      i := 1 ;
      WHILE i<=n DO
         ModSym := LookupModule(OperandTok(n+1-i),
                                OperandT(n+1-i)) ;
         PutImported(ModSym) ;
         IF definition
         THEN
            PutIncludedByDefinition(ModSym)
         END ;
         INC(i)
      END
   ELSE
      (* Ident List contains list of objects *)
      ModSym := LookupModule(OperandTok(n+1),
                             OperandT(n+1)) ;
      i := 1 ;
      WHILE i<=n DO
(*
         WriteString('Importing ') ; WriteKey(Operand(j)) ; WriteString(' from ') ; WriteKey(GetSymName(ModSym)) ; WriteLn ;
*)
         Sym := GetExported (OperandTok (n+1-i),
                             ModSym, OperandT (n+1-i)) ;
         PutImported (Sym) ;
         INC (i)
      END
   END ;
   PopN (n+1)   (* clear stack *)
END BuildImportOuterModule ;


(*
   BuildExportOuterModule - Builds exported identifiers from an outer module
                            to the outside world of library modules.

                            The Stack is expected:

                            Entry           OR    Entry

                     Ptr ->                Ptr ->
                            +------------+        +--------------+
                            | #          |        | #            |
                            |------------|        |--------------|
                            | Id1        |        | Id1          |
                            |------------|        |--------------|
                            .            .        .              .
                            .            .        .              .
                            .            .        .              .
                            |------------|        |--------------|
                            | Id#        |        | Id#          |
                            |------------|        |--------------|
                            | ExportTok  |        | QualifiedTok |
                            |------------|        |--------------|

                            EXPORT Id1, .. Id# ;  EXPORT QUALIFIED Id1 .. Id# ;

                            Error Condition


                            Exit

                            All above stack discarded
*)

PROCEDURE BuildExportOuterModule ;
VAR
   i, n: CARDINAL ;
BEGIN
   PopT (n) ;       (* n   = # of the Ident List *)
   IF (OperandT(n+1)=QualifiedTok) AND CompilingDefinitionModule()
   THEN
      PutDoesNeedExportList(GetCurrentModule()) ;
      (* Ident List contains list of export qualified objects *)
      i := 1 ;
      WHILE i<=n DO
         PutExportQualified (OperandTok (i), OperandT (i)) ;
         INC (i)
      END
   ELSIF (OperandT(n+1)=UnQualifiedTok) AND CompilingDefinitionModule()
   THEN
      PutDoesNeedExportList(GetCurrentModule()) ;
      (* Ident List contains list of export unqualified objects *)
      i := 1 ;
      WHILE i<=n DO
         PutExportUnQualified (OperandTok (i), OperandT(i)) ;
         INC (i)
      END
   ELSIF CompilingDefinitionModule()
   THEN
      MetaError0 ('the {%EkEXPORT} must be either {%kQUALIFIED} or {%kUNQUALIFIED} in a definition module')
   ELSE
      MetaError0 ('{%E}only allowed inter module exports in a definition module')
   END ;
   PopN (n+1)  (* clear stack *)
END BuildExportOuterModule ;


(*
   CheckExplicitExported - checks to see whether we are compiling
                           a definition module and whether the ident
                           is implicitly export qualified or unqualified.


                                  The Stack is expected:

                                  Entry                 Exit

                           Ptr ->                Ptr ->
                                  +------------+        +-----------+
                                  | Identname  |        | Identname |
                                  |------------|        |-----------|

*)

PROCEDURE CheckExplicitExported ;
BEGIN
   IF CompilingDefinitionModule() AND DoesNotNeedExportList(GetCurrentModule())
   THEN
      (* printf1('exporting identifier %a\n', OperandT(1)) ; *)
      PutExportQualified (OperandTok (1), OperandT(1))
   END
END CheckExplicitExported ;


(*
   BuildImportInnerModule - Builds imported identifiers into an inner module
                            from the last level of module.

                            The Stack is expected:

                            Entry           OR    Entry

                     Ptr ->                Ptr ->
                            +------------+        +-----------+
                            | #          |        | #         |
                            |------------|        |-----------|
                            | Id1        |        | Id1       |
                            |------------|        |-----------|
                            .            .        .           .
                            .            .        .           .
                            .            .        .           .
                            |------------|        |-----------|
                            | Id#        |        | Id#       |
                            |------------|        |-----------|
                            | ImportTok  |        | Ident     |
                            |------------|        |-----------|

                            IMPORT Id1, .. Id# ;  FROM Ident IMPORT Id1 .. Id# ;

                            Exit

                            All above stack discarded
*)

PROCEDURE BuildImportInnerModule ;
VAR
   Sym, ModSym,
   i, n       : CARDINAL ;
BEGIN
   PopT (n) ;       (* n   = # of the Ident List *)
   IF OperandT (n+1) = ImportTok
   THEN
      (* Ident List contains list of objects *)
      i := 1 ;
      WHILE i<=n DO
         AddNameToImportList (OperandT (i)) ;
         INC (i)
      END
   ELSE
      (* Ident List contains list of objects *)
      ModSym := LookupOuterModule (OperandTok(n+1),
                                   OperandT(n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetExported (OperandTok (n+1-i), ModSym, OperandT (n+1-i)) ;
         PutImported (Sym) ;
         INC (i)
      END
   END ;
   PopN (n+1)    (* clear stack *)
END BuildImportInnerModule ;


(*
   BuildExportInnerModule - Builds exported identifiers from an inner module
                            to the next layer module.

                            The Stack is expected:

                            Entry           OR    Entry

                     Ptr ->                Ptr ->
                            +------------+        +--------------+
                            | #          |        | #            |
                            |------------|        |--------------|
                            | Id1        |        | Id1          |
                            |------------|        |--------------|
                            .            .        .              .
                            .            .        .              .
                            .            .        .              .
                            |------------|        |--------------|
                            | Id#        |        | Id#          |
                            |------------|        |--------------|
                            | ExportTok  |        | QualifiedTok |
                            |------------|        |--------------|

                            EXPORT Id1, .. Id# ;  EXPORT QUALIFIED Id1 .. Id# ;


                            Exit


                            All above stack discarded
*)

PROCEDURE BuildExportInnerModule ;
VAR
   tok    : CARDINAL ;
   PrevMod,
   Sym,
   i, n   : CARDINAL ;
BEGIN
   PopT (n) ;       (* n   = # of the Ident List *)
   IF OperandT (n+1) = ExportTok
   THEN
      (* Ident List contains list of objects *)
      i := 1 ;
      PrevMod := GetScope (GetCurrentScope ()) ;
      WHILE i<=n DO
         tok := OperandTok (i) ;
         IF (PrevMod#NulSym) AND (IsModule(PrevMod) OR IsDefImp(PrevMod))
         THEN
            Sym := GetLocalSym (PrevMod, OperandT(i)) ;
            IF Sym=NulSym
            THEN
               Sym := TryMoveUndeclaredSymToInnerModule (PrevMod, GetCurrentScope (), OperandT (i)) ;
               IF Sym=NulSym
               THEN
                  Sym := RequestSym (tok, OperandT(i)) ;
                  PutExported (Sym)
               END
            ELSE
               (* use Sym which has already been created in outer scope *)
               AddSymToModuleScope (GetCurrentScope (), Sym)
            END
         ELSE
            Sym := RequestSym (tok, OperandT(i)) ;
            PutExported (Sym)
         END ;
         INC (i)
      END
   ELSE
      MetaError0 ('{%EkQUALIFIED} not allowed in an inner module')
   END ;
   PopN(n+1)    (* clear stack *)
END BuildExportInnerModule ;


(*
   StartBuildEnumeration - Builds an Enumeration type Type.


                           Stack

                           Entry                 Exit

                    Ptr ->
                           +------------+
                           | #          |
                           |------------|
                           | en 1       |
                           |------------|
                           | en 2       |
                           |------------|
                           .            .
                           .            .
                           .            .                       <- Ptr
                           |------------|        +------------+
                           | en #       |        | Type       |
                           |------------|        |------------|
                           | Name       |        | Name       |
                           |------------|        |------------|
*)

PROCEDURE StartBuildEnumeration ;
VAR
   name : Name ;
   n, i,
   Type : CARDINAL ;
   tokno: CARDINAL ;
BEGIN
   PopT(n) ;      (* No := # *)
   name := OperandT(n+1) ;
   tokno := OperandTok(n+1) ;
   Type := MakeEnumeration(tokno, name) ;
   i := 1 ;
   WHILE i<=n DO
      PutFieldEnumeration(OperandTok(n-i+1), Type, OperandT(n-i+1)) ;
      INC(i)
   END ;
   PutEnumerationIntoFifoQueue(Type) ;  (* store enumeration away for pass 2 *)
   PopN(n+1) ;
   PushTtok(name, tokno) ;
   PushTtok(Type, tokno)
END StartBuildEnumeration ;


(*
   EndBuildEnumeration - completes the construction of the enumeration type.


                         Stack

                         Entry                 Exit

                  Ptr ->
                         +------------+
                         | Type       |                          <- Ptr
                         |------------|        +---------------+
                         | Name       |        | Type  | Name  |
                         |------------|        |---------------|

                                               Empty
*)

PROCEDURE EndBuildEnumeration ;
VAR
   tokno : CARDINAL ;
   Sym,
   Type  : CARDINAL ;
   n1, n2,
   name  : Name ;
BEGIN
   (*
      Two cases

      - the type name the same as Name, or the name is nul. - do nothing.
      - when type with a name that is different to Name. In which case
        we create a new type.
   *)
   PopTtok(Type, tokno) ;
   PopT(name) ;

   IF Debugging
   THEN
      n1 := GetSymName(GetCurrentModule()) ;
      printf2('inside module %a declaring type name %a\n',
              n1, name) ;
      IF (NOT IsUnknown(Type))
      THEN
         n1 := GetSymName(GetScope(Type)) ;
         n2 := GetSymName(Type) ;
         printf2('type was created inside scope %a as name %a\n',
                 n1, n2)
      END
   END ;
   IF (name=NulName) OR (GetSymName(Type)=name)
   THEN
      (*
         Typically the declaration that causes this case is:

         VAR
            a: (blue, green, red) ;
             ^
             |
             +---- type has no name.

         in which case the constructed from StartBuildEnumeration is complete
      *)
      PushTFtok(Type, name, tokno)
   ELSE
      (* in this case we are seeing:

         TYPE
            name = (blue, green, red)

         so we construct the type name and define it to have the previously
         created enumeration type
      *)
      Sym := MakeType(tokno, name) ;
      PutType(Sym, Type) ;
      PushTFtok(Sym, name, tokno)
   END
END EndBuildEnumeration ;


(*
   BuildHiddenType - Builds a Hidden Type.


                     Stack

                     Entry                 Exit

              Ptr ->
                     +------------+
                     | Name       |                          <- Ptr
                     |------------|        Empty
*)

PROCEDURE BuildHiddenType ;
VAR
   name : Name ;
   tokno: CARDINAL ;
BEGIN
   PopTtok (name, tokno) ;
   (* WriteString('Hidden type encountered: ') ; *)
   (* WriteKey(Name) ; WriteLn ; *)
   Assert (MakeHiddenType (tokno, name) # NulSym)
END BuildHiddenType ;


(*
   StartBuildProcedure - Builds a Procedure.

                         The Stack:

                         Entry                 Exit

                  Ptr ->                                      <- Ptr
                         +------------+        +------------+
                         | Name       |        | ProcSym    |
                         |------------|        |------------|
                         | inlinetok  |        |            |
                         | or         |        |            |
                         | builtintok |        |            |
                         | or name or |        | Name       |
                         | NulTok     |        |            |
                         |------------|        |------------|
*)

PROCEDURE StartBuildProcedure ;
VAR
   tokno   : CARDINAL ;
   builtin,
   name    : Name ;
   ProcSym : CARDINAL ;
BEGIN
   PopTtok (name, tokno) ;
   PopT (builtin) ; (* was this procedure defined as a builtin?        *)
   PushTtok (name, tokno) ;   (* Name saved for the EndBuildProcedure name check *)
   ProcSym := RequestSym (tokno, name) ;
   IF IsUnknown (ProcSym)
   THEN
      (*
         May have been compiled in DEF or IMP module, remember that IMP maybe
         compiled before corresponding DEF module.
      *)
      ProcSym := MakeProcedure (tokno, name)
   ELSIF IsProcedure (ProcSym)
   THEN
      (* declared in the other module, we record declaration here as well *)
      PutDeclared (tokno, ProcSym)
   ELSE
      MetaError1 ('expecting a procedure name and symbol {%1Ea} has been declared as a {%1d}', ProcSym) ;
      PushT (ProcSym) ;
      RETURN
   END ;
   IF builtin#NulTok
   THEN
      IF builtin=BuiltinTok
      THEN
         PutProcedureBuiltin (ProcSym, name)
      ELSIF builtin=InlineTok
      THEN
         PutProcedureInline (ProcSym)
      ELSE
         PutProcedureBuiltin (ProcSym, builtin)
      END
   END ;
   PushT (ProcSym) ;
   StartScope (ProcSym) ;
   IF NOT CompilingDefinitionModule ()
   THEN
      EnterBlock (name)
   END
END StartBuildProcedure ;


(*
   EndBuildProcedure - Ends building a Procedure.
                       It checks the start procedure name matches the end
                       procedure name.

                       The Stack:

                       (Procedure Not Defined in definition module)

                       Entry                 Exit

                Ptr ->
                       +------------+
                       | NameEnd    |
                       |------------|
                       | ProcSym    |
                       |------------|
                       | NameStart  |
                       |------------|
                                             Empty
*)

PROCEDURE EndBuildProcedure ;
VAR
   start, end: CARDINAL ;
   ProcSym   : CARDINAL ;
   NameEnd,
   NameStart : Name ;
BEGIN
   PopTtok(NameEnd, end) ;
   PopT(ProcSym) ;
   PopTtok(NameStart, start) ;
   IF NameEnd#NameStart
   THEN
      IF end # UnknownTokenNo
      THEN
         MetaErrorT1 (end,
                      'procedure name at end does not match name at beginning {%1EDa}', ProcSym)
      ELSIF start # UnknownTokenNo
      THEN
         MetaErrorT2 (start,
                      'procedure name at end {%1EDa} does not match name at beginning {%2a}',
                      MakeError (end, NameEnd), ProcSym)
      ELSE
         MetaError1 ('procedure name at end does not match name at beginning {%1EDa}', ProcSym)
      END
   END ;
   EndScope ;
   Assert (NOT CompilingDefinitionModule()) ;
   LeaveBlock
END EndBuildProcedure ;


(*
   BuildProcedureHeading - Builds a procedure heading for the definition
                           module procedures.

                           Operation only performed if compiling a
                           definition module.

                           The Stack:

                           Entry                       Exit

                    Ptr ->
                           +------------+
                           | ProcSym    |
                           |------------|
                           | NameStart  |
                           |------------|
                                                       Empty

*)

PROCEDURE BuildProcedureHeading ;
VAR
   ProcSym  : CARDINAL ;
   NameStart: Name ;
BEGIN
   IF CompilingDefinitionModule()
   THEN
      PopT(ProcSym) ;
      PopT(NameStart) ;
      EndScope
   END
END BuildProcedureHeading ;


(*
   BuildNulName - Pushes a NulName onto the top of the stack.
                  The Stack:


                  Entry                    Exit

                                                          <- Ptr
                  Empty                    +------------+
                                           | NulName    |
                                           |------------|
*)

PROCEDURE BuildNulName ;
BEGIN
   PushT(NulName)
END BuildNulName ;


(*
   BuildTypeEnd - Pops the type Type and Name.
                  The Stack:


                  Entry                    Exit


           Ptr ->
                  +-------------+
                  | Type | Name |          Empty
                  |-------------|
*)

PROCEDURE BuildTypeEnd ;
VAR
   Type: CARDINAL ;
   name: Name ;
BEGIN
   PopTF (Type, name)
END BuildTypeEnd ;


(*
   BuildImportStatement - create a new import statement in the current module.
                          It ignores local modules.

                          The quadruple stack is not used.
*)

PROCEDURE BuildImportStatement (tok: CARDINAL) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetCurrentScope () ;
   IF IsDefImp (scope) OR (IsModule (scope) AND (NOT IsInnerModule (scope)))
   THEN
      IF CompilingDefinitionModule () AND (NOT IsDefImp (scope))
      THEN
         MetaError1 ('module scope should be a definition module rather than {%1EDa}', scope)
      ELSE
         INC (importStatementCount) ;
         AppendModuleImportStatement (scope, MakeImportStatement (tok, importStatementCount))
      END
   END
END BuildImportStatement ;


(*
   AddImportToImportStatement - the top of stack is expected to be a module name.
                                This is looked up from the module universe and
                                wrapped in an import symbol and placed into the
                                current import statement.

                                The quadruple stack is unchanged.

                                Entry                      Exit


                         Ptr ->                                                   <- Ptr
                                +---------------------+    +---------------------+
                                | ImportedModuleName  |    | ImportedModuleName  |
                                |---------------------|    |---------------------|
*)

PROCEDURE AddImportToImportStatement (qualified: BOOLEAN) ;
VAR
   scope: CARDINAL ;
BEGIN
   scope := GetCurrentScope () ;
   IF IsDefImp (scope) OR (IsModule (scope) AND (NOT IsInnerModule (scope)))
   THEN
      IF CompilingDefinitionModule () AND (NOT IsDefImp (scope))
      THEN
         MetaError1 ('module scope should be a definition module rather than {%1EDa}', scope) ;
      ELSE
         AppendModuleOnImportStatement (scope, MakeImport (OperandTok (1),
                                                           LookupModule (OperandTok (1), OperandT (1)),
                                                           importStatementCount, qualified))
      END
   END
END AddImportToImportStatement ;


END P1SymBuild.
