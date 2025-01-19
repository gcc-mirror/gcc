(* P2SymBuild.mod pass 2 symbol creation.

Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE P2SymBuild ;


FROM libc IMPORT strlen ;
FROM NameKey IMPORT Name, MakeKey, makekey, KeyToCharStar, NulName, LengthKey, WriteKey ;
FROM StrLib IMPORT StrEqual ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2LexBuf IMPORT UnknownTokenNo, GetTokenNo, MakeVirtual2Tok ;
FROM M2Error IMPORT InternalError, WriteFormat1, WriteFormat2, WriteFormat0, ErrorStringAt, ErrorStringAt2, WarnStringAt ;
FROM DynamicStrings IMPORT String, InitString, InitStringCharStar, Mark, Slice, ConCat, KillString, string ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3, Sprintf4 ;
FROM M2Printf IMPORT printf0, printf1, printf2, printf3 ;
FROM M2StackWord IMPORT StackOfWord, InitStackWord, PushWord, PopWord ;
FROM M2Options IMPORT PedanticParamNames, ExtendedOpaque ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM M2Base IMPORT ZType ;
FROM Storage IMPORT ALLOCATE ;
FROM gcctypes IMPORT location_t ;
FROM M2LexBuf IMPORT TokenToLocation ;

FROM M2Reserved IMPORT ImportTok, ExportTok, QualifiedTok, UnQualifiedTok,
                       NulTok, VarTok, ArrayTok ;

FROM M2MetaError IMPORT MetaError1, MetaError2, MetaError3,
                        MetaErrorsT2, MetaErrors1, MetaErrorT1,
                        MetaErrors2, MetaErrorString1, MetaErrorStringT1,
                        MetaErrorString3, MetaErrorStringT3 ;

FROM FifoQueue IMPORT GetEnumerationFromFifoQueue, PutSubrangeIntoFifoQueue,
                      PutConstructorIntoFifoQueue, PutConstIntoFifoQueue ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr, ProcedureKind,
                        StartScope, EndScope, PseudoScope,
                        GetCurrentScope, GetScope,
                        IsDeclaredIn,
                        SetCurrentModule, SetFileModule,
                        GetCurrentModule, GetMainModule,
                        MakeTemporary, CheckAnonymous, IsNameAnonymous,
                        MakeConstLit,
                        MakeConstString,
                        MakeSubrange,
                        MakeVar, MakeType, PutType,
                        MakeModuleCtor,
                        PutMode, PutDeclared, GetParameterShadowVar,
                        PutFieldEnumeration, PutSubrange, PutVar, PutVarTok, PutConst,
                        PutConstSet, PutConstructor,
                        IsDefImp, IsType, IsRecord, IsRecordField, IsPointer,
                        IsSubrange, IsEnumeration, IsConstString,
                        IsError, IsAModula2Type, IsParameterVar, IsParameterUnbounded,
                        GetSym, GetDeclareSym, IsUnknown, RenameSym,
                        GetLocalSym, GetParent, IsRecord, GetRecord,
                        GetFromOuterModule,
                        GetExported,
                        PutExported, PutExportQualified, PutExportUnQualified,
                        PutExportUnImplemented,
                        PutFieldVarient, PutVarientTag,
                        IsFieldVarient, IsVarient,
                        CheckForEnumerationInCurrentModule,
                        CheckForExportedImplementation,
                        MakeProcedure,
                        PutFunction, PutOptFunction,
                        PutParam, PutVarParam,
                        GetNthParam,
                        IsProcedure,
                        NoOfElements,
                        MakePointer, PutPointer,
      	          	MakeSet, PutSet,
                        MakeRecord, PutFieldRecord,
                        MakeVarient, MakeFieldVarient,
                        MakeArray, PutArraySubscript,
                        MakeSubscript, PutSubscript,
                        MakeError,
                        PutConstStringKnown, GetString,
                        PutArray, IsArray,
                        GetType, SkipType,
                        IsProcType, MakeProcType,
                        PutProcTypeVarParam, PutProcTypeParam,
                        MakeConstVar,
                        PutVariableAtAddress, IsVariableAtAddress,
                        GetAlignment, PutAlignment,
                        PutDefaultRecordFieldAlignment,
                        GetDefaultRecordFieldAlignment,
                        PutUnused,
                        MakeUnbounded, IsUnbounded,
                        NoOfParam,
                        PutParamName,
                        GetParam, GetDimension,
                        PutProcedureParametersDefined,
                        GetProcedureParametersDefined,
                        PutProcedureParameterHeapVars,
                        CheckForUnImplementedExports,
                        CheckForUndeclaredExports,
                        IsHiddenTypeDeclared,
                        IsUnboundedParam,
                        IsVarParam,
                        PutUseVarArgs,
                        UsesVarArgs,
                        PutUseOptArg,
                        UsesOptArg,
                        IsDefinitionForC,
                        GetSymName,
                        GetDeclaredDef, GetDeclaredMod,
                        RequestSym,
                        PutDeclared,
                        GetPackedEquivalent,
                        GetVarDeclTok,
                        PutVarDeclTok,
                        GetVarDeclTypeTok,
                        GetProcedureKindDesc,
                        GetProcedureDeclaredTok,
                        GetProcedureKind,
                        GetReturnTypeTok,
                        SetReturnOptional,
                        IsReturnOptional,
                        PutProcedureNoReturn,
                        PutProcedureDefined,
                        DisplayTrees ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource,
                    LookupModule, LookupOuterModule ;

FROM M2Quads IMPORT PushT, PopT,
                    PushTF, PopTF, PopTtok, PushTFtok, PushTtok, PopTFtok,
                    OperandT, OperandF, OperandA, OperandTok, PopN, DisplayStack, Annotate,
                    AddVarientFieldToList, Top ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

FROM M2Const IMPORT constType ;
FROM M2Students IMPORT CheckVariableAgainstKeyword ;
IMPORT M2Error ;


CONST
   Debugging = FALSE ;

VAR
   alignTypeNo       : CARDINAL ;
   castType          : CARDINAL ;
   type              : constType ;
   RememberedConstant: CARDINAL ;
   RememberStack,
   TypeStack         : StackOfWord ;
   curModuleSym      : CARDINAL ;
   curBeginTok,
   curFinallyTok,
   curStartTok,
   curEndTok         : CARDINAL ;
   BlockStack        : StackOfWord ;


PROCEDURE stop ; BEGIN END stop ;


(*
   Debug - call stop if symbol name is name.
*)

PROCEDURE Debug (tok: CARDINAL; sym: CARDINAL; name: ARRAY OF CHAR) ;
BEGIN
   IF MakeKey (name) = GetSymName (sym)
   THEN
      stop
   END ;
   MetaErrorT1 (tok, 'procedure {%1Wa}', sym)
END Debug ;


(*
   BlockStart - tokno is the module/procedure/implementation/definition token
*)

PROCEDURE BlockStart (tokno: CARDINAL) ;
BEGIN
   PushBlock (tokno) ;
END BlockStart ;


(*
   propageteTokenPosition - if laterTokPos is unknown then return knownTokPos.
                            else return laterTokPos.
*)

PROCEDURE propageteTokenPosition (knownTokPos, laterTokPos: CARDINAL) : CARDINAL ;
BEGIN
   IF laterTokPos = UnknownTokenNo
   THEN
      RETURN knownTokPos
   ELSE
      RETURN laterTokPos
   END
END propageteTokenPosition ;


(*
   BlockEnd - declare module ctor/init/fini/dep procedures.
*)

PROCEDURE BlockEnd (tokno: CARDINAL) ;
BEGIN
   curBeginTok := propageteTokenPosition (curStartTok, curBeginTok) ;
   curFinallyTok := propageteTokenPosition (tokno, curFinallyTok) ;
   Assert (curModuleSym # NulSym) ;
   MakeModuleCtor (curStartTok, curBeginTok, curFinallyTok,
                   curModuleSym) ;
   PopBlock
END BlockEnd ;


(*
   BlockBegin - assign curBeginTok to tokno.
*)

PROCEDURE BlockBegin (tokno: CARDINAL) ;
BEGIN
   curBeginTok := tokno
END BlockBegin ;


(*
   BlockFinally - assign curFinallyTok to tokno.
*)

PROCEDURE BlockFinally (tokno: CARDINAL) ;
BEGIN
   curFinallyTok := tokno
END BlockFinally ;


(*
   PushBlock - push the block variables to the block stack.
*)

PROCEDURE PushBlock (tokno: CARDINAL) ;
BEGIN
   PushWord (BlockStack, curStartTok) ;   (* module/implementation/definition/procedure token pos.  *)
   PushWord (BlockStack, curBeginTok) ;   (* BEGIN keyword pos.  *)
   PushWord (BlockStack, curEndTok) ;     (* END keyword pos.  *)
   PushWord (BlockStack, curFinallyTok) ; (* FINALLY keyword pos.  *)
   PushWord (BlockStack, curModuleSym) ;  (* current module.  *)
   curStartTok := tokno ;
   curBeginTok := UnknownTokenNo ;
   curEndTok := UnknownTokenNo ;
   curFinallyTok := UnknownTokenNo ;
   curModuleSym := NulSym
END PushBlock ;


(*
   PopBlock - pop the block variables from the block stack.
*)

PROCEDURE PopBlock ;
BEGIN
   curModuleSym := PopWord (BlockStack) ;
   curFinallyTok := PopWord (BlockStack) ;
   curEndTok := PopWord (BlockStack) ;
   curBeginTok := PopWord (BlockStack) ;
   curStartTok := PopWord (BlockStack)
END PopBlock ;


(*
   StartBuildDefinitionModule - Creates a definition module and starts
                                a new scope.

                                he Stack is expected:

                                Entry                 Exit

                         Ptr ->                                     <- Ptr
                                +------------+        +-----------+
                                | NameStart  |        | NameStart |
                                |------------|        |-----------|

*)

PROCEDURE P2StartBuildDefModule ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
   tokno    : CARDINAL ;
BEGIN
   PopTtok (name, tokno) ;
   ModuleSym := MakeDefinitionSource(tokno, name) ;
   curModuleSym := ModuleSym ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingDefinitionModule()) ;
   PushT(name) ;
   Annotate("%1n||definition module name") ;
   M2Error.EnterDefinitionScope (name)
END P2StartBuildDefModule ;


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

PROCEDURE P2EndBuildDefModule ;
VAR
   NameStart,
   NameEnd  : Name ;
BEGIN
   Assert(CompilingDefinitionModule()) ;
   CheckForUndeclaredExports(GetCurrentModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF Debugging
   THEN
      printf0('pass 2: ') ;
      DisplayTrees(GetCurrentModule())
   END ;
   IF NameStart#NameEnd
   THEN
      WriteFormat2('inconsistant definition module name, module began as (%a) and ended with (%a)', NameStart, NameEnd)
   END ;
   M2Error.LeaveErrorScope
END P2EndBuildDefModule ;


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

PROCEDURE P2StartBuildImplementationModule ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
   tokno    : CARDINAL ;
BEGIN
   PopTtok (name, tokno) ;
   ModuleSym := MakeImplementationSource(tokno, name) ;
   curModuleSym := ModuleSym ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingImplementationModule()) ;
   PushT(name) ;
   Annotate("%1n||implementation module name") ;
   M2Error.EnterImplementationScope (name)
END P2StartBuildImplementationModule ;


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

PROCEDURE P2EndBuildImplementationModule ;
VAR
   NameStart,
   NameEnd  : Name ;
BEGIN
   Assert(CompilingImplementationModule()) ;
   CheckForUnImplementedExports ;
   EndScope ;
   PopT (NameStart) ;
   PopT (NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteFormat1('inconsistant implementation module name %a', NameStart)
   END ;
   M2Error.LeaveErrorScope
END P2EndBuildImplementationModule ;


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

PROCEDURE P2StartBuildProgramModule ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
   tokno    : CARDINAL ;
BEGIN
   PopTtok (name, tokno) ;
   ModuleSym := MakeProgramSource(tokno, name) ;
   curModuleSym := ModuleSym ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(CompilingProgramModule()) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushT(name) ;
   Annotate("%1n||program module name") ;
   M2Error.EnterProgramScope (name)
END P2StartBuildProgramModule ;


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

PROCEDURE P2EndBuildProgramModule ;
VAR
   NameStart,
   NameEnd  : Name ;
BEGIN
   Assert(CompilingProgramModule()) ;
   CheckForUndeclaredExports(GetCurrentModule()) ;  (* Not really allowed exports here though! *)
   EndScope ;
   PopT (NameStart) ;
   PopT (NameEnd) ;
   IF Debugging
   THEN
      printf0('pass 2: ') ;
      DisplayTrees(GetCurrentModule())
   END ;
   IF NameStart#NameEnd
   THEN
      WriteFormat2('inconsistant program module name %a does not match %a', NameStart, NameEnd)
   END ;
   M2Error.LeaveErrorScope
END P2EndBuildProgramModule ;


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
   name     : Name ;
   tok      : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopTtok (name, tok) ;
   ModuleSym := GetDeclareSym (tok, name) ;
   curModuleSym := ModuleSym ;
   StartScope (ModuleSym) ;
   Assert(NOT IsDefImp (ModuleSym)) ;
   PushTtok (name, tok) ;
   Annotate ("%1n||inner module name") ;
   M2Error.EnterModuleScope (name)
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
   NameStart,
   NameEnd  : Name ;
BEGIN
   CheckForUndeclaredExports(GetCurrentModule()) ;
   EndScope ;
   PopT(NameStart) ;
   PopT(NameEnd) ;
   IF NameStart#NameEnd
   THEN
      WriteFormat2('inconsistant inner module name %a does not match %a',
                   NameStart, NameEnd)
   END ;
   M2Error.LeaveErrorScope
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

PROCEDURE BuildImportOuterModule ;
VAR
   Sym, ModSym,
   i, n       : CARDINAL ;
BEGIN
   PopT (n) ;       (* n   = # of the Ident List *)
   IF OperandT (n+1) # ImportTok
   THEN
      (* Ident List contains list of objects imported from ModSym *)
      ModSym := LookupModule (OperandTok(n+1), OperandT (n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetExported (OperandTok(i), ModSym, OperandT (i)) ;
         CheckForEnumerationInCurrentModule (Sym) ;
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
   n: CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   PopN(n+1)
END BuildExportOuterModule ;


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
   n, i       : CARDINAL ;
BEGIN
   PopT (n) ;       (* i   = # of the Ident List *)
   IF OperandT(n+1)=ImportTok
   THEN
      (* Ident List contains list of objects *)
      i := 1 ;
      WHILE i<=n DO
         Sym := GetFromOuterModule (OperandTok (i), OperandT (i)) ;
         CheckForEnumerationInCurrentModule (Sym) ;
         INC (i)
      END
   ELSE
      (* Ident List contains list of objects from ModSym *)
      ModSym := LookupOuterModule(OperandTok(n+1), OperandT(n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetExported (OperandTok (i), ModSym, OperandT(i)) ;
         CheckForEnumerationInCurrentModule (Sym) ;
         INC  (i)
      END
   END ;
   PopN (n+1)   (* Clear Stack *)
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
   n: CARDINAL ;
BEGIN
   PopT(n) ;
   PopN(n+1)   (* clear stack               *)
END BuildExportInnerModule ;


(*
   BuildNumber - Converts a number into a symbol.


                 Stack

                 Entry                 Exit

          Ptr ->                                             <- Ptr
                 +------------+        +-------------------+
                 | Name | tok |        | Sym | Type | tok  |
                 |------------+        |-------------------|
*)

PROCEDURE BuildNumber ;
VAR
   name: Name ;
   Sym : CARDINAL ;
   tok : CARDINAL ;
BEGIN
   PopTtok (name, tok) ;
   Sym := MakeConstLit (tok, name, NulSym) ;
   PushTFtok (Sym, GetType (Sym), tok) ;
   Annotate ("%1s(%1d)||constant number")
END BuildNumber ;


(*
   BuildString - Converts a string into a symbol.


                 Stack

                 Entry                 Exit

          Ptr ->                                               <- Ptr
                 +-------------+        +--------------------+
                 | Name | | tok|        | Sym | NulSym | tok |
                 |-------------+        |--------------------|
*)

PROCEDURE BuildString ;
VAR
   name: Name ;
   Sym : CARDINAL ;
   tok : CARDINAL ;
BEGIN
   PopTtok (name, tok) ;
   (* slice off the leading and trailing quotes *)
   IF name = 1140
   THEN
      stop
   END ;
   Sym := MakeConstString (tok, makekey (string (Mark (Slice (Mark (InitStringCharStar (KeyToCharStar (name))), 1, -1))))) ;
   PushTFtok (Sym, NulSym, tok) ;
   Annotate ("%1s(%1d)|%3d||constant string")
END BuildString ;


(*
   BuildConst - builds a constant.
                Stack

                Entry                 Exit

         Ptr ->
                +------------+
                | Name       |
                |------------+                       <- Ptr
*)

PROCEDURE BuildConst ;
VAR
   name: Name ;
   sym : CARDINAL ;
   tok : CARDINAL ;
BEGIN
   PopTtok(name, tok) ;
   sym := MakeConstVar(tok, name) ;
   PushTtok(sym, tok) ;
   RememberConstant(sym) ;
   Annotate("%1s(%1d)|%3d||remembered constant")
END BuildConst ;


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
   n,
   Type: CARDINAL ;
   tok : CARDINAL ;
BEGIN
   PopT (n) ;       (* n := # *)
   (* name is in OperandT(n+1) but we dont need it here.  *)
   tok := OperandTok (n+1) ;
   GetEnumerationFromFifoQueue (Type) ;
   CheckForExportedImplementation (Type) ;   (* May be an exported hidden type *)
   PopN (n) ;
   PushTtok (Type, tok) ;
   Annotate ("%1s(%1d)|%3d||enumerated type")
END StartBuildEnumeration ;


(*
   BuildSubrange - Builds a Subrange type Symbol, the base type can also be
                   supplied if known.

                      Stack

                      Entry                 Exit


                                                           <- Ptr
                                            +------------+
               Ptr ->                       | Type       |
                      +------------+        |------------|
                      | Name       |        | Name       |
                      |------------|        |------------|
*)

PROCEDURE BuildSubrange (tok: CARDINAL; Base: CARDINAL) ;
VAR
   name: Name ;
   Type: CARDINAL ;
BEGIN
   PopT (name) ;
   Type := MakeSubrange (tok, name) ;
   PutSubrangeIntoFifoQueue(Type) ;   (* Store Subrange away so that we can fill in *)
                                      (* its bounds during pass 3.                  *)
   PutSubrangeIntoFifoQueue(Base) ;   (* store Base type of subrange away as well.  *)
   CheckForExportedImplementation(Type) ; (* May be an exported hidden type *)
   PushTtok(name, tok) ;
   Annotate("%1n|%3d||subrange name|token no") ;
   PushTtok(Type, tok) ;
   Annotate("%1s(%1d)|%3d||subrange type|token no")
END BuildSubrange ;


(*
   BuildDefaultFieldAlignment -

                 The Stack:

                 Entry                         Exit
                 =====                         ====


          Ptr ->
                 +-----------+
                 | Alignment |
                 |-----------|                 +-----------+
                 | RecordSym |                 | RecordSym |
                 |-----------|                 |-----------|
                 | Name      |                 | Name      |
                 |-----------|                 |-----------|

*)

PROCEDURE P2BuildDefaultFieldAlignment ;
VAR
   tok      : CARDINAL ;
   alignment: Name ;
   align    : CARDINAL ;
BEGIN
   PopTtok(alignment, tok) ;
   align := MakeTemporary(tok, ImmediateValue) ;
   PutConst(align, ZType) ;
   PutConstIntoFifoQueue(align) ;     (* store align away ready for pass 3 *)
   PutDefaultRecordFieldAlignment(OperandT(1), align)
END P2BuildDefaultFieldAlignment ;


(*
   BuildPragmaConst - pushes a constant to the stack and stores it away into the
                      const fifo queue ready for pass 3.
*)

PROCEDURE BuildPragmaConst ;
VAR
   value : CARDINAL ;
BEGIN
   value := MakeTemporary(GetTokenNo (), ImmediateValue) ;
   PutConst(value, ZType) ;
   PutConstIntoFifoQueue(value) ;     (* Store value away so that we can fill it in   *)
   PushT(value) ;                     (* during pass 3.                               *)
   Annotate("%1s(%1d)||pragma constant")
END BuildPragmaConst ;


(*
   BuildAligned - builds an alignment constant symbol which is placed onto
                  the stack.  It expects the ident ALIGNED to be on the
                  stack.

                         Stack

                           Entry                 Exit


                  Ptr ->                                            <- Ptr
                         +---------------+      +-----------------+
                         | bytealignment |      | AlignmentConst  |
                         +---------------+      |-----------------|
*)

PROCEDURE BuildAligned ;
VAR
   tok  : CARDINAL ;
   name : Name ;
   align: CARDINAL ;
BEGIN
   PopTtok (name, tok) ;
   IF name=MakeKey('bytealignment')
   THEN
      align := MakeTemporary (tok, ImmediateValue) ;
      PutConst(align, ZType) ;
      PutConstIntoFifoQueue(align) ;     (* Store align away so that we can fill in its  *)
      PushT(align) ;                     (* value during pass 3.                         *)
      Annotate("%1s(%1d)|%3d||bytealignment constant generated from <* *>|token no") ;
      PushTtok(name, tok)
   ELSE
      WriteFormat1('expecting bytealignment identifier, rather than %a', name) ;
      PushT(NulSym)
   END ;
   Annotate("%1n(%1d)||bytealignment constant generated from <* *>")
END BuildAligned ;


(*
   BuildTypeAlignment - the AlignmentConst is either a temporary or NulSym.
                        In the case of NulSym it is popped from the stack
                        and the procedure returns.  Otherwise the temporary
                        is popped and recorded as the alignment value for this
                        type.  A type may only have one alignment value and
                        error checking is performed.

                        Stack

                            Entry                 Exit


                    Ptr ->
                           +-----------------+
                           | AlignmentConst  |
                           |-----------------|
                           | Type            |    Empty
                           |-----------------|
*)

PROCEDURE BuildTypeAlignment ;
VAR
   alignment: Name ;
   type,
   align    : CARDINAL ;
BEGIN
   PopT (alignment) ;
   IF alignment = MakeKey ('bytealignment')
   THEN
      PopT (align) ;
      PopT (type) ;
      IF align # NulSym
      THEN
         IF IsRecord (type) OR IsRecordField (type) OR IsType (type) OR
            IsArray (type) OR IsPointer( type) OR IsSubrange (type)
         THEN
            PutAlignment (type, align)
         ELSE
            MetaError1 ('not allowed to add an alignment attribute to type {%1ad}', type)
         END
      END
   ELSIF alignment # NulName
   THEN
      WriteFormat1 ('unknown type alignment attribute, %a', alignment)
   ELSE
      PopT (type)
   END
END BuildTypeAlignment ;


(*
   BuildVarAlignment - the AlignmentConst is either a temporary or NulSym.
                       A type may only have one alignment value and
                       error checking is performed.

                       Stack

                           Entry                 Exit


                   Ptr ->
                          +-----------------+
                          | AlignmentConst  |                             <- Ptr
                          |-----------------|        +------------------+
                          | Type            |        | Type  | TypeName |
                          |-----------------|        |------------------|
*)

PROCEDURE BuildVarAlignment ;
VAR
   tokno    : CARDINAL ;
   alignment,
   newname  : Name ;
   new,
   type,
   align    : CARDINAL ;
   s        : String ;
BEGIN
   PopT(alignment) ;
   IF alignment=MakeKey('bytealignment')
   THEN
      PopT(align) ;
      PopTtok(type, tokno) ;
      IF IsRecord(type) OR IsRecordField(type) OR IsType(type) OR IsArray(type) OR IsPointer(type)
      THEN
         stop ;
         IF IsNameAnonymous(type)
         THEN
            PutAlignment(type, align) ;
            PushTFtok(type, GetSymName(type), tokno) ;
            Annotate("%1s(%1d)|%2n|%3d||aligned type|aligned type name|token no")
         ELSE
            (* create a pseudonym *)
            s := Sprintf1(Mark(InitString('_$A%d')), alignTypeNo) ;
            INC(alignTypeNo) ;
            newname := makekey(string(s)) ;
            IF IsPointer(type)
            THEN
               new := MakePointer(tokno, newname)
            ELSE
               new := MakeType(tokno, newname)
            END ;
            s := KillString(s) ;
            PutType(new, type) ;
            PutAlignment(new, align) ;
            PushTFtok(new, GetSymName(new), tokno) ;
            Annotate("%1s(%1d)|%2n|%3d||aligned type|aligned type name")
         END
      ELSE
         MetaError1('not allowed to add an alignment attribute to type {%1ad}', type) ;
         PushTFtok(type, GetSymName(type), tokno) ;
         Annotate("%1s(%1d)|%2n|%3d||error aligned type|error aligned type name")
      END
   ELSIF alignment#NulName
   THEN
      WriteFormat1('unknown variable alignment attribute, %a', alignment)
   END
END BuildVarAlignment ;


(*
   BuildVariable - Builds variables listed in an IdentList with a Type.

                   Stack

                   Entry                 Exit

            Ptr ->
                   +------------+        +------------+
                   | Type | Name|        |            |
                   |------------|        |------------|
                   | #          |        |            |
                   |------------|        |------------|
                   | Ident 1    |        |            |
                   |------------|        |------------|
                   | Ident 2    |        |            |
                   |------------|        |------------|
                   .            .        .            .
                   .            .        .            .
                   .            .        .            .
                   |------------|        |------------|
                   | Ident #    |        |            | <- Ptr
                   |------------|        |------------|

                                           Empty
*)

PROCEDURE BuildVariable ;
VAR
   name     : Name ;
   tok,
   typetok,
   AtAddress,
   Type,
   Var,
   i, n     : CARDINAL ;
BEGIN
   PopTFtok (Type, name, typetok) ;
   PopT (n) ;
   i := 1 ;
   WHILE i <= n DO
      CheckVariableAgainstKeyword (OperandT (n+1-i)) ;
      tok := OperandTok (n+1-i) ;
      Var := MakeVar (tok, OperandT (n+1-i)) ;
      AtAddress := OperandA (n+1-i) ;
      IF AtAddress # NulSym
      THEN
         PutVariableAtAddress (Var, NulSym) ;
         PutMode (Var, LeftValue)
      END ;
      PutVarTok (Var, Type, typetok) ;
      IF tok # UnknownTokenNo
      THEN
         PutDeclared (tok, Var) ;
         PutVarDeclTok (Var, tok)
      END ;
      INC (i)
   END ;
   PopN (n)
END BuildVariable ;


(*
   BuildType - Builds a Type.


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

PROCEDURE BuildType ;
VAR
   isunknown: BOOLEAN ;
   n1, n2   : Name ;
   Sym,
   Type     : CARDINAL ;
   name     : Name ;
   tokno    : CARDINAL ;
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
   IF name=NulName
   THEN
      (*
         Typically the declaration that causes this case is:

         VAR
            a: RECORD
                  etc
               END ;
             ^
             |
             +---- type has no name.

      *)
      (* WriteString('Blank name type') ; WriteLn ; *)
      PushTFtok(Type, name, tokno) ;
      Annotate("%1s(%1d)|%2n|%3d||type|type name|token no")
   ELSIF IsError(Type)
   THEN
      PushTFtok(Type, name, tokno) ;
      Annotate("%1s(%1d)|%2n|%3d||error type|error type name|token no")
   ELSIF GetSymName(Type)=name
   THEN
      isunknown := IsUnknown(Type) ;
      IF isunknown OR
         (NOT IsDeclaredIn(GetCurrentScope(), Type))
      THEN
         Sym := MakeType(tokno, name) ;
         IF NOT IsError(Sym)
         THEN
            IF Sym=Type
            THEN
               IF isunknown
               THEN
                  MetaError2('attempting to declare a type {%1ad} to a type which is itself and also unknown {%2ad}',
                             Sym, Type)
               ELSE
                  MetaError1('attempting to declare a type {%1ad} as itself', Sym)
               END
            ELSE
               PutType(Sym, Type) ;
               CheckForExportedImplementation(Sym) ;    (* May be an exported hidden type *)
               (* if Type is an enumerated type then add its contents to the pseudo scope *)
               CheckForEnumerationInCurrentModule(Type)
            END
         END ;
         PushTFtok(Sym, name, tokno) ;
         Annotate("%1s(%1d)|%2n|%3d||type|type name|token no")
      ELSE
         PushTFtok(Type, name, tokno) ;
         Annotate("%1s(%1d)|%2n|%3d||type|type name|token no")
      END
   ELSE
      (* example   TYPE a = CARDINAL *)
      Sym := MakeType(tokno, name) ;
      PutType(Sym, Type) ;
      CheckForExportedImplementation(Sym) ;   (* May be an exported hidden type *)
      PushTFtok(Sym, name, tokno) ;
      Annotate("%1s(%1d)|%2n|%3d||type|type name|token no")
   END
END BuildType ;


(*
   StartBuildProcedure - Builds a Procedure.

                         The Stack:

                         Entry                 Exit

                                                              <- Ptr
                                               +------------+
                  Ptr ->                       | ProcSym    |
                         +------------+        |------------|
                         | Name       |        | Name       |
                         |------------|        |------------|
*)

PROCEDURE StartBuildProcedure ;
VAR
   name   : Name ;
   ProcSym: CARDINAL ;
   tokno  : CARDINAL ;
BEGIN
   PopTtok (name, tokno) ;
   PushTtok (name, tokno) ;  (* name saved for the EndBuildProcedure name check *)
   Annotate ("%1n|(%1d)||procedure name saved by StartBuildProcedure") ;
   ProcSym := GetDeclareSym (tokno, name) ;
   IF IsUnknown (ProcSym)
   THEN
      (* May have been compiled in the definition or implementation module.
         Note we always see an implementation module before its corresponding
         definition module.  *)
      ProcSym := MakeProcedure (tokno, name)
   ELSIF IsProcedure (ProcSym)
   THEN
      PutDeclared (tokno, ProcSym)
   ELSE
      ErrorStringAt2 (Sprintf1(Mark(InitString('procedure name (%a) has been declared as another object elsewhere')),
                               name), tokno, GetDeclaredMod (ProcSym))
   END ;
   IF CompilingDefinitionModule ()
   THEN
      PutExportUnImplemented (tokno, ProcSym)    (* Defined but not yet implemented *)
   ELSE
      CheckForExportedImplementation (ProcSym)   (* May be exported procedure *)
   END ;
   PushTtok (ProcSym, tokno) ;
   Annotate ("%1s(%1d)||procedure start symbol") ;
   StartScope (ProcSym) ;
   M2Error.EnterProcedureScope (name)
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
   NameEnd,
   NameStart: Name ;
   tok      : CARDINAL ;
   ProcSym  : CARDINAL ;
   kind     : ProcedureKind ;
BEGIN
   PopT (NameEnd) ;
   PopTtok (ProcSym, tok) ;
   Assert (IsProcedure(ProcSym)) ;
   kind := GetProcedureKind (ProcSym, tok) ;
   PopT (NameStart) ;
   IF NameEnd # NameStart
   THEN
      WriteFormat2 ('end procedure name does not match beginning %a name %a', NameStart, NameEnd)
   END ;
   PutProcedureParameterHeapVars (ProcSym) ;
   PutProcedureDefined (ProcSym, kind) ;
   EndScope ;
   M2Error.LeaveErrorScope
END EndBuildProcedure ;


(*
   EndBuildForward - ends building a forward procedure.
*)

PROCEDURE EndBuildForward ;
BEGIN
   PopN (2) ;
   EndScope ;
   M2Error.LeaveErrorScope
END EndBuildForward ;


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
   tok,
   ProcSym  : CARDINAL ;
   NameStart: Name ;
BEGIN
   ProcSym := OperandT (1) ;
   tok := OperandTok (1) ;
   PutProcedureParametersDefined (ProcSym, GetProcedureKind (ProcSym, tok)) ;
   IF CompilingDefinitionModule ()
   THEN
      PopT (ProcSym) ;
      Assert (IsProcedure (ProcSym)) ;
      PopT (NameStart) ;
      EndScope
   END
END BuildProcedureHeading ;


(*
   BuildFPSection - Builds a Formal Parameter in a procedure.

                    The Stack:

                    Entry                 Exit

             Ptr ->
                    +------------+
                    | ParamTotal |
                    |------------|
                    | TypeSym    |
                    |------------|
                    | Array/Nul  |
                    |------------|
                    | NoOfIds    |
                    |------------|
                    | Id 1       |
                    |------------|
                    .            .
                    .            .
                    .            .
                    |------------|
                    | Id n       |                       <- Ptr
                    |------------|        +------------+
                    | Var / Nul  |        | ParamTotal |
                    |------------|        |------------|
                    | ProcSym    |        | ProcSym    |
                    |------------|        |------------|
*)

PROCEDURE BuildFPSection ;
VAR
   kind,
   curkind   : ProcedureKind ;
   tok       : CARDINAL ;
   top,
   ProcSym,
   ParamTotal: CARDINAL ;
BEGIN
   top := Top () ;
   PopT (ParamTotal) ;
   ProcSym := CARDINAL (OperandT (3 + CARDINAL (OperandT (3)) + 2)) ;
   tok := CARDINAL (OperandTok (3 + CARDINAL (OperandT (3)) + 2)) ;
   (* Debug (tok, ProcSym, 'foo') ;   *)
   curkind := GetProcedureKind (ProcSym, tok) ;
   PushT (ParamTotal) ;
   Annotate ("%1d||running total of no. of parameters") ;
   Assert (IsProcedure (ProcSym) OR IsProcType (ProcSym)) ;
   Assert (top = Top ()) ;
   ProcSym := CARDINAL (OperandT (3 + CARDINAL (OperandT (3 + 1)) + 2 + 1)) ;
   Assert (IsProcedure (ProcSym) OR IsProcType (ProcSym)) ;

   IF NOT GetProcedureParametersDefined (ProcSym, curkind)
   THEN
      BuildFormalParameterSection (curkind)
   END ;
   (* Check against any previous declaration.  *)
   FOR kind := MIN (ProcedureKind) TO MAX (ProcedureKind) DO
      IF (kind # curkind) AND GetProcedureParametersDefined (ProcSym, kind)
      THEN
         Assert (top = Top ()) ;
         CheckFormalParameterSection (curkind, kind) ;
         Assert (top = Top ())
      END ;
      ProcSym := CARDINAL (OperandT (3 + CARDINAL (OperandT (3 + 1)) + 2 + 1)) ;
      Assert (IsProcedure (ProcSym) OR IsProcType (ProcSym))
   END ;
   RemoveFPParameters ;
   Assert (IsProcedure (OperandT (2)))
END BuildFPSection ;


(*
   BuildVarArgs - indicates that the ProcSym takes varargs
                  after ParamTotal.
                                                         <- Ptr
                    +------------+        +------------+
                    | ParamTotal |        | ParamTotal |
                    |------------|        |------------|
                    | ProcSym    |        | ProcSym    |
                    |------------|        |------------|

*)

PROCEDURE BuildVarArgs ;
VAR
   kind      : ProcedureKind ;
   tok       : CARDINAL ;
   ProcSym,
   ParamTotal: CARDINAL ;
BEGIN
   PopT (ParamTotal) ;
   PopTtok (ProcSym, tok) ;
   kind := GetProcedureKind (ProcSym, tok) ;
   IF UsesOptArg (ProcSym, kind)
   THEN
      WriteFormat0('procedure can use either a single optional argument or a single vararg section ... at the end of the formal parameter list')
   END ;
   IF UsesVarArgs (ProcSym)
   THEN
      WriteFormat0('procedure can only have one vararg section ... at the end of the formal parameter list')
   END ;
   PutUseVarArgs (ProcSym) ;
   IF IsDefImp(GetCurrentModule())
   THEN
      IF NOT IsDefinitionForC(GetCurrentModule())
      THEN
         WriteFormat0('the definition module must be declared as DEFINITION MODULE FOR "C" if varargs are to be used')
      END
   ELSE
      WriteFormat0('varargs can only be used in the module declared as DEFINITION MODULE FOR "C"')
   END ;
   PushTtok (ProcSym, tok) ;
   PushT (ParamTotal)
END BuildVarArgs ;


(*
   BuildOptArg - indicates that the ProcSym takes a single optarg
                 after ParamTotal.

                                                         <- Ptr
                 +------------+        +------------+
                 | ParamTotal |        | ParamTotal |
                 |------------|        |------------|
                 | ProcSym    |        | ProcSym    |
                 |------------|        |------------|
*)

PROCEDURE BuildOptArg ;
VAR
   kind      : ProcedureKind ;
   tok       : CARDINAL ;
   ProcSym,
   ParamTotal: CARDINAL ;
BEGIN
   PopT (ParamTotal) ;
   PopTtok (ProcSym, tok) ;
   kind := GetProcedureKind (ProcSym, tok) ;
   IF UsesVarArgs (ProcSym)
   THEN
      WriteFormat0('procedure can not use an optional argument after a vararg ...')
   END ;
   PutUseOptArg (ProcSym, kind) ;
   PushTtok (ProcSym, tok) ;
   PushT (ParamTotal)
END BuildOptArg ;


(*
   BuildFormalVarArgs - indicates that the procedure type takes varargs.

                                                             <- Ptr
                        +------------+        +------------+
                        | ProcSym    |        | ProcSym    |
                        |------------|        |------------|

*)

PROCEDURE BuildFormalVarArgs ;
VAR
   ProcSym: CARDINAL ;
BEGIN
   PopT(ProcSym) ;
   IF UsesVarArgs(ProcSym)
   THEN
      WriteFormat0('procedure type can only have one vararg section ... at the end of the formal parameter list')
   END ;
   PutUseVarArgs(ProcSym) ;
   IF IsDefImp(GetCurrentModule())
   THEN
      IF NOT IsDefinitionForC(GetCurrentModule())
      THEN
         WriteFormat0('the definition module must be declared as DEFINITION MODULE FOR "C" if varargs are to be used')
      END
   ELSE
      WriteFormat0('varargs can only be used in the module declared as DEFINITION MODULE FOR "C"')
   END ;
   PushT (ProcSym)
END BuildFormalVarArgs ;


(*
   BuildFormalParameterSection - Builds a Formal Parameter in a procedure.

                                 The Stack:

                                 Entry and Exit

                          Ptr ->                <- Ptr
                                 +------------+
                                 | ParamTotal |
                                 |------------|
                                 | TypeSym    |
                                 |------------|
                                 | Array/Nul  |
                                 |------------|
                                 | NoOfIds    |
                                 |------------|
                                 | Id 1       |
                                 |------------|
                                 .            .
                                 .            .
                                 .            .
                                 |------------|
                                 | Id n       |
                                 |------------|
                                 | Var / Nul  |
                                 |------------|
                                 | ProcSym    |
                                 |------------|
*)

PROCEDURE BuildFormalParameterSection (kind: ProcedureKind) ;
VAR
   ParamName,
   Var,
   Array     : Name ;
   tok       : CARDINAL ;
   pi,
   TypeTok,
   ParamTotal,
   TypeSym,
   UnBoundedSym,
   NoOfIds,
   ProcSym,
   i, ndim   : CARDINAL ;
BEGIN
   PopT (ParamTotal) ;
   PopTtok (TypeSym, TypeTok) ;
   PopTF (Array, ndim) ;
   Assert ((Array=ArrayTok) OR (Array=NulTok)) ;
   PopT (NoOfIds) ;
   ProcSym := OperandT (NoOfIds + 2) ;
   Assert (IsProcedure (ProcSym)) ;
   Var := OperandT (NoOfIds + 1) ;
   tok := OperandTok (NoOfIds + 2) ;
   Assert ((Var=VarTok) OR (Var=NulTok)) ;
   (* Restore popped elements.  *)
   PushT (NoOfIds) ;
   PushTF (Array, ndim) ;
   PushTtok (TypeSym, TypeTok) ;
   PushT (ParamTotal) ;

   IF Array = ArrayTok
   THEN
      UnBoundedSym := MakeUnbounded (tok, TypeSym, ndim) ;
      TypeSym := UnBoundedSym
   END ;
   i := 1 ;
   (* +4 to skip over the top restored elements.  *)
   pi := NoOfIds + 4 ;  (* Stack index referencing stacked parameter i.  *)
   WHILE i <= NoOfIds DO
      IF CompilingDefinitionModule () AND (NOT PedanticParamNames) AND
         (* We will see the parameters in the implementation module.  *)
         ((GetMainModule () = GetCurrentModule ()) OR
          (IsHiddenTypeDeclared (GetCurrentModule ()) AND ExtendedOpaque))
      THEN
         ParamName := NulName
      ELSE
         ParamName := OperandT (pi)
      END ;
      tok := OperandTok (pi) ;
      IF Var=VarTok
      THEN
         (* VAR parameter.  *)
         IF NOT PutVarParam (tok, ProcSym, kind, ParamTotal + i, ParamName,
                             TypeSym, Array = ArrayTok, TypeTok)
         THEN
            InternalError ('problems adding a VarParameter - wrong param number?')
         END
      ELSE
         (* Non VAR parameter.  *)
         IF NOT PutParam (tok, ProcSym, kind, ParamTotal + i, ParamName,
                          TypeSym, Array = ArrayTok, TypeTok)
         THEN
            InternalError ('problems adding a Parameter - wrong param number?')
         END
      END ;
      (*
      IF kind = ProperProcedure
      THEN
         PutDeclared (OperandTok (pi), GetParameterShadowVar (GetNthParam (ProcSym, kind, ParamTotal + i)))
      END ;
      *)
      INC (i) ;
      DEC (pi)
   END
END BuildFormalParameterSection ;


(*
   CheckFormalParameterSection - Checks a Formal Parameter in a procedure.
                                 The stack is unaffected.

                                 The Stack:

                                 Entry and Exit

                          Ptr ->
                                 +------------+
                                 | ParamTotal |
                                 |------------|
                                 | TypeSym    |
                                 |------------|
                                 | Array/Nul  |
                                 |------------|
                                 | NoOfIds    |
                                 |------------|
                                 | Id 1       |
                                 |------------|
                                 .            .
                                 .            .
                                 .            .
                                 |------------|
                                 | Id n       |
                                 |------------|
                                 | Var / Nul  |
                                 |------------|
                                 | ProcSym    |
                                 |------------|
*)

PROCEDURE CheckFormalParameterSection (curkind, prevkind: ProcedureKind) ;
VAR
   Array, Var: Name ;
   isVarParam,
   Unbounded : BOOLEAN ;
   ParamI,
   ParamIType,
   ParamTotal,
   TypeTok,
   TypeSym,
   NoOfIds,
   ProcSym,
   pi, i, ndim: CARDINAL ;
BEGIN
   PopT(ParamTotal) ;
   PopTtok(TypeSym, TypeTok) ;
   PopTF(Array, ndim) ;
   Assert( (Array=ArrayTok) OR (Array=NulTok) ) ;
   PopT(NoOfIds) ;
   ProcSym := OperandT (NoOfIds+2) ;
   Assert (IsProcedure (ProcSym)) ;
   Var := OperandT (NoOfIds+1) ;
   Assert ((Var = NulName) OR (Var = VarTok)) ;
   isVarParam := (Var # NulName) ;

   (* Restore popped elements.  *)
   PushT (NoOfIds) ;
   PushTF (Array, ndim) ;
   PushTtok (TypeSym, TypeTok) ;
   PushT (ParamTotal) ;

   Assert( (Var=VarTok) OR (Var=NulTok) ) ;
   Unbounded := (Array=ArrayTok) ;  (* ARRAY OF Type, parameter.  *)
   i := 1 ;
   (* +4 to skip over the top restored elements.  *)
   pi := NoOfIds + 4 ;  (* Stack index referencing stacked parameter i.  *)

   (* If there are an incorrect number of parameters specified then this
      will be detcted by EndBuildFormalParameters.  *)
   WHILE i<=NoOfIds DO
      IF ParamTotal+i <= NoOfParam (ProcSym, prevkind)
      THEN
         (* WarnStringAt (InitString ('parampos?'), OperandTok (pi)) ;  *)
         IF Unbounded AND (NOT IsUnboundedParam (ProcSym, prevkind, ParamTotal+i))
         THEN
            ParameterError ('declaration of procedure {%%1a} in the %s differs from the %s, {%%2N} parameter is inconsistant, %s',
                            'the parameter {%3EHa} was not declared as an ARRAY OF type',
                            'the parameter {%3EVa} was declared as an ARRAY OF type',
                            ParamTotal+i, ProcSym, curkind, prevkind)
         ELSIF (NOT Unbounded) AND IsUnboundedParam (ProcSym, prevkind, ParamTotal+i)
         THEN
            ParameterError ('declaration of procedure {%%1a} in the %s differs from the %s, {%%2N} parameter is inconsistant, %s',
                            'the parameter {%3EHa} was declared as an ARRAY OF type',
                            'the parameter {%3EVa} was not declared as an ARRAY OF type',
                            ParamTotal+i, ProcSym, curkind, prevkind)
         END ;
         IF Unbounded
         THEN
            IF GetDimension (GetNthParam (ProcSym, prevkind, ParamTotal+1)) # ndim
            THEN
               ParameterError ('declaration of procedure {%%1a} in the %s differs from the %s, {%%2N} parameter is inconsistant, %s',
                               'the dynamic array parameter {%3EHa} was declared with a different of dimensions',
                               'the dynamic array parameter {%3EVa} was declared with a different of dimensions',
                               ParamTotal+i, ProcSym, curkind, prevkind)
            END
         END ;
         IF isVarParam AND (NOT IsVarParam (ProcSym, prevkind, ParamTotal+i))
         THEN
            (* Expecting non VAR parameter.  *)
            ParameterError ('declaration of procedure {%%1a} in the %s differs from the %s, {%%2N} parameter is inconsistant, %s',
                            '{%3EHa} was not declared as a {%kVAR} parameter',
                            '{%3EVa} was declared as a {%kVAR} parameter',
                            ParamTotal+i, ProcSym, curkind, prevkind)
         ELSIF (NOT isVarParam) AND IsVarParam (ProcSym, prevkind, ParamTotal+i)
         THEN
            (* Expecting VAR pamarater.  *)
            ParameterError ('declaration of procedure {%%1a} in the %s differs from the %s, {%%2N} parameter is inconsistant, %s',
                            '{%3EHa} was declared as a {%kVAR} parameter',
                            '{%3EVa} was not declared as a {%kVAR} parameter',
                            ParamTotal+i, ProcSym, curkind, prevkind)
         END ;
         ParamI := GetNthParam (ProcSym, prevkind, ParamTotal+i) ;
         IF PedanticParamNames
         THEN
            IF GetSymName (ParamI) # OperandT (pi)
            THEN
               (* Different parameter names.  *)
               ParameterError ('procedure {%%1a} in the %s differs from the %s, {%%2N} parameter name is inconsistant, %s',
                            'named as {%3EVa}',
                            'named as {%3EVa}',
                            ParamTotal+i, ProcSym, curkind, prevkind)
            END
         END ;
         IF Unbounded
         THEN
            (* GetType(ParamI) yields an UnboundedSym or a PartialUnboundedSym,
               depending whether it has been resolved.. *)
            ParamIType := GetType (GetType (ParamI))
         ELSE
            ParamIType := GetType (ParamI)
         END ;
         IF ((SkipType(ParamIType)#SkipType(TypeSym)) OR
             (PedanticParamNames AND (ParamIType#TypeSym))) AND
            (NOT IsUnknown(SkipType(TypeSym))) AND
            (NOT IsUnknown(SkipType(ParamIType)))
         THEN
            (* Different parameter types.  *)
            ParameterError ('declaration in the %s differs from the %s, {%%2N} parameter is inconsistant, %s',
                            'the parameter {%3EHa} was declared with a different type',
                            'the parameter {%3EVa} was declared with a different type',
                            ParamTotal+i, ProcSym, curkind, prevkind)
         END
      END ;
      INC (i) ;
      DEC (pi)
   END
END CheckFormalParameterSection ;


(*
   RemoveFPParameters - remove the FPSection parameters from the stack and
                        increment the param total with the NoOfIds.

                        The Stack:

                               Entry                 Exit

                        Ptr ->
                               +------------+
                               | ParamTotal |
                               |------------|
                               | TypeSym    |
                               |------------|
                               | Array/Nul  |
                               |------------|
                               | NoOfIds    |
                               |------------|
                               | Id 1       |
                               |------------|
                               .            .
                               .            .
                               .            .
                               |------------|
                               | Id n       |                       <- Ptr
                               |------------|        +------------+
                               | Var / Nul  |        | ParamTotal |
                               |------------|        |------------|
                               | ProcSym    |        | ProcSym    |
                               |------------|        |------------|
*)

PROCEDURE RemoveFPParameters ;
VAR
   ParamTotal,
   Array,
   TypeSym,
   NoOfIds,
   ProcSym   : CARDINAL ;
BEGIN
   PopT (ParamTotal) ;
   PopT (TypeSym) ;
   PopT (Array) ;
   Assert ((Array=ArrayTok) OR (Array=NulTok)) ;
   PopT (NoOfIds) ;
   ProcSym := OperandT (NoOfIds+2) ;
   Assert (IsProcedure (ProcSym)) ;
   PopN (NoOfIds+1) ;   (* +1 for the Var/Nul.  *)
   PushT (ParamTotal + NoOfIds) ;
   Annotate ("%1d||running total of no. of parameters") ;
   Assert (IsProcedure (OperandT (2)))
END RemoveFPParameters ;


(*
   ParameterError - create two error strings chained together.
*)

PROCEDURE ParameterError (FmtHeader, PrevDesc, CurDesc: ARRAY OF CHAR;
                          ParamNo, ProcSym: CARDINAL;
                          curkind, prevkind: ProcedureKind) ;
VAR
   PrevParam,
   CurParam   : CARDINAL ;
   CurStr,
   PrevStr,
   Msg,
   CurKindStr,
   PrevKindStr: String ;
BEGIN
   CurParam := GetNthParam (ProcSym, curkind, ParamNo) ;
   CurKindStr := GetProcedureKindDesc (curkind) ;
   PrevKindStr := GetProcedureKindDesc (prevkind) ;
   PrevParam := GetNthParam (ProcSym, prevkind, ParamNo) ;
   PrevStr := InitString (PrevDesc) ;
   CurStr := InitString (CurDesc) ;
   Msg := Sprintf3 (Mark (InitString (FmtHeader)), CurKindStr, PrevKindStr, PrevStr) ;
   MetaErrorString3 (Msg, ProcSym, ParamNo, PrevParam) ;
   Msg := Sprintf3 (Mark (InitString (FmtHeader)), CurKindStr, PrevKindStr, CurStr) ;
   MetaErrorString3 (Msg, ProcSym, ParamNo, CurParam)
END ParameterError ;


(*
   StartBuildFormalParameters - Initialises the quadruple stack for
                                Formal Parameters.

                                The Stack:

                                Entry                Exit

                                                                    <- Ptr
                                                     +------------+
                                Empty                | 0          |
                                                     |------------|
*)

PROCEDURE StartBuildFormalParameters ;
BEGIN
   PushT (0) ;
   Annotate ("%1d||running total of no. of parameters")
END StartBuildFormalParameters ;


(*
   ParameterMismatch - generate a parameter mismatch error between the current
                       declaration at tok and a previous ProcSym declaration.
                       NoOfPar is the current number of parameters.
*)

PROCEDURE ParameterMismatch (tok: CARDINAL; ProcSym: CARDINAL;
                             NoOfPar: CARDINAL; prevkind, curkind: ProcedureKind) ;
VAR
   MsgCurrent,
   MsgPrev,
   CompCur,
   CompPrev,
   CurDesc,
   PrevDesc  : String ;
BEGIN
   CurDesc := GetProcedureKindDesc (curkind) ;
   PrevDesc := GetProcedureKindDesc (prevkind) ;
   CompPrev := GetComparison (NoOfParam (ProcSym, prevkind), NoOfPar) ;
   CompCur := GetComparison (NoOfPar, NoOfParam (ProcSym, prevkind)) ;
   MsgCurrent := Sprintf3 (Mark (InitString ('the %s for {%%1ad} has %s parameters than the %s')),
                           CurDesc, CompCur, PrevDesc) ;
   MsgPrev := Sprintf3 (Mark (InitString ('the %s for {%%1ad} has %s parameters than the %s')),
                        PrevDesc, CompPrev, CurDesc) ;
   MetaErrorStringT1 (GetProcedureDeclaredTok (ProcSym, prevkind), MsgPrev, ProcSym) ;
   MetaErrorStringT1 (tok, MsgCurrent, ProcSym) ;
   CurDesc := KillString (CurDesc) ;
   PrevDesc := KillString (PrevDesc) ;
   CompCur := KillString (CompCur) ;
   CompPrev := KillString (CompPrev)
END ParameterMismatch ;


(*
   EndBuildFormalParameters - Resets the quadruple stack after building
                              Formal Parameters.

                              The Stack:

                              Entry                    Exit

                       Ptr ->
                              +------------+
                              | NoOfParam  |                          <- Ptr
                              |------------|           +------------+
                              | ProcSym    |           | ProcSym    |
                              |------------|           |------------|
*)

PROCEDURE EndBuildFormalParameters ;
VAR
   kind,
   curkind: ProcedureKind ;
   tok    : CARDINAL ;
   NoOfPar: CARDINAL ;
   ProcSym: CARDINAL ;
BEGIN
   PopT (NoOfPar) ;
   PopTtok (ProcSym, tok) ;
   PushTtok (ProcSym, tok) ;
   Annotate ("%1s(%1d)||procedure start symbol") ;
   Assert (IsProcedure (ProcSym)) ;
   curkind := GetProcedureKind (ProcSym, tok) ;
   FOR kind := MIN (ProcedureKind) TO MAX (ProcedureKind) DO
      IF GetProcedureParametersDefined (ProcSym, kind) AND
         (curkind # kind) AND (NoOfParam (ProcSym, kind) # NoOfPar)
      THEN
         ParameterMismatch (tok, ProcSym, NoOfPar, kind, curkind)
      END
   END ;
   (* All parameter seen so set procedure defined.  *)
   PutProcedureParametersDefined (ProcSym, curkind) ;
   Assert (IsProcedure (OperandT (1)))
END EndBuildFormalParameters ;


(*
   GetComparison - return a simple description from the result of
                   a comparison between left and right.
*)

PROCEDURE GetComparison (left, right: CARDINAL) : String ;
BEGIN
   IF left < right
   THEN
      RETURN InitString ('fewer')
   ELSIF left > right
   THEN
      RETURN InitString ('more')
   ELSE
      RETURN InitString ('same')
   END
END GetComparison ;


(*
   ReturnTypeMismatch - generate two errors showing the return type mismatches between
                        ProcSym and ReturnType at procedure location tok.
*)

PROCEDURE ReturnTypeMismatch (curtok: CARDINAL; ProcSym, CurRetType: CARDINAL;
                              curtypetok: CARDINAL;
                              curkind, prevkind: ProcedureKind;
                              PrevRetType: CARDINAL) ;
VAR
   prevtok   : CARDINAL ;
   CurDesc,
   PrevDesc,
   MsgCurrent,
   MsgPrev   : String ;
BEGIN
   CurDesc := GetProcedureKindDesc (curkind) ;
   PrevDesc := GetProcedureKindDesc (prevkind) ;
   prevtok := GetProcedureDeclaredTok (ProcSym, prevkind) ;
   IF CurRetType = NulSym
   THEN
      MsgCurrent := Sprintf2 (Mark (InitString ('there is no return type for {%%1ad} specified in the %s whereas a return type is specified in the %s')),
                              CurDesc, PrevDesc) ;
      MsgPrev := Sprintf2 (Mark (InitString ('there is no return type for {%%1ad} specified in the %s whereas a return type is specified in the %s')),
                              CurDesc, PrevDesc) ;
      prevtok := GetReturnTypeTok (ProcSym, prevkind)
   ELSIF PrevRetType = NulSym
   THEN
      MsgCurrent := Sprintf2 (Mark (InitString ('there is no return type for {%%1ad} specified in the %s whereas a return type is specified in the %s')),
                              PrevDesc, CurDesc) ;
      MsgPrev := Sprintf2 (Mark (InitString ('there is no return type for {%%1ad} specified in the %s whereas a return type is specified in the %s')),
                              PrevDesc, CurDesc) ;
      curtok := curtypetok
   ELSE
      MsgCurrent := Sprintf2 (Mark (InitString ('the return type for {%%1ad} specified in the %s differs in the %s')),
                              CurDesc, PrevDesc) ;
      MsgPrev := Sprintf2 (Mark (InitString ('the return type for {%%1ad} specified in the %s differs in the %s')),
                              CurDesc, PrevDesc) ;
      curtok := curtypetok ;
      prevtok := GetReturnTypeTok (ProcSym, prevkind)
   END ;
   MetaErrorStringT1 (curtok, MsgCurrent, ProcSym) ;
   MetaErrorStringT1 (prevtok, MsgPrev, ProcSym)
END ReturnTypeMismatch ;


(*
   BuildFunction - Builds a procedures return type.
                   Procedure becomes a function.

                    The Stack:

                    Entry                 Exit

             Ptr ->
                    +------------+
                    | TypeSym    |                       <- Ptr
                    |------------|        +------------+
                    | ProcSym    |        | ProcSym    |
                    |------------|        |------------|
*)

PROCEDURE BuildFunction ;
VAR
   tok    : CARDINAL ;
   ProcSym,
   typetok: CARDINAL ;
   RetType: CARDINAL ;
BEGIN
   PopTtok (RetType, typetok) ;
   PopTtok (ProcSym, tok) ;
   PushTtok (ProcSym, tok) ;
   PutFunction (typetok, ProcSym, GetProcedureKind (ProcSym, tok), RetType) ;
   CheckOptFunction (tok, ProcSym, GetProcedureKind (ProcSym, tok), FALSE) ;
   CheckProcedureReturn  (RetType, typetok)
END BuildFunction ;


(*
   BuildOptFunction - Builds a procedures optional return type.
                      Procedure becomes a function and the user
                      can either call it as a function or a procedure.

                      The Stack:

                      Entry                 Exit

               Ptr ->
                      +------------+
                      | TypeSym    |                       <- Ptr
                      |------------|        +------------+
                      | ProcSym    |        | ProcSym    |
                      |------------|        |------------|
*)

PROCEDURE BuildOptFunction ;
VAR
   typetok,
   tok     : CARDINAL ;
   RetType,
   ProcSym : CARDINAL ;
BEGIN
   PopTtok (RetType, typetok) ;
   PopTtok (ProcSym, tok) ;
   PutOptFunction (typetok, ProcSym, GetProcedureKind (ProcSym, tok), RetType) ;
   CheckOptFunction (tok, ProcSym, GetProcedureKind (ProcSym, tok), TRUE) ;
   PushTtok (ProcSym, tok)
END BuildOptFunction ;


(*
   CheckOptFunction - checks to see whether the optional return value
                      has been set before and if it differs it will
                      generate an error message.  It will set the
                      new value to isopt.
*)

PROCEDURE CheckOptFunction (tok: CARDINAL; sym: CARDINAL; kind: ProcedureKind;
                            isopt: BOOLEAN) ;
VAR
   other: ProcedureKind ;
BEGIN
   IF GetType (sym) # NulSym
   THEN
      (* Procedure sym has been declared as a function.  *)
      FOR other := MIN (ProcedureKind) TO MAX (ProcedureKind) DO
         IF (kind # other) AND GetProcedureParametersDefined (sym, other)
         THEN
            IF IsReturnOptional (sym, kind) AND (NOT isopt)
            THEN
               MetaErrorT1 (tok, 'procedure {%1Ea} is not declared with an optional return type here', sym) ;
               MetaErrorT1 (GetReturnTypeTok (sym, kind),
                            'previously procedure {%1Ea} was declared with an optional return type', sym)
            ELSIF (NOT IsReturnOptional (sym, kind)) AND isopt
            THEN
               MetaErrorT1 (tok, 'procedure {%1Ea} is declared with an optional return type here', sym) ;
               MetaErrorT1 (GetReturnTypeTok (sym, kind),
                            'previously procedure {%1Ea} was declared without an optional return type', sym)
            END
         END
      END
   END ;
   SetReturnOptional (sym, kind, isopt)
END CheckOptFunction ;


(*
   BuildNoReturnAttribute - provide an interface to the symbol table module.
*)

PROCEDURE BuildNoReturnAttribute ;
VAR
   kind   : ProcedureKind ;
   ProcSym,
   tok    : CARDINAL ;
BEGIN
   PopTtok (ProcSym, tok) ;
   PushTtok (ProcSym, tok) ;
   kind := GetProcedureKind (ProcSym, tok) ;
   Assert (IsProcedure (ProcSym)) ;
   PutProcedureNoReturn (ProcSym, kind, TRUE)
END BuildNoReturnAttribute ;


(*
   CheckProcedure - checks to see that the top of stack procedure
                    has not been declared as a procedure function.

                    The Stack:

                    Entry                 Exit

             Ptr ->                                      <- Ptr
                    +------------+        +------------+
                    | ProcSym    |        | ProcSym    |
                    |------------|        |------------|
*)

PROCEDURE CheckProcedure ;
BEGIN
   CheckProcedureReturn (NulSym, UnknownTokenNo)
END CheckProcedure ;



PROCEDURE CheckProcedureReturn (RetType: CARDINAL; typetok: CARDINAL) ;
VAR
   curkind,
   kind       : ProcedureKind ;
   tok        : CARDINAL ;
   PrevRetType,
   ProcSym    : CARDINAL ;
BEGIN
   PopTtok (ProcSym, tok) ;
   PushTtok (ProcSym, tok) ;
   Annotate ("%1s(%1d)||procedure start symbol") ;
   IF IsProcedure (ProcSym)
   THEN
      curkind := GetProcedureKind (ProcSym, tok) ;
      (* Check against any previously declared kinds.  *)
      FOR kind := MIN (ProcedureKind) TO MAX (ProcedureKind) DO
         IF (kind # curkind) AND GetProcedureParametersDefined (ProcSym, kind)
         THEN
            PrevRetType := GetType (ProcSym) ;
            IF PrevRetType # RetType
            THEN
               ReturnTypeMismatch (tok, ProcSym, RetType, typetok,
                                   curkind, kind, PrevRetType)
            END
         END
      END ;
      PutFunction (tok, ProcSym, curkind, RetType)
   END
END CheckProcedureReturn ;


(*
   BuildPointerType - builds a pointer type.
                      The Stack:

                      Entry                       Exit
                      =====                       ====


               Ptr ->                                             <- Ptr
                      +------------+              +-------------+
                      | Type       |              | PointerType |
                      |------------|              |-------------|
                      | Name       |              | Name        |
                      |------------|              |-------------|
*)

PROCEDURE BuildPointerType (pointerpos: CARDINAL) ;
VAR
   combined,
   namepos,
   typepos  : CARDINAL ;
   name     : Name ;
   Type,
   PtrToType: CARDINAL ;
BEGIN
   PopTtok (Type, typepos) ;
   PopTtok (name, namepos) ;
   name := CheckAnonymous (name) ;

   combined := MakeVirtual2Tok (pointerpos, typepos) ;
   PtrToType := MakePointer (combined, name) ;
   PutPointer (PtrToType, Type) ;
   CheckForExportedImplementation(PtrToType) ;   (* May be an exported hidden type *)
   PushTtok (name, namepos) ;
   Annotate("%1n|%3d||pointer type name") ;
   PushTtok(PtrToType, combined) ;
   Annotate("%1s(%1d)|%3d||pointer type")
END BuildPointerType ;


(*
   BuildSetType - builds a set type.
                  The Stack:

                  Entry                       Exit
                  =====                       ====


           Ptr ->                                             <- Ptr
                  +------------+              +-------------+
                  | Type       |              | SetType     |
                  |------------|              |-------------|
                  | Name       |              | Name        |
                  |------------|              |-------------|
*)

PROCEDURE BuildSetType (setpos: CARDINAL; ispacked: BOOLEAN) ;
VAR
   combined,
   namepos,
   typepos : CARDINAL ;
   name    : Name ;
   Type,
   SetType : CARDINAL ;
BEGIN
   PopTtok (Type, typepos) ;
   PopTtok (name, namepos) ;
   combined := MakeVirtual2Tok (setpos, typepos) ;
   SetType := MakeSet (combined, name) ;
   CheckForExportedImplementation(SetType) ;   (* May be an exported hidden type *)
   PutSet(SetType, Type, ispacked) ;
   PushTtok (name, namepos) ;
   Annotate("%1n||set type name") ;
   PushTtok (SetType, combined) ;
   Annotate ("%1s(%1d)|%3d||set type|token no")
END BuildSetType ;


(*
   BuildRecord - Builds a record type.
                 The Stack:

                 Entry                              Exit
                 =====                              ====


                                                                  <- Ptr
                                                    +-----------+
          Ptr ->                                    | RecordSym |
                 +------------------+               |-----------|
                 | Name             |               | Name      |
                 |------------------|               |-----------|
*)

PROCEDURE BuildRecord ;
VAR
   tokno     : CARDINAL ;
   name      : Name ;
   RecordType: CARDINAL ;
BEGIN
   name := OperandT(1) ;
   name := CheckAnonymous(name) ;
   tokno := OperandTok(1) ;
   RecordType := MakeRecord(tokno, name) ;
   CheckForExportedImplementation(RecordType) ;   (* May be an exported hidden type *)
   PushT(RecordType) ;
(* ; WriteKey(name) ; WriteString(' RECORD made') ; WriteLn *)
   Annotate("%1s(%1d)||record type")
END BuildRecord ;


(*
   HandleRecordFieldPragmas -

                      Entry                     Exit
                      =====                     ====

               Ptr ->                                     <- Ptr

                      |-------------|           |-------------|
                      | Const1      |           | Const1      |
                      |-------------|           |-------------|
                      | PragmaName1 |           | PragmaName1 |
                      |-------------|           |-------------|
*)

PROCEDURE HandleRecordFieldPragmas (record, field: CARDINAL; n: CARDINAL) ;
VAR
   seenAlignment   : BOOLEAN ;
   defaultAlignment,
   sym             : CARDINAL ;
   i               : CARDINAL ;
   name            : Name ;
   s               : String ;
BEGIN
   seenAlignment := FALSE ;
   defaultAlignment := GetDefaultRecordFieldAlignment(record) ;
   i := 1 ;
   WHILE i<=n DO
      name := OperandT(i*2) ;
      sym  := OperandT(i*2-1) ;
      IF name=MakeKey('unused')
      THEN
         IF sym=NulSym
         THEN
            PutUnused(field)
         ELSE
            WriteFormat0("not expecting pragma 'unused' to contain an expression")
         END
      ELSIF name=MakeKey('bytealignment')
      THEN
         IF sym=NulSym
         THEN
            WriteFormat0("expecting an expression with the pragma 'bytealignment'")
         ELSE
            PutAlignment(field, sym) ;
            seenAlignment := TRUE
         END
      ELSE
         s := InitString("cannot use pragma '") ;
         s := ConCat(s, Mark(InitStringCharStar(KeyToCharStar(name)))) ;
         s := ConCat(s, Mark(InitString("' on record field {%1ad}"))) ;
         MetaErrorString1(s, field)
      END ;
      INC(i)
   END ;
   IF (NOT seenAlignment) AND (defaultAlignment#NulSym)
   THEN
      PutAlignment(field, defaultAlignment)
   END
END HandleRecordFieldPragmas ;


(*
   BuildFieldRecord - Builds a field into a record sym.
                      The Stack:


                      Entry                     Exit
                      =====                     ====

               Ptr ->
                      +-------------+
                      | NoOfPragmas |
                      |-------------|
                      | Const1      |
                      |-------------|
                      | PragmaName1 |
                      |-------------|
                      | Type | Name |
                      |-------------|
                      | n           |
                      |-------------|
                      | Id 1        |
                      |-------------|
                      .             .
                      .             .
                      .             .
                      |-------------|
                      | Id n        |                           <- Ptr
                      |-------------|           +-------------+
                      | RecordSym   |           | RecordSym   |
                      |-------------|           |-------------|
                      | RecordName  |           | RecordName  |
                      |-------------|           |-------------|
*)

PROCEDURE BuildFieldRecord ;
VAR
   n1         : Name ;
   tok,
   fsym,
   Field,
   Varient,
   Parent,
   Type,
   NoOfPragmas,
   NoOfFields,
   Record,
   i          : CARDINAL ;
BEGIN
   PopT(NoOfPragmas) ;
   Type := OperandT(NoOfPragmas*2+1) ;
   (* name := OperandF(NoOfPragmas*2+1) ;  *)
   NoOfFields := OperandT(NoOfPragmas*2+2) ;
   Record := OperandT(NoOfPragmas*2+NoOfFields+3) ;
   IF IsRecord(Record)
   THEN
      Parent := Record ;
      Varient := NulSym
   ELSE
      (* Record maybe FieldVarient *)
      Parent := GetRecord(GetParent(Record)) ;
      Assert(IsFieldVarient(Record)) ;
      Varient := OperandT(NoOfPragmas*2+NoOfFields+4) ;
      Assert(IsVarient(Varient)) ;
      PutFieldVarient(Record, Varient) ;
      IF Debugging
      THEN
         n1 := GetSymName(Record) ;
         WriteString('Record ') ;
         WriteKey(n1) ;
         WriteString(' has varient ') ;
         n1 := GetSymName(Varient) ;
         WriteKey(n1) ; WriteLn
      END
   END ;
   Field := NulSym ;
   i := 1 ;
   WHILE i<=NoOfFields DO
      IF Debugging
      THEN
         n1 := GetSymName(Record) ;
         WriteString('Record ') ;
         WriteKey(n1) ;
         WriteString('  ') ;
         WriteKey(OperandT(NoOfPragmas*2+NoOfFields+3-i)) ; WriteString(' is a Field with type ') ;
         WriteKey(GetSymName(Type)) ; WriteLn ;
      END ;
      fsym := GetLocalSym(Parent, OperandT(NoOfPragmas*2+NoOfFields+3-i)) ;
      IF fsym=NulSym
      THEN
         Field := PutFieldRecord(Record, OperandT(NoOfPragmas*2+NoOfFields+3-i), Type, Varient) ;
         HandleRecordFieldPragmas(Record, Field, NoOfPragmas)
      ELSE
         MetaErrors2('record field {%1ad} has already been declared inside a {%2Dd} {%2a}',
                     'attempting to declare a duplicate record field', fsym, Parent)
      END ;
      (* adjust the location of declaration to the one on the stack (rather than GetTokenNo).  *)
      tok := OperandTok(NoOfPragmas*2+NoOfFields+3-i) ;
      IF (tok # UnknownTokenNo) AND (Field # NulSym)
      THEN
         PutDeclared (tok, Field)
      END ;
      INC(i)
   END ;
   PopN(NoOfPragmas*2+NoOfFields+3) ;
   PushT(Record) ;
   IF IsRecord(Record)
   THEN
      Annotate("%1s(%1d)||record type")
   ELSE
      Assert(IsFieldVarient(Record)) ;
      Annotate("%1s(%1d)||varient field type")
   END
END BuildFieldRecord ;


(*
   BuildVarientSelector - Builds a field into a record sym.
                          The Stack:


                          Entry                     Exit
                          =====                     ====

                   Ptr ->
                          +-------------+
                          | Type        |
                          |-------------|
                          | Tag         |                           <- Ptr
                          |-------------|           +-------------+
                          | RecordSym   |           | RecordSym   |
                          |-------------|           |-------------|
*)

PROCEDURE BuildVarientSelector ;
VAR
   tagtok    : CARDINAL ;
   tag       : Name ;
   Field,
   Type,
   Varient,
   VarField,
   Record    : CARDINAL ;
BEGIN
   PopT(Type) ;
   PopTtok (tag, tagtok) ;
   Record := OperandT(1) ;
   IF IsRecord(Record)
   THEN
      Varient := NulSym ;
      InternalError ('not expecting a record symbol')
   ELSIF IsVarient(Record)
   THEN
      Varient := Record ;
      VarField := GetParent(Varient) ;
      IF (Type=NulSym) AND (tag=NulName)
      THEN
         MetaError1('expecting a tag field in the declaration of a varient record {%1Ua}', Record)
      ELSIF Type=NulSym
      THEN
         PutVarientTag (Varient, RequestSym (tagtok, tag))
      ELSE
         Field := PutFieldRecord (VarField, tag, Type, Varient) ;
         PutVarientTag(Varient, Field) ;
         IF Debugging
         THEN
            WriteString('varient field ') ; WriteKey(GetSymName(VarField)) ;
            WriteString('varient ') ; WriteKey(GetSymName(Varient)) ; WriteLn
         END
      END
   ELSE
      (* Record maybe FieldVarient *)
      Assert(IsFieldVarient(Record)) ;
      Varient := OperandT(1+2) ;
      Assert(IsVarient(Varient)) ;
      PutFieldVarient(Record, Varient) ;
      IF Debugging
      THEN
         WriteString('record ') ; WriteKey(GetSymName(Record)) ;
         WriteString('varient ') ; WriteKey(GetSymName(Varient)) ; WriteLn
      END ;
      IF (Type=NulSym) AND (tag=NulName)
      THEN
         MetaError1('expecting a tag field in the declaration of a varient record {%1Ua}', Record)
      ELSIF Type=NulSym
      THEN
         PutVarientTag(Varient, RequestSym (tagtok, tag))
      ELSE
         Field := PutFieldRecord(Record, tag, Type, Varient) ;
         PutVarientTag(Varient, Field) ;
         IF Debugging
         THEN
            WriteString('record ') ; WriteKey(GetSymName(Record)) ;
            WriteString('varient ') ; WriteKey(GetSymName(Varient)) ; WriteLn
         END
      END
   END
END BuildVarientSelector ;


(*
   StartBuildVarientFieldRecord - Builds a varient field into a varient sym.
                                  The Stack:


                                  Entry                     Exit
                                  =====                     ====

                                                                       <- Ptr
                                                       +-------------+
                      Ptr ->                           | VarientField|
                             +-------------+           |-------------|
                             | VarientSym  |           | VarientSym  |
                             |-------------|           |-------------|
*)

PROCEDURE StartBuildVarientFieldRecord ;
VAR
   VarientSym,
   FieldSym  : CARDINAL ;
BEGIN
   VarientSym := OperandT(1) ;
   FieldSym := MakeFieldVarient(CheckAnonymous(NulName), VarientSym) ;
   Annotate("%1s(%1d)||varient sym") ;
   PushT(FieldSym) ;
   Annotate("%1s(%1d)||varient field type") ;
   Assert(IsFieldVarient(FieldSym)) ;
   PutFieldVarient(FieldSym, VarientSym) ;
   AddVarientFieldToList(FieldSym)
END StartBuildVarientFieldRecord ;


(*
   EndBuildVarientFieldRecord - Removes a varient field from the stack.
                                The Stack:


                                Entry                     Exit
                                =====                     ====

                         Ptr ->
                                +-------------+
                                | VarientField|                           <- Ptr
                                |-------------|           +-------------+
                                | VarientSym  |           | VarientSym  |
                                |-------------|           |-------------|
*)

PROCEDURE EndBuildVarientFieldRecord ;
VAR
   FieldSym: CARDINAL ;
BEGIN
   PopT(FieldSym) ;
   (* GCFieldVarient(FieldSym) *)
END EndBuildVarientFieldRecord ;


(*
   StartBuildVarient - Builds a varient symbol on top of a record sym.
                       The Stack:


                       Entry                     Exit
                       =====                     ====

                                                                 <- Ptr
                                                 +-------------+
                Ptr ->                           | VarientSym  |
                       +-------------+           |-------------|
                       | RecordSym   |           | RecordSym   |
                       |-------------|           |-------------|
                       | RecordName  |           | RecordName  |
                       |-------------|           |-------------|
*)

PROCEDURE StartBuildVarient ;
VAR
   tokno    : CARDINAL ;
   RecordSym,
   Sym      : CARDINAL ;
BEGIN
   RecordSym := OperandT(1) ;
   tokno := OperandTok(1) ;
   Sym := MakeVarient(tokno, RecordSym) ;
   PushT(Sym) ;
   Annotate("%1s(%1d)||varient type")
END StartBuildVarient ;


(*
   EndBuildVarient - Removes the varient symbol from the stack.
                     The Stack:

                     Entry                     Exit
                     =====                     ====

              Ptr ->
                     +-------------+
                     | VarientSym  |                           <- Ptr
                     |-------------|           +-------------+
                     | RecordSym   |           | RecordSym   |
                     |-------------|           |-------------|
                     | RecordName  |           | RecordName  |
                     |-------------|           |-------------|
*)

PROCEDURE EndBuildVarient ;
VAR
   Sym: CARDINAL ;
BEGIN
   PopT(Sym)
END EndBuildVarient ;


(*
   BuildNulName - Pushes a NulName onto the top of the stack.
                  The Stack:


                  Entry                    Exit

                                                          <- Ptr
                  Empty                    +------------+
                                           | NulName   |
                                           |------------|
*)

PROCEDURE BuildNulName ;
BEGIN
   PushTtok (NulName, GetTokenNo ()) ;
   Annotate ("%1n|%3d||NulName|token no")
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
   PopTF(Type, name)
END BuildTypeEnd ;


(*
   StartBuildArray - Builds an array type.
                     The Stack:

                     Entry                        Exit
                     =====                        ====

                                                                <- Ptr
                                                  +-----------+
              Ptr ->                              | ArraySym  |
                     +------------+               |-----------|
                     | Name       |               | Name      |
                     |------------|               |-----------|
*)

PROCEDURE StartBuildArray ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ArrayType: CARDINAL ;
BEGIN
   name := OperandT(1) ;
   tok := OperandTok(1) ;
   ArrayType := MakeArray (tok, name) ;
   CheckForExportedImplementation (ArrayType) ;   (* May be an exported hidden type *)
   PushTtok(ArrayType, tok) ;
   Annotate("%1s(%1d)|%3d||array type|token no")
(* ; WriteKey(Name) ; WriteString(' ARRAY made') ; WriteLn *)
END StartBuildArray ;


(*
   EndBuildArray - Builds an array type.
                   The Stack:

                   Entry                        Exit
                   =====                        ====

            Ptr ->
                   +------------+
                   | TypeSym    |                              <- Ptr
                   |------------|               +------------+
                   | ArraySym   |               | ArraySym   |
                   |------------|               |------------|
                   | Name       |               | Name       |
                   |------------|               |------------|
*)

PROCEDURE EndBuildArray ;
VAR
   typetok,
   arraytok,
   combinedtok: CARDINAL ;
   TypeSym,
   ArraySym   : CARDINAL ;
BEGIN
   PopTtok (TypeSym, typetok) ;
   PopTtok (ArraySym, arraytok) ;
   Assert (IsArray (ArraySym)) ;
   combinedtok := MakeVirtual2Tok (arraytok, typetok) ;
   PutArray (ArraySym, TypeSym) ;
   PutDeclared (combinedtok, ArraySym) ;
   PushTtok (ArraySym, combinedtok) ;
   Annotate ("%1s(%1d)||array type")
END EndBuildArray ;


(*
   BuildFieldArray - Builds a field into an array sym.
                     The Stack:


                     Entry                     Exit
                     =====                     ====

              Ptr ->
                     +-------------+
                     | Type | Name |                           <- Ptr
                     |-------------|           +-------------+
                     | ArraySym    |           | ArraySym    |
                     |-------------|           |-------------|
                     | ArrayName   |           | ArrayName   |
                     |-------------|           |-------------|
*)

PROCEDURE BuildFieldArray ;
VAR
   typetok,
   arraytok  : CARDINAL ;
   Subscript,
   Type,
   Array     : CARDINAL ;
   name      : Name ;
BEGIN
   PopTFtok (Type, name, typetok) ;
   PopTtok (Array, arraytok) ;
   Assert (IsArray (Array)) ;
   Subscript := MakeSubscript () ;
   (*
      We cannot Assert(IsSubrange(Type)) as the subrange type might be
      declared later on in the file.
      We also note it could be an ordinal type or enumerated type.
      Therefore we must save this information and deal with the
      different cases in M2GCCDeclare.mod and M2GenGCC.mod.
      However this works to our advantage as it preserves the
      actual declaration as specified by the source file.
   *)
   PutSubscript (Subscript, Type) ;
   PutArraySubscript (Array, Subscript) ;
   PushTtok (Array, arraytok) ;
   Annotate ("%1s(%1d)||array type")
(* ; WriteString('Field Placed in Array') ; WriteLn *)
END BuildFieldArray ;


(*
   BuildArrayComma - converts ARRAY [..], [..] OF into ARRAY [..] OF ARRAY [..]


              Ptr ->                                           <- Ptr
                     +-------------+           +-------------+
                     | ArraySym1   |           | ArraySym2   |
                     |-------------|           |-------------|
                     | ArrayName   |           | ArrayName   |
                     |-------------|           |-------------|
*)

PROCEDURE BuildArrayComma ;
VAR
   Nothing,
   ArraySym1,
   ArraySym2: CARDINAL ;
BEGIN
   PushT(NulName) ;
   StartBuildArray ;
   PopT(ArraySym2) ;
   PopT(Nothing) ;
   PushT(ArraySym2) ;
   EndBuildArray ;
   PopT(ArraySym1) ;
   PushT(ArraySym2) ;
   Annotate("%1s(%1d)||array type comma")
END BuildArrayComma ;


(*
   BuildProcedureType - builds a procedure type symbol.
                        The Stack:


                                                               <- Ptr
                                               +-------------+
                 Ptr ->                        | ProcTypeSym |
                        +-------------+        |-------------|
                        | Name        |        | Name        |
                        |-------------|        |-------------|
*)

PROCEDURE BuildProcedureType ;
VAR
   tok        : CARDINAL ;
   name       : Name ;
   ProcTypeSym: CARDINAL ;
BEGIN
   name := OperandT (1) ;
   tok := OperandTok (1) ;
   ProcTypeSym := MakeProcType (tok, name) ;
   CheckForExportedImplementation (ProcTypeSym) ;   (* May be an exported hidden type *)
   Annotate ("%1n||procedure type name") ;
   PushTtok (ProcTypeSym, tok) ;
   Annotate ("%1s(%1d)|%3d||proc type|token no")
END BuildProcedureType ;


(*
   BuildFormalType - Builds a Formal Parameter in a procedure type.

                     The Stack:

                     Entry                 Exit

              Ptr ->
                     +------------+
                     | TypeSym    |
                     |------------|
                     | Array/Nul  |
                     |------------|
                     | Var / Nul  |                         <- Ptr
                     |------------|        +--------------+
                     | ProcTypeSym|        | ProcTypeSym  |
                     |------------|        |--------------|
*)

PROCEDURE BuildFormalType ;
VAR
   tok        : CARDINAL ;
   Array, Var : Name ;
   TypeSym,
   UnboundedSym,
   ProcTypeSym: CARDINAL ;
BEGIN
   PopT(TypeSym) ;
   PopT(Array) ;
   PopT(Var) ;
   PopT(ProcTypeSym) ;
   tok := GetTokenNo () ;

   Assert( (Array=ArrayTok) OR (Array=NulTok) ) ;
   Assert(IsProcType(ProcTypeSym)) ;
   Assert( (Var=VarTok) OR (Var=NulTok) ) ;

   IF Array=ArrayTok
   THEN
      UnboundedSym := MakeUnbounded(tok, TypeSym, 1) ;
      TypeSym := UnboundedSym
   END ;
   IF Var=VarTok
   THEN
      (* VAR parameter *)
      PutProcTypeVarParam(ProcTypeSym, TypeSym, IsUnbounded(TypeSym))
   ELSE
      (* Non VAR parameter *)
      PutProcTypeParam(ProcTypeSym, TypeSym, IsUnbounded(TypeSym))
   END ;
   PushT(ProcTypeSym) ;
   Annotate("%1s(%1d)||proc type")
END BuildFormalType ;


(*
   SaveRememberedConstructor -
*)

PROCEDURE SaveRememberedConstructor ;
BEGIN
(*
   IF RememberedConstant=NulSym
   THEN
      RememberedConstant := MakeTemporary(ImmediateValue)
   END ;
   PutConstructorIntoFifoQueue(RememberedConstant)
*)

END SaveRememberedConstructor ;


(*
   GetSeenString - returns a string corresponding to, s.
*)

PROCEDURE GetSeenString (s: constType) : String ;
BEGIN
   CASE s OF

   unknown    : RETURN( InitString('unknown') ) |
   set        : RETURN( InitString('SET') ) |
   str        : RETURN( InitString('string') ) |
   constructor: RETURN( InitString('constructor') ) |
   array      : RETURN( InitString('ARRAY') ) |
   cast       : RETURN( InitStringCharStar(KeyToCharStar(GetSymName(castType))) ) |
   boolean    : RETURN( InitString('BOOLEAN') ) |
   ztype      : RETURN( InitString('Z type') ) |
   rtype      : RETURN( InitString('R type') ) |
   ctype      : RETURN( InitString('C type') ) |
   procedure  : RETURN( InitString('PROCEDURE') )

   ELSE
      InternalError ('unexpected value of type')
   END ;
   RETURN( NIL )
END GetSeenString ;


(*
   SetTypeTo - attempts to set, type, to, s.
*)

PROCEDURE SetTypeTo (s: constType) ;
VAR
   s1, s2, s3: String ;
BEGIN
   IF type=unknown
   THEN
      type := s
   ELSIF (type=constructor) AND (s#str)
   THEN
      type := s
   ELSIF (s=constructor) AND ((type=array) OR (type=set))
   THEN
      (* leave it alone *)
   ELSIF type#s
   THEN
      s1 := GetSeenString(type) ;
      s2 := GetSeenString(s) ;
      s3 := Sprintf2(InitString('cannot create a %s constant together with a %s constant'), s1, s2) ;
      ErrorStringAt(s3, GetTokenNo()) ;
      s1 := KillString(s1) ;
      s2 := KillString(s2)
   END
END SetTypeTo ;


(*
   SeenUnknown - sets the operand type to unknown.
*)

PROCEDURE SeenUnknown ;
BEGIN
   type := unknown
END SeenUnknown ;


(*
   SeenCast - sets the operand type to cast.
*)

PROCEDURE SeenCast (sym: CARDINAL) ;
BEGIN
   type := cast ;
   castType := sym ;
   Assert(IsAModula2Type(sym)) ;
END SeenCast ;


(*
   SeenBoolean - sets the operand type to a BOOLEAN.
*)

PROCEDURE SeenBoolean ;
BEGIN
   type := boolean
END SeenBoolean ;


(*
   SeenZType - sets the operand type to a Z type.
*)

PROCEDURE SeenZType ;
BEGIN
   type := ztype
END SeenZType ;


(*
   SeenRType - sets the operand type to a R type.
*)

PROCEDURE SeenRType ;
BEGIN
   type := rtype
END SeenRType ;


(*
   SeenCType - sets the operand type to a C type.
*)

PROCEDURE SeenCType ;
BEGIN
   type := ctype
END SeenCType ;


(*
   SeenSet - sets the operand type to set.
*)

PROCEDURE SeenSet ;
BEGIN
   SetTypeTo(set) ;
   SaveRememberedConstructor
END SeenSet ;


(*
   SeenArray - sets the operand type to array.
*)

PROCEDURE SeenArray ;
BEGIN
   SetTypeTo(array)
END SeenArray ;


(*
   SeenConstructor - sets the operand type to constructor.
*)

PROCEDURE SeenConstructor ;
BEGIN
   SetTypeTo(constructor) ;
   SaveRememberedConstructor
END SeenConstructor ;


(*
   SeenString - sets the operand type to string.
*)

PROCEDURE SeenString ;
BEGIN
   SetTypeTo(str)
END SeenString ;


(*
   DetermineType - assigns the top of stack symbol with the type of
                   constant expression, if known.
*)

PROCEDURE DetermineType ;
VAR
   Sym: CARDINAL ;
BEGIN
   Sym := OperandT(1) ;
   CASE type OF

   set        :  PutConstSet(Sym) |
   str        :  PutConstStringKnown (GetTokenNo(), Sym, MakeKey(''), FALSE, FALSE) |
   array,
   constructor:  PutConstructor(Sym) |
   cast       :  PutConst(Sym, castType) |
   unknown    :

   ELSE
   END
END DetermineType ;


(*
   PushType -
*)

PROCEDURE PushType ;
BEGIN
   PushWord(TypeStack, type)
END PushType ;


(*
   PopType -
*)

PROCEDURE PopType ;
BEGIN
   type := PopWord(TypeStack)
END PopType ;


(*
   PushRememberConstant -
*)

PROCEDURE PushRememberConstant ;
BEGIN
   PushWord(RememberStack, RememberedConstant) ;
   RememberConstant(NulSym)
END PushRememberConstant ;


(*
   PopRememberConstant -
*)

PROCEDURE PopRememberConstant ;
BEGIN
   RememberedConstant := PopWord(RememberStack)
END PopRememberConstant ;


(*
   RememberConstant -
*)

PROCEDURE RememberConstant (sym: CARDINAL) ;
BEGIN
   RememberedConstant := sym
END RememberConstant ;


BEGIN
   alignTypeNo := 0 ;
   TypeStack := InitStackWord () ;
   RememberStack := InitStackWord () ;
   BlockStack := InitStackWord () ;
   castType := NulSym
END P2SymBuild.
