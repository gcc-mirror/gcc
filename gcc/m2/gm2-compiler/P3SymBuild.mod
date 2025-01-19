(* P3SymBuild.mod pass 3 symbol creation.

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

IMPLEMENTATION MODULE P3SymBuild ;


FROM NameKey IMPORT Name, WriteKey, NulName ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2Error IMPORT WriteFormat0, WriteFormat1, WriteFormat2, FlushErrors, InternalError ;
FROM M2LexBuf IMPORT GetTokenNo ;

FROM SymbolTable IMPORT NulSym, ModeOfAddr, ProcedureKind,
                        StartScope, EndScope, GetScope, GetCurrentScope,
                        GetModuleScope,
                        SetCurrentModule, GetCurrentModule, SetFileModule,
                        GetExported, IsExported, IsImplicityExported,
                        IsDefImp, IsModule, IsImported, IsIncludedByDefinition,
                        RequestSym,
                        IsProcedure, PutOptArgInit,
                        IsFieldEnumeration, GetType,
                        CheckForUnknownInModule,
                        GetFromOuterModule,
                        GetMode, PutVariableAtAddress, ModeOfAddr, SkipType,
                        IsSet, PutConstSet,
                        IsConst, IsConstructor, PutConst, PutConstructor,
                        PopValue, PushValue,
                        MakeTemporary, PutVar,
                        PutSubrange, GetProcedureKind,
                        GetSymName ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource,
                    LookupOuterModule ;

FROM M2Quads IMPORT PushT, PopT, OperandT, PopN, PopTF, PushTF,
                    PopTtok, PopTFtok, PushTtok, PushTFtok, OperandTok ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

FROM FifoQueue IMPORT GetSubrangeFromFifoQueue ;
FROM M2Reserved IMPORT NulTok, ImportTok ;
IMPORT M2Error ;


(*
   StartBuildDefinitionModule - Creates a definition module and starts
                                a new scope.

                                The Stack is expected:

                                Entry                 Exit

                         Ptr ->                                     <- Ptr
                                +------------+        +-----------+
                                | NameStart  |        | NameStart |
                                |------------|        |-----------|

*)

PROCEDURE P3StartBuildDefModule ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopTtok (name, tok) ;
   ModuleSym := MakeDefinitionSource (tok, name) ;
   SetCurrentModule (ModuleSym) ;
   SetFileModule (ModuleSym) ;
   StartScope (ModuleSym) ;
   Assert (IsDefImp (ModuleSym)) ;
   Assert (CompilingDefinitionModule ()) ;
   PushT (name) ;
   M2Error.EnterDefinitionScope (name)
END P3StartBuildDefModule ;


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

PROCEDURE P3EndBuildDefModule ;
VAR
   NameStart,
   NameEnd  : CARDINAL ;
BEGIN
   Assert(CompilingDefinitionModule()) ;
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      WriteFormat2('inconsistant definition module was named (%a) and concluded as (%a)',
                   NameStart, NameEnd)
   END ;
   M2Error.LeaveErrorScope
END P3EndBuildDefModule ;


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

PROCEDURE P3StartBuildImpModule ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopTtok (name, tok) ;
   ModuleSym := MakeImplementationSource (tok, name) ;
   SetCurrentModule (ModuleSym) ;
   SetFileModule (ModuleSym) ;
   StartScope (ModuleSym) ;
   Assert (IsDefImp(ModuleSym)) ;
   Assert (CompilingImplementationModule()) ;
   PushT (name) ;
   M2Error.EnterImplementationScope (name)
END P3StartBuildImpModule ;


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

PROCEDURE P3EndBuildImpModule ;
VAR
   NameStart,
   NameEnd  : Name ;
BEGIN
   Assert(CompilingImplementationModule()) ;
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      (* we dont issue an error based around incorrect module names as this is done in P1 and P2.
         If we get here then something has gone wrong with our error recovery in P3, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   M2Error.LeaveErrorScope
END P3EndBuildImpModule ;


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

PROCEDURE P3StartBuildProgModule ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   (* WriteString('StartBuildProgramModule') ; WriteLn ; *)
   PopTtok(name, tok) ;
   ModuleSym := MakeProgramSource(tok, name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   (* WriteString('MODULE - ') ; WriteKey(GetSymName(ModuleSym)) ; WriteLn ; *)
   StartScope(ModuleSym) ;
   Assert(CompilingProgramModule()) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   PushT(name) ;
   M2Error.EnterProgramScope (name)
END P3StartBuildProgModule ;


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

PROCEDURE P3EndBuildProgModule ;
VAR
   NameStart,
   NameEnd  : Name ;
BEGIN
   Assert(CompilingProgramModule()) ;
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      (* we dont issue an error based around incorrect module names this would be done in P1 and P2.
         If we get here then something has gone wrong with our error recovery in P3, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   M2Error.LeaveErrorScope
END P3EndBuildProgModule ;


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
   ModuleSym := RequestSym (tok, name) ;
   Assert(IsModule(ModuleSym)) ;
   StartScope(ModuleSym) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   SetCurrentModule(ModuleSym) ;
   PushT(name) ;
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
   CheckForUnknownInModule ;
   EndScope ;
   PopT(NameEnd) ;
   PopT(NameStart) ;
   IF NameStart#NameEnd
   THEN
      (* we dont issue an error based around incorrect module names this would be done in P1 and P2.
         If we get here then something has gone wrong with our error recovery in P3, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   SetCurrentModule(GetModuleScope(GetCurrentModule())) ;
   M2Error.LeaveErrorScope
END EndBuildInnerModule ;


(*
   CheckImportListOuterModule - checks to see that all identifiers are
                                exported from the definition module.

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


                                                      Error Condition
                                Exit

                                All above stack discarded
*)

PROCEDURE CheckImportListOuterModule ;
VAR
   n1, n2     : Name ;
   tok        : CARDINAL ;
   ModSym,
   i, n       : CARDINAL ;
BEGIN
   PopT(n) ;       (* n   = # of the Ident List *)
   IF OperandT(n+1)#ImportTok
   THEN
      (* Ident List contains list of objects *)
      ModSym := LookupOuterModule(OperandTok(n+1), OperandT(n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         tok := OperandTok (i) ;
         IF (NOT IsExported(ModSym, RequestSym (tok, OperandT (i)))) AND
            (NOT IsImplicityExported(ModSym, RequestSym (tok, OperandT(i))))
         THEN
            n1 := OperandT(n+1) ;
            n2 := OperandT(i) ;
            WriteFormat2 ('symbol %a is not exported from definition or inner module %a', n2, n1)
         END ;
         INC(i)
      END
   END ;
   PopN(n+1)   (* clear stack *)
END CheckImportListOuterModule ;


(*
   CheckCanBeImported - checks to see that it is legal to import, Sym, from, ModSym.
*)

PROCEDURE CheckCanBeImported (ModSym, Sym: CARDINAL) ;
VAR
   n1, n2: Name ;
BEGIN
   IF IsDefImp(ModSym)
   THEN
      IF IsExported(ModSym, Sym)
      THEN
         (* great all done *)
         RETURN
      ELSE
         IF IsImplicityExported(ModSym, Sym)
         THEN
            (* this is also legal *)
            RETURN
         ELSIF IsDefImp(Sym) AND IsIncludedByDefinition(ModSym, Sym)
         THEN
            (* this is also legal (for a definition module) *)
            RETURN
         END ;
         n1 := GetSymName(ModSym) ;
         n2 := GetSymName(Sym) ;
         WriteFormat2('symbol %a is not exported from definition module %a', n2, n1)
      END
   END
END CheckCanBeImported ;


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
   tok     : CARDINAL ;
   name    : Name ;
   ProcSym : CARDINAL ;
BEGIN
   PopTtok (name, tok) ;
   PushTtok (name, tok) ;  (* Name saved for the EndBuildProcedure name check *)
   ProcSym := RequestSym (tok, name) ;
   Assert (IsProcedure (ProcSym)) ;
   PushTtok (ProcSym, tok) ;
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
   ProcSym  : CARDINAL ;
   NameEnd,
   NameStart: Name ;
BEGIN
   PopT(NameEnd) ;
   PopT(ProcSym) ;
   PopT(NameStart) ;
   IF NameEnd#NameStart
   THEN
      (* we dont issue an error based around incorrect module names this would be done in P1 and P2.
         If we get here then something has gone wrong with our error recovery in P3, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   EndScope ;
   M2Error.LeaveErrorScope
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
   EndBuildForward -
*)

PROCEDURE EndBuildForward ;
BEGIN
   PopN (2) ;
   EndScope ;
   M2Error.LeaveErrorScope
END EndBuildForward ;


(*
   BuildSubrange - Builds a Subrange type Symbol.


                      Stack

                      Entry                 Exit

               Ptr ->
                      +------------+
                      | High       |
                      |------------|
                      | Low        |                       <- Ptr
                      |------------|
*)

PROCEDURE BuildSubrange ;
VAR
   Base,
   Type,
   Low,
   High: CARDINAL ;
BEGIN
   PopT(High) ;
   PopT(Low) ;
   GetSubrangeFromFifoQueue(Type) ;  (* Collect subrange type from pass 2 and fill in *)
                                     (* bounds.                                       *)
   GetSubrangeFromFifoQueue(Base) ;  (* Get base of subrange (maybe NulSym)           *)
(*
   WriteString('Subrange type name is: ') ; WriteKey(GetSymName(Type)) ; WriteLn ;
   WriteString('Subrange High is: ') ; WriteKey(GetSymName(High)) ;
   WriteString(' Low is: ') ; WriteKey(GetSymName(Low)) ; WriteLn ;
*)
   PutSubrange(Type, Low, High, Base)   (* if Base is NulSym then it is       *)
                                        (* worked out later in M2GCCDeclare   *)
END BuildSubrange ;


(*
   BuildNulName - Pushes a NulKey onto the top of the stack.
                  The Stack:


                  Entry                    Exit

                                                          <- Ptr
                  Empty                    +------------+
                                           | NulKey     |
                                           |------------|
*)

PROCEDURE BuildNulName ;
BEGIN
   PushT(NulName)
END BuildNulName ;


(*
   BuildConst - builds a constant.
                Stack

                Entry                 Exit

         Ptr ->                                      <- Ptr
                +------------+        +------------+
                | Name       |        | Sym        |
                |------------+        |------------|
*)

PROCEDURE BuildConst ;
VAR
   name: Name ;
   tok : CARDINAL ;
   Sym : CARDINAL ;
BEGIN
   PopTtok (name, tok) ;
   Sym := RequestSym (tok, name) ;
   PushTtok (Sym, tok)
END BuildConst ;


(*
   BuildVarAtAddress - updates the symbol table entry of, variable sym, to be declared
                       at address, address.

                       Stack

                       Entry                 Exit

                Ptr ->
                       +--------------+
                       | Expr | EType |                         <- Ptr
                       |--------------+        +--------------+
                       | name | SType |        | name | SType |
                       |--------------+        |--------------|
*)

PROCEDURE BuildVarAtAddress ;
VAR
   nametok   : CARDINAL ;
   name      : Name ;
   Sym, SType,
   Exp, EType: CARDINAL ;
BEGIN
   PopTF(Exp, EType) ;
   PopTFtok (name, SType, nametok) ;
   PushTF(name, SType) ;
   Sym := RequestSym (nametok, name) ;
   IF GetMode(Sym)=LeftValue
   THEN
      PutVariableAtAddress(Sym, Exp)
   ELSE
      InternalError ('expecting lvalue for this variable which is declared at an explicit address')
   END
END BuildVarAtAddress ;


(*
   BuildOptArgInitializer - assigns the constant value symbol, const, to be the
                            initial value of the optional parameter should it be
                            absent.

                            Ptr ->
                                   +------------+
                                   | const      |
                                   |------------|                      <- Ptr
*)

PROCEDURE BuildOptArgInitializer ;
VAR
   tok    : CARDINAL ;
   const,
   ProcSym: CARDINAL ;
BEGIN
   PopT (const) ;
   PopTtok (ProcSym, tok) ;
   Assert (IsProcedure (ProcSym)) ;
   PushTtok (ProcSym, tok) ;
   PutOptArgInit (GetCurrentScope (), const)
END BuildOptArgInitializer ;


END P3SymBuild.
