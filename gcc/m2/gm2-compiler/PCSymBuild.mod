(* PCSymBuild.mod pass C symbol creation.

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

IMPLEMENTATION MODULE PCSymBuild ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM NameKey IMPORT Name, WriteKey, MakeKey, NulName ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM M2Error IMPORT WriteFormat0, WriteFormat1, WriteFormat2, FlushErrors, InternalError, NewError, ErrorFormat0 ;
FROM M2MetaError IMPORT MetaError1, MetaErrorT1 ;
FROM M2LexBuf IMPORT GetTokenNo ;
FROM M2Reserved IMPORT NulTok, ImportTok ;
FROM M2Const IMPORT constType ;
FROM Indexing IMPORT Index, InitIndex, GetIndice, PutIndice, InBounds, IncludeIndiceIntoIndex, HighIndice ;

FROM M2Quads IMPORT PushT, PopT, OperandT, PopN, PopTF, PushTF, IsAutoPushOn,
                    PopNothing, PushTFn, PopTFn, PushTtok, PopTtok, PushTFtok, PopTFtok, OperandTok ;

FROM M2Options IMPORT Iso ;
FROM StdIO IMPORT Write ;
FROM M2System IMPORT Cast, IsPseudoSystemFunctionConstExpression ;

FROM M2Base IMPORT MixTypes,
                   ZType, RType, Char, Boolean, Val, Max, Min, Convert,
                   IsPseudoBaseFunction, IsRealType, IsComplexType, IsOrdinalType ;

FROM M2Reserved IMPORT PlusTok, MinusTok, TimesTok, DivTok, ModTok,
                       DivideTok, RemTok,
                       OrTok, AndTok, AmbersandTok,
                       EqualTok, LessEqualTok, GreaterEqualTok,
                       LessTok, GreaterTok, HashTok, LessGreaterTok,
                       InTok, NotTok ;

FROM SymbolTable IMPORT NulSym, ModeOfAddr,
                        StartScope, EndScope, GetScope, GetCurrentScope,
                        GetModuleScope,
                        SetCurrentModule, GetCurrentModule, SetFileModule,
                        GetExported,
                        IsDefImp, IsModule,
                        RequestSym,
                        IsProcedure, PutOptArgInit, IsEnumeration,
                        CheckForUnknownInModule,
                        GetFromOuterModule,
                        CheckForEnumerationInCurrentModule,
                        GetMode, PutVariableAtAddress, ModeOfAddr, SkipType,
                        IsSet, PutConstSet,
                        IsConst, IsConstructor, PutConst, PutConstructor,
                        PopValue, PushValue,
                        MakeTemporary, PutVar,
                        PutSubrange,
                        GetSymName,
                        CheckAnonymous,
                        IsProcedureBuiltin,
                        MakeProcType,
                        NoOfParam,
                        GetParam,
                        IsParameterVar, PutProcTypeParam,
                        PutProcTypeVarParam, IsParameterUnbounded,
                        PutFunction, PutProcTypeParam,
                        GetType, IsVar,
                        IsAModula2Type, GetDeclaredMod ;

FROM M2Batch IMPORT MakeDefinitionSource,
                    MakeImplementationSource,
                    MakeProgramSource,
                    LookupModule, LookupOuterModule ;

FROM M2Comp IMPORT CompilingDefinitionModule,
                   CompilingImplementationModule,
                   CompilingProgramModule ;

FROM M2StackAddress IMPORT StackOfAddress, InitStackAddress, KillStackAddress,
                           PushAddress, PopAddress, PeepAddress,
                           IsEmptyAddress, NoOfItemsInStackAddress ;

FROM M2StackWord IMPORT StackOfWord, InitStackWord, KillStackWord,
                        PushWord, PopWord, PeepWord,
                        IsEmptyWord, NoOfItemsInStackWord ;

IMPORT M2Error ;


CONST
   Debugging = FALSE ;

TYPE
   tagType = (leaf, unary, binary, designator, expr, convert, function) ;

   exprNode = POINTER TO eNode ;

   eDes = RECORD
             type: CARDINAL ;
             meta: constType ;
             sym : CARDINAL ;
             left: exprNode ;
          END ;

   eLeaf = RECORD
              type: CARDINAL ;
              meta: constType ;
              sym: CARDINAL ;
           END ;

   eUnary = RECORD
               type: CARDINAL ;
               meta: constType ;
               left: exprNode ;
               op  : Name ;
            END ;

   eBinary = RECORD
                type: CARDINAL ;
                meta: constType ;
                left,
                right: exprNode ;
                op   : Name ;
             END ;

   eExpr = RECORD
              type: CARDINAL ;
              meta: constType ;
              left: exprNode ;
           END ;

   eFunction = RECORD
                  type  : CARDINAL ;
                  meta  : constType ;
                  func  : CARDINAL ;
                  first,
                  second: exprNode ;
                  third : BOOLEAN ;
               END ;

   eConvert = RECORD
                 type  : CARDINAL ;
                 meta  : constType ;
                 totype: exprNode ;
                 expr  : exprNode ;
              END ;

   eNode    = RECORD
                 CASE tag: tagType OF

                 designator:  edes     : eDes |
                 leaf      :  eleaf    : eLeaf |
                 unary     :  eunary   : eUnary |
                 binary    :  ebinary  : eBinary |
                 expr      :  eexpr    : eExpr |
                 function  :  efunction: eFunction |
                 convert   :  econvert : eConvert

                 END
              END ;


VAR
   exprStack   : StackOfAddress ;
   constList   : Index ;
   constToken  : CARDINAL ;
   desStack    : StackOfWord ;
   inDesignator: BOOLEAN ;


(*
   GetSkippedType -
*)

PROCEDURE GetSkippedType (sym: CARDINAL) : CARDINAL ;
BEGIN
   RETURN( SkipType(GetType(sym)) )
END GetSkippedType ;


(*
   CheckNotVar - checks to see that the top of stack is not a variable.
*)

PROCEDURE CheckNotVar (tok: CARDINAL) ;
VAR
   const: CARDINAL ;
BEGIN
   const := OperandT (1) ;
   IF (const # NulSym) AND IsVar (const)
   THEN
      MetaErrorT1 (tok, 'not expecting a variable {%Aad} as a term in a constant expression', const)
   END
END CheckNotVar ;


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

PROCEDURE PCStartBuildDefModule ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopTtok(name, tok) ;
   ModuleSym := MakeDefinitionSource(tok, name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingDefinitionModule()) ;
   PushT(name) ;
   M2Error.EnterDefinitionScope (name)
END PCStartBuildDefModule ;


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

PROCEDURE PCEndBuildDefModule ;
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
END PCEndBuildDefModule ;


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

PROCEDURE PCStartBuildImpModule ;
VAR
   tok      : CARDINAL ;
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopTtok(name, tok) ;
   ModuleSym := MakeImplementationSource(tok, name) ;
   SetCurrentModule(ModuleSym) ;
   SetFileModule(ModuleSym) ;
   StartScope(ModuleSym) ;
   Assert(IsDefImp(ModuleSym)) ;
   Assert(CompilingImplementationModule()) ;
   PushTtok(name, tok) ;
   M2Error.EnterImplementationScope (name)
END PCStartBuildImpModule ;


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

PROCEDURE PCEndBuildImpModule ;
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
         If we get here then something has gone wrong with our error recovery in PC, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   M2Error.LeaveErrorScope
END PCEndBuildImpModule ;


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

PROCEDURE PCStartBuildProgModule ;
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
   PushTtok(name, tok) ;
   M2Error.EnterProgramScope (name)
END PCStartBuildProgModule ;


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

PROCEDURE PCEndBuildProgModule ;
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
         If we get here then something has gone wrong with our error recovery in PC, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   M2Error.LeaveErrorScope
END PCEndBuildProgModule ;


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

PROCEDURE PCStartBuildInnerModule ;
VAR
   name     : Name ;
   tok      : CARDINAL ;
   ModuleSym: CARDINAL ;
BEGIN
   PopTtok(name, tok) ;
   ModuleSym := RequestSym(tok, name) ;
   Assert(IsModule(ModuleSym)) ;
   StartScope(ModuleSym) ;
   Assert(NOT IsDefImp(ModuleSym)) ;
   SetCurrentModule(ModuleSym) ;
   PushTtok(name, tok) ;
   M2Error.EnterModuleScope (name)
END PCStartBuildInnerModule ;


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

PROCEDURE PCEndBuildInnerModule ;
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
         If we get here then something has gone wrong with our error recovery in PC, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   SetCurrentModule(GetModuleScope(GetCurrentModule())) ;
   M2Error.LeaveErrorScope
END PCEndBuildInnerModule ;


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

PROCEDURE PCBuildImportOuterModule ;
VAR
   Sym, ModSym,
   i, n       : CARDINAL ;
BEGIN
   PopT (n) ;       (* n   = # of the Ident List *)
   IF OperandT (n+1) # ImportTok
   THEN
      (* Ident List contains list of objects imported from ModSym *)
      ModSym := LookupModule (OperandTok (n+1), OperandT (n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetExported (OperandTok (i), ModSym, OperandT (i)) ;
         CheckForEnumerationInCurrentModule (Sym) ;
         INC (i)
      END
   END ;
   PopN (n+1)   (* clear stack *)
END PCBuildImportOuterModule ;


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

PROCEDURE PCBuildImportInnerModule ;
VAR
   Sym, ModSym,
   n, i       : CARDINAL ;
BEGIN
   PopT (n) ;       (* i   = # of the Ident List *)
   IF OperandT (n+1) = ImportTok
   THEN
      (* Ident List contains list of objects *)
      i := 1 ;
      WHILE i<=n DO
         Sym := GetFromOuterModule (OperandTok (i), OperandT (i)) ;
         CheckForEnumerationInCurrentModule (Sym) ;
         INC (i)
      END
   ELSE
      (* Ident List contains list of objects imported from ModSym *)
      ModSym := LookupOuterModule (OperandTok (n+1), OperandT (n+1)) ;
      i := 1 ;
      WHILE i<=n DO
         Sym := GetExported (OperandTok (i), ModSym, OperandT (i)) ;
         CheckForEnumerationInCurrentModule (Sym) ;
         INC (i)
      END
   END ;
   PopN (n+1)   (* Clear Stack *)
END PCBuildImportInnerModule ;


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

PROCEDURE PCStartBuildProcedure ;
VAR
   name    : Name ;
   ProcSym : CARDINAL ;
   tok     : CARDINAL ;
BEGIN
   PopTtok(name, tok) ;
   PushTtok(name, tok) ;  (* Name saved for the EndBuildProcedure name check *)
   ProcSym := RequestSym (tok, name) ;
   Assert (IsProcedure (ProcSym)) ;
   PushTtok (ProcSym, tok) ;
   StartScope (ProcSym) ;
   M2Error.EnterProcedureScope (name)
END PCStartBuildProcedure ;


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

PROCEDURE PCEndBuildProcedure ;
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
         If we get here then something has gone wrong with our error recovery in PC, so we bail out.
      *)
      WriteFormat0('too many errors in pass 3') ;
      FlushErrors
   END ;
   EndScope ;
   M2Error.LeaveErrorScope
END PCEndBuildProcedure ;


(*
   EndBuildForward - Ends building a forward declaration.

                     The Stack:

                     Entry                 Exit

              Ptr ->
                     +------------+
                     | ProcSym    |
                     |------------|
                     | NameStart  |
                     |------------|
                                           Empty
*)

PROCEDURE PCEndBuildForward ;
BEGIN
   PopN (2)
END PCEndBuildForward ;


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

PROCEDURE PCBuildProcedureHeading ;
VAR
   ProcSym  : CARDINAL ;
   NameStart: Name ;
BEGIN
   IF CompilingDefinitionModule ()
   THEN
      PopT (ProcSym) ;
      PopT (NameStart) ;
      EndScope
   END
END PCBuildProcedureHeading ;


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
   PushT (NulName)
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

(*
PROCEDURE BuildVarAtAddress ;
VAR
   name      : Name ;
   Sym, SType,
   Exp, EType: CARDINAL ;
   etok, ntok: CARDINAL ;
BEGIN
   PopTFtok (Exp, EType, etok) ;
   PopTFtok (name, SType, ntok) ;
   PushTFtok (name, SType, ntok) ;
   Sym := RequestSym (ntok, name) ;
   IF GetMode(Sym)=LeftValue
   THEN
      PutVariableAtAddress(Sym, Exp)
   ELSE
      InternalError ('expecting lvalue for this variable which is declared at an explicit address')
   END
END BuildVarAtAddress ;
*)


(*
   BuildOptArgInitializer - assigns the constant value symbol, const, to be the
                            initial value of the optional parameter should it be
                            absent.

                            Ptr ->
                                   +------------+
                                   | const      |
                                   |------------|                      <- Ptr
*)

(*
PROCEDURE BuildOptArgInitializer ;
VAR
   const: CARDINAL ;
BEGIN
   PopT(const) ;
   PutOptArgInit(GetCurrentScope(), const)
END BuildOptArgInitializer ;
*)


(*
   InitDesExpr -
*)

PROCEDURE InitDesExpr (des: CARDINAL) ;
VAR
   e: exprNode ;
BEGIN
   NEW (e) ;
   WITH e^ DO
      tag := designator ;
      CASE tag OF

      designator:  WITH edes DO
                      type := NulSym ;
                      meta := unknown ;
                      tag := designator ;
                      sym := des ;
                      left := NIL
                   END

      ELSE
         InternalError ('expecting designator')
      END
   END ;
   PushAddress (exprStack, e)
END InitDesExpr ;


(*
   DebugNode -
*)

PROCEDURE DebugNode (d: exprNode) ;
BEGIN
   IF Debugging AND (d#NIL)
   THEN
      WITH d^ DO
         CASE tag OF

         designator:  DebugDes(d) |
         expr      :  DebugExpr(d) |
         leaf      :  DebugLeaf(d) |
         unary     :  DebugUnary(d) |
         binary    :  DebugBinary(d) |
         function  :  DebugFunction(d) |
         convert   :  DebugConvert(d)

         END
      END
   END
END DebugNode ;


(*
   DebugDes -
*)

PROCEDURE DebugDes (d: exprNode) ;
BEGIN
   WITH d^ DO
      WITH edes DO
         DebugSym(sym) ; Write(':') ; DebugMeta(meta) ; Write(':') ; DebugType(type) ;
         WriteString(' = ') ;
         DebugNode(left) ;
         WriteLn
      END
   END
END DebugDes ;


(*
   DebugSym -
*)

PROCEDURE DebugSym (sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   n := GetSymName(sym) ;
   IF n#NulName
   THEN
      WriteKey(n)
   END ;
   Write(':') ; WriteCard(sym, 0)
END DebugSym ;


(*
   DebugMeta -
*)

PROCEDURE DebugMeta (m: constType) ;
BEGIN
   CASE m OF

   unknown    :  WriteString('unknown') |
   set        :  WriteString('set') |
   str        :  WriteString('str') |
   constructor:  WriteString('constructor') |
   array      :  WriteString('array') |
   cast       :  WriteString('cast') |
   boolean    :  WriteString('boolean') |
   ztype      :  WriteString('ztype') |
   rtype      :  WriteString('rtype') |
   ctype      :  WriteString('ctype') |
   procedure  :  WriteString('procedure') |
   char       :  WriteString('ctype')

   END
END DebugMeta ;


(*
   DebugType -
*)

PROCEDURE DebugType (type: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   WriteString('[type:') ;
   IF type=NulSym
   THEN
      WriteString('<nulsym>')
   ELSE
      n := GetSymName(type) ;
      IF n#NulSym
      THEN
         WriteKey(n)
      END ;
      Write(':') ; WriteCard(type, 0)
   END ;
   Write(']')
END DebugType ;


(*
   DebugExpr -
*)

PROCEDURE DebugExpr (e: exprNode) ;
BEGIN
   WITH e^.eexpr DO
      WriteString('expr (') ;
      DebugType(type) ; Write(':') ;
      DebugMeta(meta) ; Write(' ') ;
      DebugNode(left) ;
      WriteString(') ')
   END
END DebugExpr ;


(*
   DebugFunction -
*)

PROCEDURE DebugFunction (f: exprNode) ;
BEGIN
   WITH f^.efunction DO
      WriteKey(GetSymName(func)) ;
      Write('(') ;
      IF first#NIL
      THEN
         DebugNode(first) ;
         IF second#NIL
         THEN
            WriteString(', ') ;
            DebugNode(second) ;
            IF third
            THEN
               WriteString(', ...')
            END
         END
      END ;
      Write(')')
   END
END DebugFunction ;


(*
   DebugConvert -
*)

PROCEDURE DebugConvert (f: exprNode) ;
BEGIN
   WITH f^.econvert DO
      DebugNode(totype) ;
      Write('(') ;
      DebugNode(expr) ;
      Write(')')
   END
END DebugConvert ;


(*
   DebugLeaf -
*)

PROCEDURE DebugLeaf (l: exprNode) ;
BEGIN
   WITH l^.eleaf DO
      WriteString('leaf (') ;
      DebugType(type) ; Write(':') ;
      DebugMeta(meta) ; Write(':') ;
      DebugSym(sym) ;
      WriteString(') ')
   END
END DebugLeaf ;


(*
   DebugUnary -
*)

PROCEDURE DebugUnary (l: exprNode) ;
BEGIN
   WITH l^.eunary DO
      WriteString('unary (') ;
      DebugType(type) ; Write(':') ;
      DebugMeta(meta) ; Write(' ') ;
      DebugOp(op) ; Write(' ') ;
      DebugNode(left) ;
      WriteString(') ')
   END
END DebugUnary ;


(*
   DebugBinary -
*)

PROCEDURE DebugBinary (l: exprNode) ;
BEGIN
   WITH l^.ebinary DO
      WriteString('unary (') ;
      DebugType(type) ; Write(':') ;
      DebugMeta(meta) ; Write(' ') ;
      DebugNode(left) ;
      DebugOp(op) ; Write(' ') ;
      DebugNode(right) ;
      WriteString(') ')
   END
END DebugBinary ;


(*
   DebugOp -
*)

PROCEDURE DebugOp (op: Name) ;
BEGIN
   WriteKey(op)
END DebugOp ;


(*
   PushInConstructor -
*)

PROCEDURE PushInConstructor ;
BEGIN
   PushWord(desStack, inDesignator) ;
   inDesignator := FALSE
END PushInConstructor ;


(*
   PopInConstructor -
*)

PROCEDURE PopInConstructor ;
BEGIN
   inDesignator := PopWord(desStack)
END PopInConstructor ;


(*
   StartDesConst -
*)

PROCEDURE StartDesConst ;
VAR
   name: Name ;
   tok : CARDINAL ;
BEGIN
   inDesignator := TRUE ;
   exprStack := KillStackAddress (exprStack) ;
   exprStack := InitStackAddress () ;
   PopTtok (name, tok) ;
   InitDesExpr (RequestSym (tok, name))
END StartDesConst ;


(*
   EndDesConst -
*)

PROCEDURE EndDesConst ;
VAR
   d, e: exprNode ;
BEGIN
   e := PopAddress (exprStack) ;
   d := PopAddress (exprStack) ;
   Assert(d^.tag=designator) ;
   d^.edes.left := e ;
   IncludeIndiceIntoIndex(constList, d) ;
   inDesignator := FALSE
END EndDesConst ;


(*
   fixupProcedureType - creates a proctype from a procedure.
*)

PROCEDURE fixupProcedureType (p: CARDINAL) : CARDINAL ;
VAR
   tok : CARDINAL ;
   par,
   t   : CARDINAL ;
   n, i: CARDINAL ;
BEGIN
   IF IsProcedure(p)
   THEN
      tok := GetTokenNo () ;
      t := MakeProcType (tok, CheckAnonymous (NulName)) ;
      i := 1 ;
      n := NoOfParam(p) ;
      WHILE i<=n DO
         par := GetParam (p, i) ;
         IF IsParameterVar (par)
         THEN
            PutProcTypeVarParam (t, GetType (par), IsParameterUnbounded (par))
         ELSE
            PutProcTypeParam (t, GetType (par), IsParameterUnbounded (par))
         END ;
         INC(i)
      END ;
      IF GetType (p) # NulSym
      THEN
         PutFunction (t, GetType (p))
      END ;
      RETURN( t )
   ELSE
      InternalError ('expecting a procedure')
   END ;
   RETURN( NulSym )
END fixupProcedureType ;


(*
   InitFunction -
*)

PROCEDURE InitFunction (m: constType; p, t: CARDINAL; f, s: exprNode; more: BOOLEAN) ;
VAR
   n: exprNode ;
BEGIN
   NEW (n) ;
   WITH n^ DO
      tag := function ;
      CASE tag OF

      function:  WITH efunction DO
                    meta := m ;
                    type := t ;
                    func := p ;
                    first := f ;
                    second := s ;
                    third := more
                 END

      ELSE
         InternalError ('expecting function')
      END
   END ;
   PushAddress (exprStack, n)
END InitFunction ;


(*
   InitConvert -
*)

PROCEDURE InitConvert (m: constType; t: CARDINAL; to, e: exprNode) ;
VAR
   n: exprNode ;
BEGIN
   NEW(n) ;
   WITH n^ DO
      tag := convert ;
      CASE tag OF

      convert:  WITH econvert DO
                   type := t ;
                   meta := m ;
                   totype := to ;
                   expr := e
                 END

      ELSE
         InternalError ('expecting convert')
      END
   END ;
   PushAddress(exprStack, n)
END InitConvert ;


(*
   InitLeaf -
*)

PROCEDURE InitLeaf (m: constType; s, t: CARDINAL) ;
VAR
   l: exprNode ;
BEGIN
   NEW (l) ;
   WITH l^ DO
      tag := leaf ;
      CASE tag OF

      leaf:  WITH eleaf DO
                type := t ;
                meta := m ;
                sym := s
             END

      ELSE
         InternalError ('expecting leaf')
      END
   END ;
   PushAddress (exprStack, l)
END InitLeaf ;


(*
   InitProcedure -
*)

PROCEDURE InitProcedure (s: CARDINAL) ;
BEGIN
   InitLeaf(procedure, s, fixupProcedureType(s))
END InitProcedure ;


(*
   InitCharType -
*)

PROCEDURE InitCharType (s: CARDINAL) ;
BEGIN
   InitLeaf(char, s, Char)
END InitCharType ;


(*
   InitZType -
*)

PROCEDURE InitZType (s: CARDINAL) ;
BEGIN
   InitLeaf(ztype, s, ZType)
END InitZType ;


(*
   InitRType -
*)

PROCEDURE InitRType (s: CARDINAL) ;
BEGIN
   InitLeaf(rtype, s, RType)
END InitRType ;


(*
   InitUnknown -
*)

PROCEDURE InitUnknown (s: CARDINAL) ;
BEGIN
   InitLeaf(unknown, s, NulSym)
END InitUnknown ;


(*
   InitBooleanType -
*)

PROCEDURE InitBooleanType (s: CARDINAL) ;
BEGIN
   InitLeaf(boolean, s, Boolean)
END InitBooleanType ;


(*
   PushConstType - pushes a constant to the expression stack.
*)

PROCEDURE PushConstType ;
VAR
   c: CARDINAL ;
BEGIN
   PopT(c) ;
   PushT(c) ;
   IF inDesignator
   THEN
      IF c=NulSym
      THEN
         WriteFormat0('module or symbol in qualident is not known') ;
         FlushErrors ;
         InitUnknown(c)
      ELSIF IsProcedure(c)
      THEN
         InitProcedure(c)
      ELSIF GetSkippedType(c)=RType
      THEN
         InitRType(c)
      ELSIF GetSkippedType(c)=ZType
      THEN
         InitZType(c)
      ELSIF GetSkippedType(c)=Boolean
      THEN
         InitBooleanType(c)
      ELSE
         InitUnknown(c)
      END
   END
END PushConstType ;


(*
   PushConstructorCastType -
*)

PROCEDURE PushConstructorCastType ;
BEGIN
   IF inDesignator
   THEN
      InitConvert (cast, OperandT (1), NIL, NIL)
   END
END PushConstructorCastType ;


(*
   TypeToMeta -
*)

PROCEDURE TypeToMeta (type: CARDINAL) : constType ;
BEGIN
   IF type=Char
   THEN
      RETURN char
   ELSIF type=Boolean
   THEN
      RETURN boolean
   ELSIF IsRealType (type)
   THEN
      RETURN rtype
   ELSIF IsComplexType (type)
   THEN
      RETURN ctype
   ELSIF IsOrdinalType (type)
   THEN
      RETURN ztype
   ELSE
      RETURN unknown
   END
END TypeToMeta ;


(*
   buildConstFunction - we are only concerned about resolving the return type o
                        a function, so we can ignore all parameters - except
                        the first one in the case of VAL(type, foo).
                        buildConstFunction uses a unary exprNode to represent
                        a function.
*)

PROCEDURE buildConstFunction (func: CARDINAL; n: CARDINAL) ;
VAR
   i     : CARDINAL ;
   first,
   second: exprNode ;
BEGIN
   first := NIL ;
   second := NIL ;
   IF n=1
   THEN
      first := PopAddress (exprStack)
   ELSIF n>=2
   THEN
      i := n ;
      WHILE i>2 DO
         second := PopAddress (exprStack) ;
         DISPOSE (second) ;
         DEC (i)
      END ;
      second := PopAddress (exprStack) ;
      first := PopAddress (exprStack)
   END ;
   IF (func=Val) OR (func=Cast)
   THEN
      InitConvert (cast, NulSym, first, second)
   ELSIF (func=Max) OR (func=Min)
   THEN
      InitFunction (unknown, func, NulSym, first, second, FALSE)
   ELSE
      InitFunction (TypeToMeta(GetSkippedType(func)), func, GetSkippedType(func),
                    first, second, n>2)
   END
END buildConstFunction ;


(*
   ErrorConstFunction - generate an error message at functok using func in the
                        error message providing it is not NulSym.
*)

PROCEDURE ErrorConstFunction (func: CARDINAL; functok: CARDINAL) ;
BEGIN
   IF func = NulSym
   THEN
      IF Iso
      THEN
         ErrorFormat0 (NewError (functok),
                       'the only functions permissible in a constant expression are: CAP, CAST, CHR, CMPLX, FLOAT, HIGH, IM, LENGTH, MAX, MIN, ODD, ORD, RE, SIZE, TSIZE, TRUNC, VAL and gcc builtins')
      ELSE
         ErrorFormat0 (NewError (functok),
                       'the only functions permissible in a constant expression are: CAP, CHR, FLOAT, HIGH, MAX, MIN, ODD, ORD, SIZE, TSIZE, TRUNC, VAL and gcc builtins')
      END
   ELSE
      IF Iso
      THEN
         MetaErrorT1 (functok,
                      'the only functions permissible in a constant expression are: CAP, CAST, CHR, CMPLX, FLOAT, HIGH, IM, LENGTH, MAX, MIN, ODD, ORD, RE, SIZE, TSIZE, TRUNC, VAL and gcc builtins, but not {%1Ead}',
                      func)
      ELSE
         MetaErrorT1 (functok,
                      'the only functions permissible in a constant expression are: CAP, CHR, FLOAT, HIGH, MAX, MIN, ODD, ORD, SIZE, TSIZE, TRUNC, VAL and gcc builtins, but not {%1Ead}',
                      func)
      END
   END
END ErrorConstFunction ;


(*
   PushConstFunctionType -
*)

PROCEDURE PushConstFunctionType ;
VAR
   functok,
   func   : CARDINAL ;
   n      : CARDINAL ;
BEGIN
   PopT (n) ;
   PopTtok (func, functok) ;
   IF inDesignator
   THEN
      IF func = NulSym
      THEN
         ErrorConstFunction (func, functok)
      ELSIF (func#Convert) AND
         (IsPseudoBaseFunction(func) OR
          IsPseudoSystemFunctionConstExpression(func) OR
          (IsProcedure(func) AND IsProcedureBuiltin(func)))
      THEN
         buildConstFunction (func, n)
      ELSIF IsAModula2Type(func)
      THEN
         IF n=1
         THEN
            (* the top element on the expression stack is the first and only parameter to the cast *)
            InitUnary(cast, func, GetSymName(func))
         ELSE
            WriteFormat0('a constant type conversion can only have one argument')
         END
      ELSE
         ErrorConstFunction (func, functok)
      END
   END ;
   PushTtok (func, functok)
END PushConstFunctionType ;


(*
   PushIntegerType -
*)

PROCEDURE PushIntegerType ;
VAR
   sym: CARDINAL ;
   m  : constType ;
BEGIN
   PopT(sym) ;
   IF inDesignator
   THEN
      m := TypeToMeta(GetSkippedType(sym)) ;
      IF m=char
      THEN
         InitCharType(sym)
      ELSE
         InitZType(sym)
      END
   END
END PushIntegerType ;


(*
   PushRType -
*)

PROCEDURE PushRType ;
VAR
   sym: CARDINAL ;
BEGIN
   PopT(sym) ;
   IF inDesignator
   THEN
      InitRType(sym)
   END
END PushRType ;


(*
   PushStringType -
*)

PROCEDURE PushStringType ;
VAR
   sym: CARDINAL ;
BEGIN
   PopT(sym) ;
   IF inDesignator
   THEN
      InitLeaf(str, sym, NulSym)
   END
END PushStringType ;


(*
   InitBinary -
*)

PROCEDURE InitBinary (m: constType; t: CARDINAL; o: Name) ;
VAR
   l, r, b: exprNode ;
BEGIN
   r := PopAddress (exprStack) ;
   l := PopAddress (exprStack) ;
   NEW (b) ;
   WITH b^ DO
      tag := binary ;
      CASE tag OF

      binary:  WITH ebinary DO
                  meta := m ;
                  type := t ;
                  left := l ;
                  right := r ;
                  op := o
               END
      ELSE
         InternalError ('expecting binary')
      END
   END ;
   PushAddress (exprStack, b)
END InitBinary ;


(*
   BuildRelationConst - builds a relationship binary operation.
*)

PROCEDURE BuildRelationConst ;
VAR
   op: Name ;
BEGIN
   PopT (op) ;
   IF inDesignator
   THEN
      InitBinary (boolean, Boolean, op)
   END
END BuildRelationConst ;


(*
   BuildBinaryConst - builds a binary operator node.
*)

PROCEDURE BuildBinaryConst ;
VAR
   op: Name ;
BEGIN
   PopT (op) ;
   IF inDesignator
   THEN
      InitBinary (unknown, NulSym, op)
   END
END BuildBinaryConst ;


(*
   InitUnary -
*)

PROCEDURE InitUnary (m: constType; t: CARDINAL; o: Name) ;
VAR
   l, b: exprNode ;
BEGIN
   l := PopAddress(exprStack) ;
   NEW(b) ;
   WITH b^ DO
      tag := unary ;
      CASE tag OF

      unary:  WITH eunary DO
                 meta := m ;
                 type := t ;
                 left := l ;
                 op := o
              END

      ELSE
         InternalError ('expecting unary')
      END
   END ;
   PushAddress(exprStack, b)
END InitUnary ;


(*
   BuildUnaryConst - builds a unary operator node.
*)

PROCEDURE BuildUnaryConst ;
VAR
   op: Name ;
BEGIN
   PopT(op) ;
   IF inDesignator
   THEN
      InitUnary(unknown, NulSym, op)
   END
END BuildUnaryConst ;


(*
   isTypeResolved -
*)

PROCEDURE isTypeResolved (e: exprNode) : BOOLEAN ;
BEGIN
   WITH e^ DO
      CASE tag OF

      leaf      :  RETURN( (eleaf.type#NulSym) OR (eleaf.meta=str) ) |
      unary     :  RETURN( (eunary.type#NulSym) OR (eunary.meta=str) ) |
      binary    :  RETURN( (ebinary.type#NulSym) OR (ebinary.meta=str) ) |
      designator:  RETURN( (edes.type#NulSym) OR (edes.meta=str) ) |
      expr      :  RETURN( (eexpr.type#NulSym) OR (eexpr.meta=str) ) |
      convert   :  RETURN( (econvert.type#NulSym) OR (econvert.meta=str) ) |
      function  :  RETURN( (efunction.type#NulSym) OR (efunction.meta=str) )

      END
   END
END isTypeResolved ;


(*
   getEtype -
*)

PROCEDURE getEtype (e: exprNode) : CARDINAL ;
BEGIN
   WITH e^ DO
      CASE tag OF

      leaf      :  RETURN( eleaf.type ) |
      unary     :  RETURN( eunary.type ) |
      binary    :  RETURN( ebinary.type ) |
      designator:  RETURN( edes.type ) |
      expr      :  RETURN( eexpr.type ) |
      convert   :  RETURN( econvert.type ) |
      function  :  RETURN( efunction.type )

      END
   END
END getEtype ;


(*
   getEmeta -
*)

PROCEDURE getEmeta (e: exprNode) : constType ;
BEGIN
   WITH e^ DO
      CASE tag OF

      leaf      :  RETURN( eleaf.meta ) |
      unary     :  RETURN( eunary.meta ) |
      binary    :  RETURN( ebinary.meta ) |
      designator:  RETURN( edes.meta ) |
      expr      :  RETURN( eexpr.meta ) |
      convert   :  RETURN( econvert.meta ) |
      function  :  RETURN( efunction.meta )

      END
   END
END getEmeta ;


(*
   assignTM -
*)

PROCEDURE assignTM (VAR td: CARDINAL; VAR md: constType; te: CARDINAL; me: constType) ;
BEGIN
   md := me ;
   td := te
END assignTM ;


(*
   assignType -
*)

PROCEDURE assignType (d, e: exprNode) ;
VAR
   t: CARDINAL ;
   m: constType ;
BEGIN
   m := getEmeta(e) ;
   t := getEtype(e) ;
   WITH d^ DO
      CASE tag OF

      leaf      :  assignTM(eleaf.type, eleaf.meta, t, m) |
      unary     :  assignTM(eunary.type, eunary.meta, t, m) |
      binary    :  assignTM(ebinary.type, ebinary.meta, t, m) |
      designator:  assignTM(edes.type, edes.meta, t, m) |
      expr      :  assignTM(eexpr.type, eexpr.meta, t, m) |
      convert   :  assignTM(econvert.type, econvert.meta, t, m) |
      function  :  assignTM(efunction.type, efunction.meta, t, m)

      END
   END
END assignType ;


(*
   deduceTypes - works out the type and metatype given, l, and, r.
*)

PROCEDURE deduceTypes (VAR t: CARDINAL;
                       VAR m: constType;
                       l, r: exprNode; op: Name) ;
BEGIN
   IF r=NIL
   THEN
      (* function or cast *)
      t := getEtype(l) ;
      m := getEmeta(l)
   ELSIF (op=EqualTok) OR (op=HashTok) OR (op=LessGreaterTok) OR
      (op=LessTok) OR (op=LessEqualTok) OR (op=GreaterTok) OR
      (op=GreaterEqualTok) OR (op=InTok) OR (op=OrTok) OR
      (op=AndTok) OR (op=NotTok) OR (op=AmbersandTok)
   THEN
      t := Boolean ;
      m := boolean
   ELSIF (op=PlusTok) OR (op=MinusTok) OR (op=TimesTok) OR (op=ModTok) OR
         (op=DivTok) OR (op=RemTok) OR (op=DivideTok)
   THEN
      t := MixTypes(getEtype(l), getEtype(r), constToken) ;
      m := getEmeta(l) ;
      IF m=unknown
      THEN
         m := getEmeta(r)
      ELSIF (getEmeta(r)#unknown) AND (m#getEmeta(r))
      THEN
         ErrorFormat0(NewError(constToken),
                      'the operands to a binary constant expression have different types')
      END
   ELSE
      InternalError ('unexpected operator')
   END
END deduceTypes ;


(*
   WalkConvert -
*)

PROCEDURE WalkConvert (e: exprNode) : BOOLEAN ;
BEGIN
   IF isTypeResolved(e)
   THEN
      RETURN( FALSE )
   ELSE
      WITH e^.econvert DO
         IF isTypeResolved(totype)
         THEN
            assignType(e, totype) ;
            RETURN( TRUE )
         END ;
         RETURN( doWalkNode(totype) )
      END
   END
END WalkConvert ;


(*
   WalkFunctionParam -
*)

PROCEDURE WalkFunctionParam (func: CARDINAL; e: exprNode) : BOOLEAN ;
BEGIN
   IF isTypeResolved(e)
   THEN
      RETURN( FALSE )
   ELSE
      IF e^.tag=leaf
      THEN
         WITH e^.eleaf DO
            IF (sym#NulSym) AND (type=NulSym)
            THEN
               IF (func=Min) OR (func=Max)
               THEN
                  IF IsSet (sym)
                  THEN
                     type := SkipType(GetType(sym))
                  ELSE
                     (* sym is the type required for MAX, MIN and VAL *)
                     type := sym
                  END
               ELSE
                  Assert(func=Val) ;
                  type := sym
               END ;
               meta := TypeToMeta(sym) ;
               RETURN( TRUE )
            END
         END
      END
   END ;
   RETURN( FALSE )
END WalkFunctionParam ;


(*
   WalkFunction -
*)

PROCEDURE WalkFunction (e: exprNode) : BOOLEAN ;
BEGIN
   IF isTypeResolved(e)
   THEN
      RETURN( FALSE )
   ELSE
      WITH e^.efunction DO
         IF (func=Max) OR (func=Min) OR (func=Val)
         THEN
            IF isTypeResolved(first)
            THEN
               IF getEmeta(first)=str
               THEN
                  MetaError1('a string parameter cannot be passed to function {%1Dad}', func) ;
                  RETURN( FALSE )
               END ;
               type := getEtype(first) ;
               RETURN( TRUE )
            END ;
            RETURN WalkFunctionParam (func, first)
         ELSE
            MetaError1('not expecting this function inside a constant expression {%1Dad}', func)
         END
      END ;
      RETURN( TRUE )
   END
END WalkFunction ;


(*
   doWalkNode -
*)

PROCEDURE doWalkNode (e: exprNode) : BOOLEAN ;
BEGIN
   WITH e^ DO
      CASE tag OF

      expr    :  RETURN( WalkExpr(e) ) |
      leaf    :  RETURN( WalkLeaf(e) ) |
      unary   :  RETURN( WalkUnary(e) ) |
      binary  :  RETURN( WalkBinary(e) ) |
      convert :  RETURN( WalkConvert(e) ) |
      function:  RETURN( WalkFunction(e) )

      ELSE
         InternalError ('unexpected tag value')
      END
   END ;
   RETURN( FALSE )
END doWalkNode ;


(*
   WalkLeaf -
*)

PROCEDURE WalkLeaf (e: exprNode) : BOOLEAN ;
VAR
   c: exprNode ;
BEGIN
   IF isTypeResolved(e)
   THEN
      RETURN( FALSE )
   ELSE
      WITH e^.eleaf DO
         IF IsConst(sym) AND (GetType(sym)#NulSym)
         THEN
            type := GetSkippedType(sym) ;
            RETURN( TRUE )
         END ;
         IF IsAModula2Type(sym)
         THEN
            type := sym ;
            RETURN( TRUE )
         END ;
         c := findConstDes(sym) ;
         IF (c#NIL) AND isTypeResolved(c)
         THEN
            assignType(e, c) ;
            RETURN( TRUE )
         END
      END
   END ;
   RETURN( FALSE )
END WalkLeaf ;


(*
   WalkUnary -
*)

PROCEDURE WalkUnary (e: exprNode) : BOOLEAN ;
BEGIN
   IF isTypeResolved(e)
   THEN
      RETURN( FALSE )
   ELSE
      WITH e^.eunary DO
         IF isTypeResolved(left)
         THEN
            deduceTypes(type, meta, left, left, op) ;
            RETURN( TRUE )
         END ;
         RETURN( doWalkNode(left) )
      END
   END
END WalkUnary ;


(*
   WalkBinary -
*)

PROCEDURE WalkBinary (e: exprNode) : BOOLEAN ;
VAR
   changed: BOOLEAN ;
BEGIN
   IF isTypeResolved(e)
   THEN
      RETURN( FALSE )
   ELSE
      WITH e^.ebinary DO
         IF isTypeResolved(left) AND isTypeResolved(right)
         THEN
            deduceTypes(type, meta, left, right, op) ;
            RETURN( TRUE )
         END ;
         changed := doWalkNode(left) ;
         RETURN( doWalkNode(right) OR changed )
      END
   END
END WalkBinary ;


(*
   WalkExpr -
*)

PROCEDURE WalkExpr (e: exprNode) : BOOLEAN ;
BEGIN
   IF isTypeResolved(e)
   THEN
      RETURN( FALSE )
   ELSE
      WITH e^.eexpr DO
         IF isTypeResolved(left)
         THEN
            assignType(e, left) ;
            RETURN( TRUE )
         END ;
         RETURN( doWalkNode(left) )
      END
   END
END WalkExpr ;


(*
   doWalkDesExpr - returns TRUE if the expression trees, d, or, e, are changed.
*)

PROCEDURE doWalkDesExpr (d, e: exprNode) : BOOLEAN ;
BEGIN
   IF isTypeResolved(e)
   THEN
      WITH d^.edes DO
         type := getEtype(e) ;
         IF type=NulSym
         THEN
            meta := getEmeta(e) ;
            IF meta=str
            THEN
               (* PutConstString(sym, getString(e)) *)
            END
         ELSE
            PutConst(sym, type)
         END ;
         RETURN( TRUE )
      END
   END ;
   RETURN( doWalkNode(e) )
END doWalkDesExpr ;


(*
   doWalkDes - return TRUE if expression, e, is changed.
*)

PROCEDURE doWalkDes (d: exprNode) : BOOLEAN ;
BEGIN
   IF isTypeResolved(d)
   THEN
      RETURN( FALSE )
   ELSE
      WITH d^ DO
         CASE tag OF

         designator:  WITH edes DO
                         constToken := GetDeclaredMod(sym) ;
                         RETURN( doWalkDesExpr(d, left) )
                      END

         ELSE
            InternalError ('unexpected tag value')
         END
      END
   END
END doWalkDes ;


(*
   findConstDes -
*)

PROCEDURE findConstDes (sym: CARDINAL) : exprNode ;
VAR
   i: CARDINAL ;
   e: exprNode ;
BEGIN
   i := 1 ;
   WHILE i<=HighIndice(constList) DO
      e := GetIndice(constList, i) ;
      WITH e^ DO
         CASE tag OF

         designator:  IF edes.sym=sym
                      THEN
                         RETURN( e )
                      END

         ELSE
         END
      END ;
      INC(i)
   END ;
   RETURN( NIL )
END findConstDes ;


(*
   WalkDes - return TRUE if expression, e, is changed.
*)

PROCEDURE WalkDes (d: exprNode) : BOOLEAN ;
BEGIN
   IF d=NIL
   THEN
      RETURN FALSE
   ELSE
      IF Debugging
      THEN
         DebugDes (d)
      END ;
      RETURN doWalkDes (d)
   END
END WalkDes ;


(*
   WalkConst - returns TRUE if the constant tree associated with, sym,
               is changed.
*)

(*
PROCEDURE WalkConst (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( WalkDes(findConstDes(sym)) )
END WalkConst ;
*)


(*
   WalkConsts - walk over the constant trees and return TRUE if any tree was changed.
                (As a result of a type resolution).
*)

PROCEDURE WalkConsts () : BOOLEAN ;
VAR
   changed: BOOLEAN ;
   i      : CARDINAL ;
BEGIN
   changed := FALSE ;
   i := 1 ;
   WHILE i<=HighIndice(constList) DO
      IF WalkDes(GetIndice(constList, i))
      THEN
         changed := TRUE
      END ;
      INC(i)
   END ;
   RETURN( changed )
END WalkConsts ;


(*
   DebugNodes -
*)

PROCEDURE DebugNodes ;
VAR
   i: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE i<=HighIndice(constList) DO
      IF isTypeResolved(GetIndice(constList, i))
      THEN
         WriteString('resolved ')
      ELSE
         WriteString('unresolved ')
      END ;
      DebugNode(GetIndice(constList, i)) ; WriteLn ;
      INC(i)
   END
END DebugNodes ;


(*
   findAlias -
*)

PROCEDURE findAlias (sym: CARDINAL; e: exprNode) : CARDINAL ;
BEGIN
   CASE e^.tag OF

   designator:  RETURN( findAlias(sym, e^.edes.left) ) |
   leaf      :  RETURN( e^.eleaf.sym ) |
   expr      :  RETURN( findAlias(sym, e^.eexpr.left) ) |
   unary,
   binary    :  RETURN( sym )

   ELSE
      InternalError ('not expecting this tag value')
   END
END findAlias ;


(*
   SkipConst - returns an alias to constant, sym, if one exists.
               Otherwise sym is returned.
*)

PROCEDURE SkipConst (sym: CARDINAL) : CARDINAL ;
VAR
   i: CARDINAL ;
   e: exprNode ;
BEGIN
   i := 1 ;
   WHILE i<=HighIndice(constList) DO
      e := GetIndice(constList, i) ;
      IF (e^.tag=designator) AND (e^.edes.sym=sym)
      THEN
         RETURN( findAlias(sym, e) )
      END ;
      INC(i)
   END ;
   RETURN( sym )
END SkipConst ;


(*
   PushConstAttributeType -
*)

PROCEDURE PushConstAttributeType ;
VAR
   n: Name ;
BEGIN
   PopT(n) ;
   PushT(n) ;
   InitZType(NulSym) ;
   IF (n=MakeKey('BITS_PER_UNIT')) OR (n=MakeKey('BITS_PER_WORD')) OR
      (n=MakeKey('BITS_PER_CHAR')) OR (n=MakeKey('UNITS_PER_WORD'))
   THEN
      (* all ok *)
   ELSE
      WriteFormat1("unknown constant attribute value '%a'", n)
   END
END PushConstAttributeType ;


(*
   PushConstAttributePairType -
*)

PROCEDURE PushConstAttributePairType ;
VAR
   q, n: Name ;
BEGIN
   PopT(n) ;
   PopT(q) ;
   PushT(q) ;
   PushT(n) ;
   IF (n=MakeKey('IEC559')) OR (n=MakeKey('LIA1')) OR (n=MakeKey('IEEE')) OR
      (n=MakeKey('ISO')) OR (n=MakeKey('rounds')) OR (n=MakeKey('gUnderflow')) OR
      (n=MakeKey('exception')) OR (n=MakeKey('extend'))
   THEN
      InitBooleanType(NulSym)
   ELSIF (n=MakeKey('radix')) OR (n=MakeKey('places')) OR (n=MakeKey('expoMin')) OR
         (n=MakeKey('expoMax')) OR (n=MakeKey('nModes'))
   THEN
      InitZType(NulSym)
   ELSIF (n=MakeKey('large')) OR (n=MakeKey('small'))
   THEN
      InitRType(NulSym)
   ELSE
      WriteFormat1("unknown constant attribute value '%a'", n) ;
      InitUnknown(NulSym)
   END
END PushConstAttributePairType ;


(*
   CheckConsts -
*)

PROCEDURE CheckConsts ;
VAR
   i: CARDINAL ;
   e: exprNode ;
BEGIN
   i := 1 ;
   WHILE i<=HighIndice(constList) DO
      e := GetIndice(constList, i) ;
      IF NOT isTypeResolved(e)
      THEN
         WITH e^ DO
            CASE tag OF

            designator:  MetaError1('the type of the constant declaration {%1Dad} cannot be determined', edes.sym)

            ELSE
            END
         END
      END ;
      INC(i)
   END
END CheckConsts ;


(*
   ResolveConstTypes - resolves the types of all designator declared constants.
*)

PROCEDURE ResolveConstTypes ;
BEGIN
   IF Debugging
   THEN
      WriteString('initially') ; WriteLn ;
      DebugNodes
   END ;
   WHILE WalkConsts() DO
      IF Debugging
      THEN
         WriteString('iteration') ; WriteLn ;
         DebugNodes
      END
   END ;
   IF Debugging
   THEN
      WriteString('finally') ; WriteLn ;
      DebugNodes
   END ;
   CheckConsts
END ResolveConstTypes ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   exprStack := InitStackAddress () ;
   constList := InitIndex (1) ;
   desStack := InitStackWord () ;
   inDesignator := FALSE
END Init ;


BEGIN
   Init
END PCSymBuild.
