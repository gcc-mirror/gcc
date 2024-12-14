(* M2GenGCC.mod convert the quadruples into GCC trees.

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

IMPLEMENTATION MODULE M2GenGCC ;

FROM SYSTEM IMPORT ADDRESS, WORD ;

FROM SymbolTable IMPORT PushSize, PopSize, PushValue, PopValue,
                        PushVarSize,
                        MakeConstLit,
                        RequestSym, FromModuleGetSym,
                        StartScope, EndScope, GetScope,
                        GetMainModule, GetModuleScope,
                        GetSymName, ModeOfAddr, GetMode,
                        GetGnuAsm, IsGnuAsmVolatile, IsGnuAsmSimple,
                        GetGnuAsmInput, GetGnuAsmOutput, GetGnuAsmTrash,
                        GetLowestType,
                        GetLocalSym, GetVarWritten,
                        GetVarient, GetVarBackEndType, GetModuleCtors,
                        NoOfVariables,
                        NoOfParamAny, GetParent, GetDimension, IsAModula2Type,
                        IsModule, IsDefImp, IsType, IsModuleWithinProcedure,
                        IsConstString, GetString, GetStringLength,
                        IsConstStringCnul, IsConstStringM2nul,
                        IsConst, IsConstSet, IsProcedure, IsProcType,
                        IsVar, IsVarParamAny, IsTemporary, IsTuple,
                        IsEnumeration,
                        IsUnbounded, IsArray, IsSet, IsConstructor,
                        IsProcedureVariable,
                        IsUnboundedParamAny,
                        IsRecordField, IsFieldVarient, IsVarient, IsRecord,
                        IsExportQualified,
                        IsExported,
                        IsSubrange, IsPointer,
                        IsProcedureBuiltinAvailable, IsProcedureInline,
                        IsParameter, IsParameterVar,
                        IsValueSolved, IsSizeSolved,
                        IsProcedureNested, IsInnerModule, IsArrayLarge,
                        IsComposite, IsVariableSSA, IsPublic, IsCtor,
                        IsConstStringKnown,
                        ForeachExportedDo,
                        ForeachImportedDo,
                        ForeachProcedureDo,
                        ForeachInnerModuleDo,
                        ForeachLocalSymDo,
			GetLType,
                        GetType, GetNth, GetNthParamAny,
                        SkipType, SkipTypeAndSubrange,
                        GetUnboundedHighOffset,
                        GetUnboundedAddressOffset,
                        GetSubrange, NoOfElements, GetArraySubscript,
                        GetFirstUsed, GetDeclaredMod,
                        GetProcedureBeginEnd,
                        GetRegInterface,
                        GetProcedureQuads,
                        GetProcedureBuiltin,
                        GetPriority, GetNeedSavePriority,
                        PutConstStringKnown,
                        PutConst, PutConstSet, PutConstructor,
			GetSType, GetTypeMode,
                        HasVarParameters, CopyConstString,
                        GetVarDeclFullTok,
                        NulSym ;

FROM M2Batch IMPORT MakeDefinitionSource ;

FROM M2LexBuf IMPORT FindFileNameFromToken, TokenToLineNo, TokenToLocation,
                     MakeVirtualTok, UnknownTokenNo, BuiltinTokenNo ;

FROM M2Code IMPORT CodeBlock ;
FROM M2Debug IMPORT Assert ;
FROM M2Error IMPORT InternalError, WriteFormat0, WriteFormat1, WriteFormat2, WarnStringAt ;

FROM M2MetaError IMPORT MetaErrorT0, MetaErrorT1, MetaErrorT2, MetaErrorT3,
                        MetaError1, MetaError2, MetaErrorStringT1,
                        MetaErrorDecl ;

FROM M2Options IMPORT UnboundedByReference, PedanticCast,
                      VerboseUnbounded, Iso, Pim, DebugBuiltins, WholeProgram,
                      StrictTypeChecking, AutoInit, cflag, ScaffoldMain,
                      ScaffoldDynamic, ScaffoldStatic, GetDebugTraceQuad ;

FROM M2Printf IMPORT printf0, printf1, printf2, printf4 ;
FROM M2Quiet IMPORT qprintf0 ;

FROM M2Base IMPORT MixTypes, MixTypesDecl, NegateType, ActivationPointer, IsMathType,
                   IsRealType, IsComplexType, IsBaseType,
                   IsOrdinalType,
                   Cardinal, Char, Integer, IsTrunc,
                   Boolean, True,
                   Im, Re, Cmplx, GetCmplxReturnType, GetBaseTypeMinMax,
                   CheckAssignmentCompatible,
                   IsAssignmentCompatible, IsExpressionCompatible ;

FROM M2Bitset IMPORT Bitset ;
FROM NameKey IMPORT Name, MakeKey, KeyToCharStar, LengthKey, makekey, NulName ;

FROM DynamicStrings IMPORT string, InitString, KillString, String,
                           InitStringCharStar, Mark, Slice, ConCat, ConCatChar,
                           InitStringChar, Dup ;

FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3, Sprintf4 ;
FROM M2System IMPORT Address, Word, System, TBitSize, MakeAdr, IsSystemType, IsGenericSystemType, IsRealN, IsComplexN, IsSetN, IsWordN, Loc, Byte ;
FROM M2FileName IMPORT CalculateFileName ;
FROM SymbolConversion IMPORT AddModGcc, Mod2Gcc, GccKnowsAbout, RemoveMod2Gcc ;

FROM M2StackWord IMPORT InitStackWord, StackOfWord, PeepWord, ReduceWord,
                        PushWord, PopWord, IsEmptyWord ;

FROM Lists IMPORT List, InitList, KillList,
                  PutItemIntoList,
                  RemoveItemFromList, IncludeItemIntoList,
                  NoOfItemsInList, GetItemFromList ;

FROM M2ALU IMPORT PtrToValue,
                  IsValueTypeReal, IsValueTypeSet,
                  IsValueTypeConstructor, IsValueTypeArray,
                  IsValueTypeRecord, IsValueTypeComplex,
                  PushIntegerTree, PopIntegerTree,
                  PushSetTree, PopSetTree,
                  PopRealTree, PushCard,
                  PushRealTree,
                  PopComplexTree, PopChar,
                  Gre, Sub, Equ, NotEqu, LessEqu,
                  BuildRange, SetOr, SetAnd, SetNegate,
                  SetSymmetricDifference, SetDifference,
                  SetShift, SetRotate,
                  AddBit, SubBit, Less, Addn, GreEqu, SetIn,
                  CheckOrResetOverflow, GetRange, GetValue,
                  ConvertToType ;

FROM M2GCCDeclare IMPORT WalkAction,
                         DeclareConstant, TryDeclareConstant,
                         DeclareConstructor, TryDeclareConstructor,
                         StartDeclareScope, EndDeclareScope,
                         PromoteToString, PromoteToCString, DeclareLocalVariable,
                         CompletelyResolved,
                         PoisonSymbols, GetTypeMin, GetTypeMax,
                         IsProcedureGccNested, DeclareParameters,
                         ConstantKnownAndUsed, PrintSym ;

FROM M2Range IMPORT CodeRangeCheck, FoldRangeCheck, CodeErrorCheck, GetMinMax ;

FROM m2builtins IMPORT BuiltInAlloca,
                       BuiltinMemSet, BuiltinMemCopy,
                       GetBuiltinConst, GetBuiltinTypeInfo,
                       BuiltinExists, BuildBuiltinTree ;

FROM m2expr IMPORT GetIntegerZero, GetIntegerOne,
                   GetCardinalOne,
                   GetPointerZero,
                   GetCardinalZero,
                   GetSizeOfInBits,
                   TreeOverflow,
                   FoldAndStrip,
                   CompareTrees,
                   StringLength,
                   AreConstantsEqual,
                   GetCstInteger,
                   BuildForeachWordInSetDoIfExpr,
                   BuildIfConstInVar,
                   BuildIfVarInVar,
                   BuildIfNotConstInVar,
                   BuildIfNotVarInVar,
                   BuildBinCheckProcedure, BuildUnaryCheckProcedure,
                   BuildBinProcedure, BuildUnaryProcedure,
                   BuildSetProcedure, BuildUnarySetFunction,
		   BuildAddCheck, BuildSubCheck, BuildMultCheck, BuildDivTruncCheck,
                   BuildDivM2Check, BuildModM2Check,
                   BuildAdd, BuildSub, BuildMult, BuildLSL,
		   BuildDivCeil, BuildModCeil,
                   BuildDivTrunc, BuildModTrunc, BuildDivFloor, BuildModFloor,
		   BuildDivM2, BuildModM2,
                   BuildRDiv,
                   BuildLogicalOrAddress,
                   BuildLogicalOr, BuildLogicalAnd, BuildSymmetricDifference,
                   BuildLogicalDifference,
                   BuildLogicalShift, BuildLogicalRotate,
                   BuildNegate, BuildNegateCheck, BuildAddr, BuildSize, BuildTBitSize,
                   BuildOffset, BuildOffset1,
                   BuildLessThan, BuildGreaterThan,
                   BuildLessThanOrEqual, BuildGreaterThanOrEqual,
                   BuildEqualTo, BuildNotEqualTo,
                   BuildIsSuperset, BuildIsNotSuperset,
                   BuildIsSubset, BuildIsNotSubset,
                   BuildIndirect, BuildArray,
                   BuildTrunc, BuildCoerce,
                   BuildBinaryForeachWordDo,
                   BuildBinarySetDo,
                   BuildSetNegate,
                   BuildComponentRef,
                   BuildCap, BuildAbs, BuildIm, BuildRe, BuildCmplx,
                   BuildAddAddress,
                   BuildIfInRangeGoto, BuildIfNotInRangeGoto ;

FROM m2tree IMPORT debug_tree, skip_const_decl ;
FROM gcctypes IMPORT location_t, tree ;

FROM m2decl IMPORT BuildStringConstant, BuildCStringConstant,
                   DeclareKnownConstant, GetBitsPerBitset,
                   BuildIntegerConstant,
                   BuildModuleCtor, DeclareModuleCtor ;

FROM m2statement IMPORT BuildAsm, BuildProcedureCallTree, BuildParam, BuildFunctValue,
                        DoJump, BuildUnaryForeachWordDo, BuildGoto, BuildCall2, BuildCall3,
                        BuildStart, BuildEnd, BuildCallInner, BuildStartFunctionCode,
                        BuildEndFunctionCode,
                        BuildAssignmentTree, DeclareLabel,
                        BuildFunctionCallTree,
                        BuildAssignmentStatement,
                        BuildIndirectProcedureCallTree,
                        BuildPushFunctionContext, BuildPopFunctionContext,
                        BuildReturnValueCode, SetLastFunction,
                        BuildIncludeVarConst, BuildIncludeVarVar,
                        BuildExcludeVarConst, BuildExcludeVarVar,
                        BuildBuiltinCallTree,
			GetParamTree, BuildCleanUp,
			BuildTryFinally,
			GetLastFunction, SetLastFunction,
                        SetBeginLocation, SetEndLocation ;

FROM m2type IMPORT ChainOnParamValue, GetPointerType, GetIntegerType, AddStatement,
                   GetCardinalType, GetWordType, GetM2ZType, GetM2RType, GetM2CType,
                   BuildCharConstant, AddStringToTreeList, BuildArrayStringConstructor,
                   GetArrayNoOfElements, GetTreeType ;

FROM m2block IMPORT RememberConstant, pushGlobalScope, popGlobalScope, finishFunctionDecl,
                    pushFunctionScope, popFunctionScope,
		    push_statement_list, pop_statement_list, begin_statement_list,
		    addStmtNote, removeStmtNote ;

FROM m2misc IMPORT DebugTree ;

FROM m2convert IMPORT BuildConvert, ConvertConstantAndCheck, ToCardinal, ConvertString ;

FROM m2except IMPORT BuildThrow, BuildTryBegin, BuildTryEnd,
                     BuildCatchBegin, BuildCatchEnd ;

FROM M2Quads IMPORT QuadOperator, GetQuad, IsReferenced, GetNextQuad,
                    SubQuad, PutQuad, MustCheckOverflow, GetQuadOtok,
                    GetQuadOTypetok,
                    QuadToTokenNo, DisplayQuad, GetQuadtok,
                    GetM2OperatorDesc, GetQuadOp,
                    IsQuadConstExpr, IsBecomes, IsGoto, IsConditional,
                    IsDummy, IsConditionalBooleanQuad,
                    GetQuadOp1, GetQuadOp3, GetQuadDest, SetQuadConstExpr ;

FROM M2Check IMPORT ParameterTypeCompatible, AssignmentTypeCompatible,  ExpressionTypeCompatible ;
FROM M2SSA IMPORT EnableSSA ;
FROM M2Optimize IMPORT FoldBranches ;

FROM M2BasicBlock IMPORT BasicBlock, IsBasicBlockFirst,
                         GetBasicBlockStart, GetBasicBlockEnd ;


CONST
   Debugging         = FALSE ;
   PriorityDebugging = FALSE ;
   CascadedDebugging = FALSE ;

TYPE
   DoProcedure      = PROCEDURE (CARDINAL) ;
   DoUnaryProcedure = PROCEDURE (CARDINAL) ;

VAR
   Memset, Memcpy           : CARDINAL ;
   CurrentQuadToken         : CARDINAL ;
   UnboundedLabelNo         : CARDINAL ;
   LastLine                 : CARDINAL ;(* The Last Line number emitted with the  *)
                                        (* generated code.                        *)
   LastOperator             : QuadOperator ; (* The last operator processed.      *)
   ScopeStack               : StackOfWord ; (* keeps track of the current scope       *)
                                            (* under translation.                     *)
   NoChange                 : BOOLEAN ;     (* has any constant been resolved?        *)


(*
   Rules for Quadruples
   ====================

   Rules
   =====

   All program declared variables are given the mode, Offset.
   All constants have mode, Immediate.

   Operators
   =========

------------------------------------------------------------------------------
   Array Operators
------------------------------------------------------------------------------
   Sym<I>   Base   a            Delivers a constant result if a is a
                                Global variable. If a is a local variable
                                then the Frame pointer needs to be added.
                                Base yields the effective location in memory
                                of, a, array [0,0, .. ,0] address.
   Sym<I>   ElementSize 1       Always delivers a constant. The number
                                indicates which specified element is chosen.
                                ElementSize is the TypeSize for that element.
   Unbounded  Op1 Op3           Initializes the op1 StartAddress of the array
                                op3. Op3 can be a normal array or unbounded array.
                                op1 (is the Unbounded.ArrayAddress) := ADR(op3).
                                In GNU Modula-2 the callee saves non var unbounded
                                arrays. This is direct contrast to the M2F native
                                code generators.
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
   Sym1<O> := Sym3<O>           := has the effect Mem[Sym1<I>] := Mem[Sym3<I>]
------------------------------------------------------------------------------
   Addr Operator  - contains the address of a variable - may need to add
------------------------------------------------------------------------------
   Yields the address of a variable - need to add the frame pointer if
   a variable is local to a procedure.

   Sym1<O>   Addr   Sym2<O>     meaning     Mem[Sym1<I>] := Sym2<I>
   Sym1<V>   Addr   Sym2<O>     meaning     Mem[Sym1<I>] := Sym2<I>
   Sym1<O>   Addr   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<V>   Addr   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
------------------------------------------------------------------------------
   Xindr Operator  ( *a = b)
------------------------------------------------------------------------------
   Sym1<O>   Copy   Sym2<I>     Meaning     Mem[Sym1<I>] := constant
   Sym1<V>   Copy   Sym2<I>     Meaning     Mem[Sym1<I>] := constant
   Sym1<O>   Copy   Sym2<O>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<V>   Copy   Sym2<O>     meaning     Mem[Sym1<I>] := Mem[Sym2<I>]
   Sym1<O>   Copy   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
   Sym1<V>   Copy   Sym2<V>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
------------------------------------------------------------------------------
   IndrX Operator  (a = *b)   where <X> means any value
------------------------------------------------------------------------------
   Sym1<X>   IndrX  Sym2<I>     meaning     Mem[Sym1<I>] := Mem[constant]
   Sym1<X>   IndrX  Sym2<I>     meaning     Mem[Sym1<I>] := Mem[constant]

   Sym1<X>   IndrX  Sym2<X>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
   Sym1<X>   IndrX  Sym2<X>     meaning     Mem[Sym1<I>] := Mem[Mem[Sym2<I>]]
------------------------------------------------------------------------------
   + - / * Operators
------------------------------------------------------------------------------
   Sym1<I>   +      Sym2<I>  Sym3<I>  meaning Sym1<I> := Sym2<I> + Sym3<I>
   Sym1<O>   +      Sym2<O>  Sym3<I>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Sym3<I>
   Sym1<O>   +      Sym2<O>  Sym3<O>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<O>   +      Sym2<O>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<V>   +      Sym2<O>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
   Sym1<V>   +      Sym2<V>  Sym3<V>  meaning Mem[Sym1<I>] :=
                                                    Mem[Sym2<I>] + Mem[Sym3<I>]
------------------------------------------------------------------------------
   Base Operator
------------------------------------------------------------------------------
   Sym1<O>   Base  Sym2   Sym3<O>     meaning     Mem[Sym1<I>] := Sym3<I>
   Sym1<V>   Base  Sym2   Sym3<O>     meaning     Should Never Occur But If it did..
                                                  Mem[Mem[Sym1<I>]] := Sym3<I>
   Sym1<O>   Base  Sym2   Sym3<V>     meaning     Mem[Sym1<I>] := Mem[Sym3<I>]
   Sym1<V>   Base  Sym2   Sym3<V>     meaning     Should Never Occur But If it did..
                                                  Mem[Mem[Sym1<I>]] := Mem[Sym3<I>]
                   Sym2 is the array type
------------------------------------------------------------------------------
*)


(*
   ErrorMessageDecl - emit an error message together with declaration fragments of left
                      and right if they are parameters or variables.
*)

PROCEDURE ErrorMessageDecl (tok: CARDINAL; message: ARRAY OF CHAR;
                            left, right: CARDINAL; iserror: BOOLEAN) ;
BEGIN
   MetaErrorT2 (tok, message, left, right) ;
   MetaErrorDecl (left, iserror) ;
   MetaErrorDecl (right, iserror)
END ErrorMessageDecl ;


(*
   IsExportedGcc - returns TRUE if this symbol should be (as far as the middle/backend of GCC)
                   is concerned, exported.
*)

PROCEDURE IsExportedGcc (sym: CARDINAL) : BOOLEAN ;
VAR
   scope: CARDINAL ;
BEGIN
   (* Has a procedure been overridden as public?  *)
   IF IsProcedure (sym) AND IsPublic (sym)
   THEN
      RETURN TRUE
   END ;
   (* Check for whole program.  *)
   IF WholeProgram
   THEN
      scope := GetScope (sym) ;
      WHILE scope # NulSym DO
         IF IsDefImp (scope)
         THEN
            RETURN IsExported (scope, sym)
         ELSIF IsModule (scope)
         THEN
            RETURN FALSE
         END ;
         scope := GetScope (scope)
      END ;
      InternalError ('expecting scope to eventually reach a module or defimp symbol')
   ELSE
      (* Otherwise it is public if it were exported.  *)
      RETURN IsExported (GetMainModule (), sym)
   END
END IsExportedGcc ;


(*
   ConvertQuadsToTree - runs through the quadruple list and converts it into
                        the GCC tree structure.
*)

PROCEDURE ConvertQuadsToTree (Start, End: CARDINAL) ;
BEGIN
   REPEAT
      CodeStatement (Start) ;
      Start := GetNextQuad (Start)
   UNTIL (Start > End) OR (Start = 0) ;
END ConvertQuadsToTree ;


(*
   IsCompilingMainModule -
*)

PROCEDURE IsCompilingMainModule (sym: CARDINAL) : BOOLEAN ;
BEGIN
   WHILE (sym # NulSym) AND (GetMainModule () # sym) DO
      sym := GetModuleScope (sym)
   END ;
   RETURN sym # NulSym
END IsCompilingMainModule ;


(*
   CodeLastForIterator - call PerformLastForIterator allowing for
                         a non constant last iterator value.
*)

PROCEDURE CodeLastForIterator (quad: CARDINAL) ;
BEGIN
   PerformLastForIterator (quad, NoWalkProcedure, FALSE)
END CodeLastForIterator ;


(*
   FoldLastForIterator - call PerformLastForIterator providing
                         all operands are constant and are known by GCC.
*)

PROCEDURE FoldLastForIterator (quad: CARDINAL; p: WalkAction) ;
VAR
   op              : QuadOperator ;
   e1, e2,
   op1, tuple, incr: CARDINAL ;
BEGIN
   GetQuad (quad, op, op1, tuple, incr) ;
   Assert (IsTuple (tuple)) ;
   e1 := GetNth (tuple, 1) ;
   e2 := GetNth (tuple, 2) ;
   IF IsConst (op1) AND IsConst (e1) AND IsConst (e2) AND IsConst (incr) AND
      GccKnowsAbout (e1) AND GccKnowsAbout (e2) AND GccKnowsAbout (incr)
   THEN
      PerformLastForIterator (quad, p, TRUE)
   END
END FoldLastForIterator ;


(*
   FoldLastForIterator - generates code to calculate the last iterator value
                         in a for loop.  It examines the increment constant
                         and generates different code depending whether it is
                         negative or positive.
*)

PROCEDURE PerformLastForIterator (quad: CARDINAL; p: WalkAction; constant: BOOLEAN) ;
VAR
   success,
   constExpr,
   overflowChecking : BOOLEAN ;
   op               : QuadOperator ;
   lastpos, op1pos,
   op2pos, incrpos,
   last, tuple, incr: CARDINAL ;
   e1, e2           : CARDINAL ;
   lasttree,
   e1tree, e2tree,
   expr, incrtree   : tree ;
   location         : location_t ;
BEGIN
   GetQuadOtok (quad, lastpos, op, last, tuple, incr,
                overflowChecking, constExpr,
                op1pos, op2pos, incrpos) ;
   DeclareConstant (incrpos, incr) ;
   lasttree := Mod2Gcc (last) ;
   success := TRUE ;
   IF IsConst (incr)
   THEN
      incrtree := Mod2Gcc (incr) ;
      location := TokenToLocation (lastpos) ;
      e1 := GetNth (tuple, 1) ;
      e2 := GetNth (tuple, 2) ;
      e1tree := Mod2Gcc (e1) ;
      e2tree := Mod2Gcc (e2) ;
      IF CompareTrees (incrtree, GetIntegerZero (location)) = 0
      THEN
         MetaErrorT0 (lastpos,
                      'the {%kFOR} loop step value must not be zero') ;
         MetaErrorDecl (incr, TRUE) ;
         NoChange := FALSE ;
         SubQuad (quad) ;
         success := FALSE
      ELSIF CompareTrees (incrtree, GetIntegerZero (location)) > 0
      THEN
         (* If incr > 0 then LastIterator := ((e2-e1) DIV incr) * incr + e1.  *)
         expr := BuildSub (location, e2tree, e1tree, FALSE) ;
         incrtree := BuildConvert (location, GetTreeType (expr), incrtree, FALSE) ;
         IF TreeOverflow (incrtree)
         THEN
            MetaErrorT0 (lastpos,
                         'the intemediate calculation for the last iterator value in the {%kFOR} loop has caused an overflow') ;
            NoChange := FALSE ;
            SubQuad (quad) ;
            success := FALSE
         ELSE
            expr := BuildDivFloor (location, expr, incrtree, FALSE) ;
            expr := BuildMult (location, expr, incrtree, FALSE) ;
            expr := BuildAdd (location, expr, e1tree, FALSE)
         END
      ELSE
         (* Else use LastIterator := e1 - ((e1-e2) DIV PositiveBy) * PositiveBy
            to avoid unsigned div signed arithmetic.  *)
         expr := BuildSub (location, e1tree, e2tree, FALSE) ;
         incrtree := BuildConvert (location, GetM2ZType (), incrtree, FALSE) ;
         incrtree := BuildNegate (location, incrtree, FALSE) ;
         incrtree := BuildConvert (location, GetTreeType (expr), incrtree, FALSE) ;
         IF TreeOverflow (incrtree)
         THEN
            MetaErrorT0 (lastpos,
                         'the intemediate calculation for the last iterator value in the {%kFOR} loop has caused an overflow') ;
            NoChange := FALSE ;
            SubQuad (quad) ;
            success := FALSE
         ELSE
            expr := BuildSub (location, e1tree, e2tree, FALSE) ;
            expr := BuildDivFloor (location, expr, incrtree, FALSE) ;
            expr := BuildMult (location, expr, incrtree, FALSE) ;
            expr := BuildSub (location, e1tree, expr, FALSE)
         END
      END ;
      IF success
      THEN
         IF IsConst (last)
         THEN
            AddModGcc (last, expr) ;
            p (last) ;
            NoChange := FALSE ;
            SubQuad (quad)
         ELSE
            Assert (NOT constant) ;
            BuildAssignmentStatement (location, lasttree, expr)
         END
      END
   ELSE
      MetaErrorT1 (lastpos,
                   'the value {%1Ead} in the {%kBY} clause of the {%kFOR} loop must be constant',
                   incr) ;
      MetaErrorDecl (incr, TRUE) ;
      NoChange := FALSE ;
      SubQuad (quad)
   END
END PerformLastForIterator ;


(*
   CodeStatement - A multi-way decision call depending on the current
                   quadruple.
*)

PROCEDURE CodeStatement (q: CARDINAL) ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   location     : location_t ;
BEGIN
   InitBuiltinSyms (BuiltinTokenNo) ;
   GetQuad(q, op, op1, op2, op3) ;
   IF op=StatementNoteOp
   THEN
      FoldStatementNote (op3)  (* will change CurrentQuadToken using op3  *)
   ELSE
      CurrentQuadToken := QuadToTokenNo (q)
   END ;
   location := TokenToLocation (CurrentQuadToken) ;
   CheckReferenced(q, op) ;
   IF GetDebugTraceQuad ()
   THEN
      printf0 ('building: ') ;
      DisplayQuad (q)
   END ;

   CASE op OF

   StartDefFileOp     : CodeStartDefFile (op3) |
   StartModFileOp     : CodeStartModFile (op3) |
   ModuleScopeOp      : CodeModuleScope (op3) |
   EndFileOp          : CodeEndFile |
   InitStartOp        : CodeInitStart (op3, IsCompilingMainModule (op3)) |
   InitEndOp          : CodeInitEnd (op3, IsCompilingMainModule (op3)) |
   FinallyStartOp     : CodeFinallyStart (op3, IsCompilingMainModule (op3)) |
   FinallyEndOp       : CodeFinallyEnd (op3, IsCompilingMainModule (op3)) |
   NewLocalVarOp      : CodeNewLocalVar (op1, op3) |
   KillLocalVarOp     : CodeKillLocalVar (op3) |
   ProcedureScopeOp   : CodeProcedureScope (op3) |
   ReturnOp           : (* Not used as return is achieved by KillLocalVar.  *)  |
   ReturnValueOp      : CodeReturnValue (q) |
   TryOp              : CodeTry |
   ThrowOp            : CodeThrow (op3) |
   CatchBeginOp       : CodeCatchBegin |
   CatchEndOp         : CodeCatchEnd |
   RetryOp            : CodeRetry (op3) |
   DummyOp            : |
   InitAddressOp      : CodeInitAddress(q, op1, op2, op3) |
   BecomesOp          : CodeBecomes(q) |
   ArithAddOp,
   AddOp              : CodeAddChecked (q, op2, op3) |
   SubOp              : CodeSubChecked (q, op2, op3) |
   MultOp             : CodeMultChecked (q, op2, op3) |
   DivM2Op            : CodeDivM2Checked (q, op2, op3) |
   ModM2Op            : CodeModM2Checked (q, op2, op3) |
   DivTruncOp         : CodeDivTrunc (q, op2, op3) |
   ModTruncOp         : CodeModTrunc (q, op2, op3) |
   DivCeilOp          : CodeDivCeil (q, op2, op3) |
   ModCeilOp          : CodeModCeil (q, op2, op3) |
   DivFloorOp         : CodeDivFloor (q, op2, op3) |
   ModFloorOp         : CodeModFloor (q, op2, op3) |
   GotoOp             : CodeGoto (op3) |
   InclOp             : CodeIncl (op1, op3) |
   ExclOp             : CodeExcl (op1, op3) |
   NegateOp           : CodeNegateChecked (q, op1, op3) |
   LastForIteratorOp  : CodeLastForIterator (q) |
   LogicalShiftOp     : CodeSetShift (q, op1, op2, op3) |
   LogicalRotateOp    : CodeSetRotate (q, op1, op2, op3) |
   LogicalOrOp        : CodeSetOr (q) |
   LogicalAndOp       : CodeSetAnd (q) |
   LogicalXorOp       : CodeSetSymmetricDifference (q) |
   LogicalDiffOp      : CodeSetLogicalDifference (q) |
   IfLessOp           : CodeIfLess (q) |
   IfEquOp            : CodeIfEqu (q) |
   IfNotEquOp         : CodeIfNotEqu (q) |
   IfGreEquOp         : CodeIfGreEqu (q) |
   IfLessEquOp        : CodeIfLessEqu (q) |
   IfGreOp            : CodeIfGre (q) |
   IfInOp             : CodeIfIn (q) |
   IfNotInOp          : CodeIfNotIn (q) |
   IndrXOp            : CodeIndrX (q, op1, op2, op3) |
   XIndrOp            : CodeXIndr (q) |
   CallOp             : CodeCall (CurrentQuadToken, op3) |
   ParamOp            : CodeParam (q) |
   FunctValueOp       : CodeFunctValue (location, op1) |
   AddrOp             : CodeAddr (CurrentQuadToken, q, op1, op3) |
   SizeOp             : CodeSize (op1, op3) |
   UnboundedOp        : CodeUnbounded (op1, op3) |
   RecordFieldOp      : CodeRecordField (op1, op2, op3) |
   HighOp             : CodeHigh (op1, op2, op3) |
   ArrayOp            : CodeArray (op1, op2, op3) |
   ElementSizeOp      : InternalError ('ElementSizeOp is expected to have been folded via constant evaluation') |
   ConvertOp          : CodeConvert (q, op1, op2, op3) |
   CoerceOp           : CodeCoerce (q, op1, op2, op3) |
   CastOp             : CodeCast (q, op1, op2, op3) |
   StandardFunctionOp : CodeStandardFunction (q, op1, op2, op3) |
   SavePriorityOp     : CodeSavePriority (op1, op2, op3) |
   RestorePriorityOp  : CodeRestorePriority (op1, op2, op3) |

   InlineOp           : CodeInline (q) |
   StatementNoteOp    : CodeStatementNote (op3) |
   CodeOnOp           : |           (* the following make no sense with gcc *)
   CodeOffOp          : |
   ProfileOnOp        : |
   ProfileOffOp       : |
   OptimizeOnOp       : |
   OptimizeOffOp      : |
   RangeCheckOp       : CodeRange (op3) |
   ErrorOp            : CodeError (op3) |
   SaveExceptionOp    : CodeSaveException (op1, op3) |
   RestoreExceptionOp : CodeRestoreException (op1, op3)

   ELSE
      WriteFormat1 ('quadruple %d not yet implemented', q) ;
      InternalError ('quadruple not implemented yet')
   END ;
   LastOperator := op
END CodeStatement ;


(*
   ResolveConstantExpressions - resolves constant expressions from the quadruple list.
                                It returns TRUE if one or more constants were folded.
                                When a constant symbol value is solved, the call back
                                p(sym) is invoked.
*)

PROCEDURE ResolveConstantExpressions (p: WalkAction; bb: BasicBlock) : BOOLEAN ;
VAR
   tokenno: CARDINAL ;
   quad   : CARDINAL ;
   op     : QuadOperator ;
   op1,
   op2,
   op3,
   op1pos,
   op2pos,
   op3pos : CARDINAL ;
   Changed: BOOLEAN ;
   start,
   end    : CARDINAL ;
BEGIN
   InitBuiltinSyms (BuiltinTokenNo) ;
   start := GetBasicBlockStart (bb) ;
   end := GetBasicBlockEnd (bb) ;
   Changed  := FALSE ;
   REPEAT
      NoChange := TRUE ;
      quad := start ;
      WHILE (quad<=end) AND (quad#0) DO
         tokenno := CurrentQuadToken ;
         IF tokenno=0
         THEN
            tokenno := QuadToTokenNo (quad)
         END ;
         IF GetDebugTraceQuad ()
         THEN
            printf0('examining fold: ') ;
            DisplayQuad (quad)
         END ;
         GetQuadtok (quad, op, op1, op2, op3,
                     op1pos, op2pos, op3pos) ;
         CASE op OF

         StandardFunctionOp : FoldStandardFunction (tokenno, p, quad, op1, op2, op3) |
         BuiltinConstOp     : FoldBuiltinConst (tokenno, p, quad, op1, op3) |
         BuiltinTypeInfoOp  : FoldBuiltinTypeInfo (tokenno, p, quad, op1, op2, op3) |
         LogicalOrOp        : FoldSetOr (tokenno, p, quad, op1, op2, op3) |
         LogicalAndOp       : FoldSetAnd (tokenno, p, quad, op1, op2, op3) |
         LogicalXorOp       : FoldSymmetricDifference (tokenno, p, quad, op1, op2, op3) |
         BecomesOp          : FoldBecomes (p, bb, quad) |
         ArithAddOp         : FoldArithAdd (op1pos, p, quad, op1, op2, op3) |
         AddOp              : FoldAdd (op1pos, p, quad, op1, op2, op3) |
         SubOp              : FoldSub (op1pos, p, quad, op1, op2, op3) |
         MultOp             : FoldMult (op1pos, p, quad, op1, op2, op3) |
         DivM2Op            : FoldDivM2 (op1pos, p, quad, op1, op2, op3) |
         ModM2Op            : FoldModM2 (op1pos, p, quad, op1, op2, op3) |
         DivTruncOp         : FoldDivTrunc (op1pos, p, quad, op1, op2, op3) |
         ModTruncOp         : FoldModTrunc (op1pos, p, quad, op1, op2, op3) |
         DivCeilOp          : FoldDivCeil (op1pos, p, quad, op1, op2, op3) |
         ModCeilOp          : FoldModCeil (op1pos, p, quad, op1, op2, op3) |
         DivFloorOp         : FoldDivFloor (op1pos, p, quad, op1, op2, op3) |
         ModFloorOp         : FoldModFloor (op1pos, p, quad, op1, op2, op3) |
         NegateOp           : FoldNegate (op1pos, p, quad, op1, op3) |
         SizeOp             : FoldSize (tokenno, p, quad, op1, op2, op3) |
         RecordFieldOp      : FoldRecordField (tokenno, p, quad, op1, op2, op3) |
         HighOp             : FoldHigh (tokenno, p, quad, op1, op2, op3) |
         ElementSizeOp      : FoldElementSize (tokenno, p, quad, op1, op2) |
         ConvertOp          : FoldConvert (tokenno, p, quad, op1, op2, op3) |
         CoerceOp           : FoldCoerce (tokenno, p, quad, op1, op2, op3) |
         CastOp             : FoldCast (tokenno, p, quad, op1, op2, op3) |
         InclOp             : FoldIncl (tokenno, p, quad, op1, op3) |
         ExclOp             : FoldExcl (tokenno, p, quad, op1, op3) |
         IfEquOp            : FoldIfEqu (tokenno, quad, op1, op2, op3) |
         IfNotEquOp         : FoldIfNotEqu (tokenno, quad, op1, op2, op3) |
         IfLessOp           : FoldIfLess (tokenno, quad, op1, op2, op3) |
         IfLessEquOp        : FoldIfLessEqu (tokenno, quad, op1, op2, op3) |
         IfGreOp            : FoldIfGre (tokenno, quad, op1, op2, op3) |
         IfGreEquOp         : FoldIfGreEqu (tokenno, quad, op1, op2, op3) |
         IfInOp             : FoldIfIn (tokenno, quad, op1, op2, op3) |
         IfNotInOp          : FoldIfNotIn (tokenno, quad, op1, op2, op3) |
         LogicalShiftOp     : FoldSetShift(tokenno, p, quad, op1, op2, op3) |
         LogicalRotateOp    : FoldSetRotate (tokenno, p, quad, op1, op2, op3) |
         ParamOp            : FoldBuiltinFunction (tokenno, p, quad, op1, op2, op3) |
         RangeCheckOp       : FoldRange (tokenno, quad, op3) |
         StatementNoteOp    : FoldStatementNote (op3) |
         StringLengthOp      : FoldStringLength (quad, p) |
         StringConvertM2nulOp: FoldStringConvertM2nul (quad, p) |
         StringConvertCnulOp : FoldStringConvertCnul (quad, p) |
         LastForIteratorOp  : FoldLastForIterator (quad, p)

         ELSE
            (* ignore quadruple as it is not associated with a constant expression *)
         END ;
         quad := GetNextQuad (quad)
      END ;
      IF NOT NoChange
      THEN
         Changed := TRUE
      END
   UNTIL NoChange ;
   RETURN Changed
END ResolveConstantExpressions ;


(*
   FindSize - given a Modula-2 symbol sym return a gcc tree
              constant representing the storage size in bytes.
*)

PROCEDURE FindSize (tokenno: CARDINAL; sym: CARDINAL) : tree ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (tokenno) ;
   IF IsConstString (sym)
   THEN
      Assert (IsConstStringKnown (sym)) ;
      PushCard (GetStringLength (tokenno, sym)) ;
      RETURN PopIntegerTree ()
   ELSIF IsSizeSolved (sym)
   THEN
      PushSize (sym) ;
      RETURN PopIntegerTree ()
   ELSE
      IF GccKnowsAbout (sym)
      THEN
         IF IsVar (sym) AND IsVariableSSA (sym)
         THEN
            sym := GetType (sym)
         END ;
         PushIntegerTree (BuildSize (location, Mod2Gcc (sym), FALSE)) ;
         PopSize (sym) ;
         PushSize (sym) ;
         RETURN PopIntegerTree ()
      ELSIF IsVar (sym) AND GccKnowsAbout (GetType (sym))
      THEN
         PushIntegerTree (BuildSize (location, Mod2Gcc (GetType (sym)), FALSE)) ;
         RETURN PopIntegerTree ()
      ELSE
         InternalError ('expecting gcc to already know about this symbol')
      END
   END
END FindSize ;


(*
   FindType - returns the type of, Sym, if Sym is a TYPE then return Sym otherwise return GetType(Sym)
*)

PROCEDURE FindType (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsType (Sym)
   THEN
      RETURN Sym
   ELSE
      RETURN GetType (Sym)
   END
END FindType ;


(*
   BuildTreeFromInterface - generates a GCC tree from an interface definition.
*)

PROCEDURE BuildTreeFromInterface (sym: CARDINAL) : tree ;
CONST
   DebugTokPos = FALSE ;
VAR
   tok     : CARDINAL ;
   i       : CARDINAL ;
   name    : Name ;
   str,
   obj     : CARDINAL ;
   gccName,
   asmTree : tree ;
BEGIN
   asmTree := tree (NIL) ;
   IF sym#NulSym
   THEN
      i := 1 ;
      REPEAT
         GetRegInterface (sym, i, tok, name, str, obj) ;
         IF str # NulSym
         THEN
            IF IsConstString (str)
            THEN
               DeclareConstant (tok, obj) ;
               IF name = NulName
               THEN
                  gccName := NIL
               ELSE
                  gccName := BuildCStringConstant (KeyToCharStar (name), LengthKey (name))
               END ;
               asmTree := ChainOnParamValue (asmTree, gccName, PromoteToCString (tok, str),
                                             skip_const_decl (Mod2Gcc (obj))) ;
               IF DebugTokPos
               THEN
                  WarnStringAt (InitString ('input expression'), tok)
               END
            ELSE
               MetaErrorT1 (tok,
                            'a constraint to the GNU ASM statement must be a constant string and not a {%1Ed}',
                            str)
            END
         END ;
         INC(i)
      UNTIL (str = NulSym) AND (obj = NulSym) ;
   END ;
   RETURN asmTree
END BuildTreeFromInterface ;


(*
   BuildTrashTreeFromInterface - generates a GCC string tree from an interface definition.
*)

PROCEDURE BuildTrashTreeFromInterface (sym: CARDINAL) : tree ;
CONST
   DebugTokPos = FALSE ;
VAR
   tok    : CARDINAL ;
   i      : CARDINAL ;
   str,
   obj    : CARDINAL ;
   name   : Name ;
   asmTree: tree ;
BEGIN
   asmTree := tree (NIL) ;
   IF sym # NulSym
   THEN
      i := 1 ;
      REPEAT
         GetRegInterface (sym, i, tok, name, str, obj) ;
         IF str # NulSym
         THEN
            IF IsConstString (str)
            THEN
               asmTree := AddStringToTreeList (asmTree, PromoteToCString (tok, str)) ;
               IF DebugTokPos
               THEN
                  WarnStringAt (InitString ('trash expression'), tok)
               END
            ELSE
               MetaErrorT1 (tok,
                            'a constraint to the GNU ASM statement must be a constant string and not a {%1Ed}',
                            str)
            END
         END ;
(*
         IF obj#NulSym
         THEN
            InternalError ('not expecting the object to be non null in the trash list')
         END ;
*)
         INC (i)
      UNTIL (str = NulSym) AND (obj = NulSym)
   END ;
   RETURN asmTree
END BuildTrashTreeFromInterface ;


(*
   CodeInline - InlineOp is a quadruple which has the following format:

                InlineOp   NulSym  NulSym  Sym
*)

PROCEDURE CodeInline (quad: CARDINAL) ;
VAR
   constExpr,
   overflowChecking: BOOLEAN ;
   op              : QuadOperator ;
   op1, op2, GnuAsm: CARDINAL ;
   op1pos, op2pos,
   op3pos, asmpos  : CARDINAL ;
   string          : CARDINAL ;
   inputs,
   outputs,
   trash,
   labels          : tree ;
   location        : location_t ;
BEGIN
   GetQuadOtok (quad, asmpos, op, op1, op2, GnuAsm,
                overflowChecking, constExpr,
                op1pos, op2pos, op3pos) ;
   location := TokenToLocation (asmpos) ;
   inputs  := BuildTreeFromInterface (GetGnuAsmInput (GnuAsm)) ;
   outputs := BuildTreeFromInterface (GetGnuAsmOutput (GnuAsm)) ;
   trash   := BuildTrashTreeFromInterface (GetGnuAsmTrash (GnuAsm)) ;
   labels  := NIL ;  (* At present it makes no sence for Modula-2 to jump to a label,
                        given that labels are not allowed in Modula-2.  *)
   string  := GetGnuAsm (GnuAsm) ;
   BuildAsm (location,
             PromoteToCString (GetDeclaredMod (string), string),
             IsGnuAsmVolatile (GnuAsm), IsGnuAsmSimple (GnuAsm),
             inputs, outputs, trash, labels)
END CodeInline ;


(*
   FoldStatementNote -
*)

PROCEDURE FoldStatementNote (tokenno: CARDINAL) ;
BEGIN
   CurrentQuadToken := tokenno
END FoldStatementNote ;


(*
   CodeStatementNote -
*)

PROCEDURE CodeStatementNote (tokenno: CARDINAL) ;
BEGIN
   CurrentQuadToken := tokenno ;
   addStmtNote (TokenToLocation (tokenno))
END CodeStatementNote ;


(*
   FoldRange - attempts to fold the range test.
               --fixme-- complete this
*)

PROCEDURE FoldRange (tokenno: CARDINAL; (* p: WalkAction; *)
                     quad: CARDINAL; rangeno: CARDINAL) ;
BEGIN
   FoldRangeCheck (tokenno, quad, rangeno)
END FoldRange ;


(*
   CodeSaveException - op1 := op3(TRUE)
*)

PROCEDURE CodeSaveException (des, exceptionProcedure: CARDINAL) ;
VAR
   functValue: tree ;
   location  : location_t;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   BuildParam (location, Mod2Gcc (True)) ;
   BuildFunctionCallTree (location,
                          Mod2Gcc (exceptionProcedure),
                          Mod2Gcc (GetType (exceptionProcedure))) ;
   functValue := BuildFunctValue (location, Mod2Gcc (des)) ;
   AddStatement (location, functValue)
END CodeSaveException ;


(*
   CodeRestoreException - op1 := op3(op1)
*)

PROCEDURE CodeRestoreException (des, exceptionProcedure: CARDINAL) ;
VAR
   functValue: tree ;
   location  : location_t;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   BuildParam (location, Mod2Gcc (des)) ;
   BuildFunctionCallTree (location,
                          Mod2Gcc (exceptionProcedure),
                          Mod2Gcc (GetType (exceptionProcedure))) ;
   functValue := BuildFunctValue (location, Mod2Gcc (des)) ;
   AddStatement (location, functValue)
END CodeRestoreException ;


(*
   PushScope -
*)

PROCEDURE PushScope (sym: CARDINAL) ;
BEGIN
   PushWord (ScopeStack, sym)
END PushScope ;


(*
   PopScope -
*)

PROCEDURE PopScope ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := PopWord (ScopeStack) ;
   Assert (sym # NulSym)
END PopScope ;


(*
   GetCurrentScopeDescription - returns a description of the current scope.
*)

PROCEDURE GetCurrentScopeDescription () : String ;
VAR
   sym : CARDINAL ;
   n   : String ;
BEGIN
   IF IsEmptyWord(ScopeStack)
   THEN
      InternalError ('not expecting scope stack to be empty')
   ELSE
      sym := PeepWord(ScopeStack, 1) ;
      n := Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym)))) ;
      IF IsDefImp(sym)
      THEN
         RETURN( Sprintf1(Mark(InitString('implementation module %s')), n) )
      ELSIF IsModule(sym)
      THEN
         IF IsInnerModule(sym)
         THEN
            RETURN( Sprintf1(Mark(InitString('inner module %s')), n) )
         ELSE
            RETURN( Sprintf1(Mark(InitString('program module %s')), n) )
         END
      ELSIF IsProcedure(sym)
      THEN
         IF IsProcedureNested(sym)
         THEN
            RETURN( Sprintf1(Mark(InitString('nested procedure %s')), n) )
         ELSE
            RETURN( Sprintf1(Mark(InitString('procedure %s')), n) )
         END
      ELSE
         InternalError ('unexpected scope symbol')
      END
   END
END GetCurrentScopeDescription ;


(*
   CodeRange - encode the range test associated with op3.
*)

PROCEDURE CodeRange (rangeId: CARDINAL) ;
BEGIN
   CodeRangeCheck (rangeId, GetCurrentScopeDescription ())
END CodeRange ;


(*
   CodeError - encode the error test associated with op3.
*)

PROCEDURE CodeError (errorId: CARDINAL) ;
BEGIN
   (* would like to test whether this position is in the same basicblock
      as any known entry point.  If so we could emit an error message.
   *)
   AddStatement (TokenToLocation (CurrentQuadToken),
                 CodeErrorCheck (errorId, GetCurrentScopeDescription (), NIL))
END CodeError ;


(*
   CodeModuleScope - ModuleScopeOp is a quadruple which has the following
                     format:

                     ModuleScopeOp  _  _  moduleSym

                     Its purpose is to reset the source file to another
                     file, hence all line numbers emitted with the
                     generated code will be relative to this source file.
*)

PROCEDURE CodeModuleScope (moduleSym: CARDINAL) ;
BEGIN
   PushScope (moduleSym)
END CodeModuleScope ;


(*
   CodeStartModFile - StartModFileOp is a quadruple which has the following
                      format:

                      StartModFileOp  _  _  moduleSym

                      A new source file has been encountered therefore
                      set LastLine to 1.
                      Call pushGlobalScope.
*)

PROCEDURE CodeStartModFile (moduleSym: CARDINAL) ;
BEGIN
   pushGlobalScope ;
   LastLine := 1 ;
   PushScope (moduleSym)
END CodeStartModFile ;


(*
   CodeStartDefFile - StartDefFileOp is a quadruple with the following
                      format:

                      StartDefFileOp  _  _  moduleSym

                      A new source file has been encountered therefore
                      set LastLine to 1.
                      Call pushGlobalScope.
*)

PROCEDURE CodeStartDefFile (moduleSym: CARDINAL) ;
BEGIN
   pushGlobalScope ;
   PushScope (moduleSym) ;
   LastLine := 1
END CodeStartDefFile ;


(*
   CodeEndFile - pops the GlobalScope.
*)

PROCEDURE CodeEndFile ;
BEGIN
   popGlobalScope
END CodeEndFile ;


(*
   CallInnerInit - produce a call to inner module initialization routine.
*)

PROCEDURE CallInnerInit (moduleSym: WORD) ;
VAR
   location             : location_t;
   ctor, init, fini, dep: CARDINAL ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
   BuildCallInner (location, Mod2Gcc (init))
END CallInnerInit ;


(*
   CallInnerFinally - produce a call to inner module finalization routine.
*)

PROCEDURE CallInnerFinally (moduleSym: WORD) ;
VAR
   location             : location_t;
   ctor, init, fini, dep: CARDINAL ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
   BuildCallInner (location, Mod2Gcc (fini))
END CallInnerFinally ;


(*
   CodeInitStart - emits starting code before the main BEGIN END of the
                   current module.
*)

PROCEDURE CodeInitStart (moduleSym: CARDINAL;
                         CompilingMainModule: BOOLEAN) ;
VAR
   location  : location_t;
   ctor, init,
   fini, dep : CARDINAL ;
BEGIN
   IF CompilingMainModule OR WholeProgram
   THEN
      (* SetFileNameAndLineNo (string (FileName), op1) ;  *)
      location := TokenToLocation (CurrentQuadToken) ;
      GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
      BuildStartFunctionCode (location, Mod2Gcc (init),
                              IsExportedGcc (init), FALSE) ;
      ForeachInnerModuleDo (moduleSym, CallInnerInit)
   END
END CodeInitStart ;


(*
   CodeInitEnd - emits terminating code after the main BEGIN END of the
                 current module.
*)

PROCEDURE CodeInitEnd (moduleSym: CARDINAL;
                       CompilingMainModule: BOOLEAN) ;
VAR
   location  : location_t;
   ctor, init,
   fini, dep : CARDINAL ;
BEGIN
   IF CompilingMainModule OR WholeProgram
   THEN
      (*
         SetFileNameAndLineNo(string(FileName), op1) ;
         EmitLineNote(string(FileName), op1) ;
      *)

      location := TokenToLocation (GetDeclaredMod (moduleSym)) ;
      GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
      finishFunctionDecl (location, Mod2Gcc (init)) ;
      BuildEndFunctionCode (location, Mod2Gcc (init),
                            IsModuleWithinProcedure (moduleSym))
   END
END CodeInitEnd ;


(*
   CodeFinallyStart - emits starting code before the main BEGIN END of the
                      current module.
*)

PROCEDURE CodeFinallyStart (moduleSym: CARDINAL;
                            CompilingMainModule: BOOLEAN) ;
VAR
   location  : location_t;
   ctor, init,
   fini, dep : CARDINAL ;
BEGIN
   IF CompilingMainModule OR WholeProgram
   THEN
      (* SetFileNameAndLineNo (string (FileName), op1) ;  *)
      location := TokenToLocation (CurrentQuadToken) ;
      GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
      BuildStartFunctionCode (location, Mod2Gcc (fini),
                              IsExportedGcc (fini), FALSE) ;
      ForeachInnerModuleDo (moduleSym, CallInnerFinally)
   END
END CodeFinallyStart ;


(*
   CodeFinallyEnd - emits terminating code after the main BEGIN END of the
                    current module.  It also creates the scaffold if the
                    cflag was not present.
*)

PROCEDURE CodeFinallyEnd (moduleSym: CARDINAL;
                          CompilingMainModule: BOOLEAN) ;
VAR
   location  : location_t;
   tokenpos  : CARDINAL ;
   ctor, init,
   fini, dep : CARDINAL ;
BEGIN
   IF CompilingMainModule OR WholeProgram
   THEN
      (*
         SetFileNameAndLineNo(string(FileName), op1) ;
         EmitLineNote(string(FileName), op1) ;
      *)

      tokenpos := GetDeclaredMod (moduleSym) ;
      location := TokenToLocation (tokenpos) ;
      GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
      finishFunctionDecl (location, Mod2Gcc (fini)) ;
      BuildEndFunctionCode (location, Mod2Gcc (fini),
                            IsModuleWithinProcedure (moduleSym))
   END
END CodeFinallyEnd ;


(*
   GetAddressOfUnbounded - returns the address of the unbounded array contents.
*)

PROCEDURE GetAddressOfUnbounded (location: location_t; param: CARDINAL) : tree ;
VAR
   UnboundedType: CARDINAL ;
BEGIN
   UnboundedType := GetType (param) ;
   Assert (IsUnbounded (UnboundedType)) ;

   RETURN BuildConvert (TokenToLocation (GetDeclaredMod (param)),
                        GetPointerType (),
                        BuildComponentRef (location, Mod2Gcc (param), Mod2Gcc (GetUnboundedAddressOffset (UnboundedType))),
                        FALSE)
END GetAddressOfUnbounded ;


(*
   GetHighFromUnbounded - returns a Tree containing the value of
                          param.HIGH.
*)

PROCEDURE GetHighFromUnbounded (location: location_t; dim, param: CARDINAL) : tree ;
VAR
   UnboundedType,
   ArrayType,
   HighField    : CARDINAL ;
   HighTree     : tree ;
   accessibleDim: CARDINAL ;
   (* remainingDim : CARDINAL ;  *)
BEGIN
   UnboundedType := GetType (param) ;
   Assert (IsUnbounded (UnboundedType)) ;
   ArrayType := GetType (UnboundedType) ;
   HighField := GetUnboundedHighOffset (UnboundedType, dim) ;
   IF HighField = NulSym
   THEN
      (* it might be a dynamic array of static arrays,
         so lets see if there is an earlier dimension available.  *)
      accessibleDim := dim ;
      WHILE (HighField = NulSym) AND (accessibleDim > 1) DO
         DEC (accessibleDim) ;
         HighField := GetUnboundedHighOffset(UnboundedType, accessibleDim)
      END ;
      IF HighField = NulSym
      THEN
         MetaError1 ('{%EkHIGH} dimension number {%1N} for array does not exist', dim) ;
         RETURN GetCardinalZero (location)
      ELSE
         (* remainingDim := dim - accessibleDim ;  --fixme-- write tests to stress this code.  *)
         HighTree := BuildHighFromStaticArray (location, (* remainingDim, *) ArrayType) ;
         IF HighTree = NIL
         THEN
            MetaError1 ('{%EkHIGH} dimension number {%1N} for array does not exist', dim) ;
            RETURN GetCardinalZero (location)
         END ;
         RETURN HighTree
      END
   ELSE
      RETURN BuildComponentRef (location, Mod2Gcc (param), Mod2Gcc (HighField))
   END
END GetHighFromUnbounded ;


(*
   GetSizeOfHighFromUnbounded - returns a Tree containing the value of
                                param.HIGH * sizeof(unboundedType).
                                The number of legal bytes this array
                                occupies.
*)

PROCEDURE GetSizeOfHighFromUnbounded (tokenno: CARDINAL; param: CARDINAL) : tree ;
VAR
   t            : tree ;
   UnboundedType,
   ArrayType    : CARDINAL ;
   i, n         : CARDINAL ;
   location     : location_t;
BEGIN
   location := TokenToLocation(tokenno) ;
   UnboundedType := GetType(param) ;
   Assert(IsUnbounded(UnboundedType)) ;
   ArrayType := GetType(UnboundedType) ;

   i := 1 ;
   n := GetDimension(UnboundedType) ;
   t := GetCardinalOne(location) ;
   WHILE i<=n DO
      t := BuildMult(location,
                     BuildAdd(location,
                              GetHighFromUnbounded(location, i, param),
                              GetCardinalOne(location),
                              FALSE),
                     t, FALSE) ;
      (* remember we must add one as HIGH(a) means we can legally reference a[HIGH(a)].  *)
      INC(i)
   END ;
   RETURN( BuildConvert(location,
                        GetCardinalType(),
                        BuildMult(location,
                                  t, BuildConvert(location,
                                                  GetCardinalType(),
                                                  FindSize(tokenno, ArrayType), FALSE), FALSE),
                        FALSE) )
END GetSizeOfHighFromUnbounded ;


(*
   MaybeDebugBuiltinAlloca - if DebugBuiltins is set
                             then call Builtins.alloca_trace
                             else call Builtins.alloca.
*)

PROCEDURE MaybeDebugBuiltinAlloca (location: location_t; tok: CARDINAL; high: tree) : tree ;
VAR
   call,
   memptr,
   func  : tree ;
BEGIN
   IF DebugBuiltins
   THEN
      func := Mod2Gcc (FromModuleGetSym (tok,
                                         MakeKey ('alloca_trace'),
                                         MakeDefinitionSource (tok,
                                                               MakeKey ('Builtins')))) ;
      call := BuiltInAlloca (location, high) ;
      SetLastFunction (call) ;
      memptr := BuildFunctValue (location, call) ;
      call := BuildCall2 (location, func, GetPointerType(), memptr, high) ;
   ELSE
      call := BuiltInAlloca (location, high)
   END ;
   SetLastFunction (call) ;
   RETURN BuildFunctValue (location, call)
END MaybeDebugBuiltinAlloca ;


(*
   MaybeDebugBuiltinMemcpy - if DebugBuiltins is set
                             then call memcpy
                             else call Builtins.memcpy.
*)

PROCEDURE MaybeDebugBuiltinMemcpy (location: location_t; src, dest, nbytes: tree) : tree ;
VAR
   call,
   func: tree ;
BEGIN
   IF DebugBuiltins
   THEN
      func := Mod2Gcc (Memcpy) ;
      call := BuildCall3 (location, func, GetPointerType (), src, dest, nbytes) ;
   ELSE
      call := BuiltinMemCopy (location, src, dest, nbytes)
   END ;
   SetLastFunction (call) ;
   RETURN BuildFunctValue (location, call)
END MaybeDebugBuiltinMemcpy ;


(*
   MakeCopyUse - make a copy of the unbounded array and alter all references
                 from the old unbounded array to the new unbounded array.
                 The parameter, param, contains a RECORD
                                                     ArrayAddress: ADDRESS ;
                                                     ArrayHigh   : CARDINAL ;
                                                  END
                 we simply declare a new array of size, ArrayHigh
                 and set ArrayAddress to the address of the copy.

                 Remember ArrayHigh == sizeof(Array)-sizeof(typeof(array))
                          so we add 1 for the size and add 1 for a possible <nul>
*)

PROCEDURE MakeCopyUse (tokenno: CARDINAL; param: CARDINAL) ;
VAR
   location     : location_t;
   UnboundedType: CARDINAL ;
   Addr,
   High,
   NewArray     : tree ;
BEGIN
   location := TokenToLocation (tokenno) ;
   UnboundedType := GetType (param) ;
   Assert (IsUnbounded (UnboundedType)) ;

   High := GetSizeOfHighFromUnbounded (tokenno, param) ;
   Addr := GetAddressOfUnbounded (location, param) ;

   NewArray := MaybeDebugBuiltinAlloca (location, tokenno, High) ;
   NewArray := MaybeDebugBuiltinMemcpy (location, NewArray, Addr, High) ;

   (* now assign  param.Addr := ADR(NewArray) *)

   BuildAssignmentStatement (location,
                             BuildComponentRef (location,
                                                Mod2Gcc (param),
                                                Mod2Gcc (GetUnboundedAddressOffset (UnboundedType))),
                             NewArray)
END MakeCopyUse ;


(*
   GetParamAddress - returns the address of parameter, param.
*)

PROCEDURE GetParamAddress (location: location_t; proc, param: CARDINAL) : tree ;
VAR
   sym,
   type: CARDINAL ;
BEGIN
   IF IsParameter (param)
   THEN
      type := GetType (param) ;
      sym := GetLocalSym (proc, GetSymName (param)) ;
      IF IsUnbounded (type)
      THEN
         RETURN( GetAddressOfUnbounded (location, sym) )
      ELSE
         Assert (GetMode (sym) = LeftValue) ;
         RETURN( Mod2Gcc (sym) )
      END
   ELSE
      Assert (IsVar (param)) ;
      Assert (GetMode (param) = LeftValue) ;
      RETURN( Mod2Gcc(param) )
   END
END GetParamAddress ;


(*
   IsUnboundedWrittenTo - returns TRUE if the unbounded parameter
                          might be written to, or if -funbounded-by-reference
                          was _not_ specified.
*)

PROCEDURE IsUnboundedWrittenTo (proc, param: CARDINAL) : BOOLEAN ;
VAR
   f     : String ;
   l     : CARDINAL ;
   sym   : CARDINAL ;
   n1, n2: Name ;
BEGIN
   sym := GetLocalSym(proc, GetSymName(param)) ;
   IF sym=NulSym
   THEN
      InternalError ('should find symbol in table')
   ELSE
      IF UnboundedByReference
      THEN
         IF (NOT GetVarWritten(sym)) AND VerboseUnbounded
         THEN
            n1 := GetSymName(sym) ;
            n2 := GetSymName(proc) ;
            f := FindFileNameFromToken(GetDeclaredMod(sym), 0) ;
            l := TokenToLineNo(GetDeclaredMod(sym), 0) ;
            printf4('%s:%d:non VAR unbounded parameter %a in procedure %a does not need to be copied\n',
                    f, l, n1, n2)
         END ;
         RETURN( GetVarWritten(sym) )
      ELSE
         RETURN( TRUE )
      END
   END
END IsUnboundedWrittenTo ;


(*
   GetParamSize - returns the size in bytes of, param.
*)

PROCEDURE GetParamSize (tokenno: CARDINAL; param: CARDINAL) : tree ;
BEGIN
   Assert(IsVar(param) OR IsParameter(param)) ;
   IF IsUnbounded(param)
   THEN
      RETURN GetSizeOfHighFromUnbounded(tokenno, param)
   ELSE
      RETURN BuildSize(tokenno, Mod2Gcc(GetType(param)), FALSE)
   END
END GetParamSize ;


(*
   DoIsIntersection - jumps to, tLabel, if the ranges i1..i2  j1..j2 overlap
                      else jump to, fLabel.
*)

PROCEDURE DoIsIntersection (tokenno: CARDINAL; ta, tb, tc, td: tree; tLabel, fLabel: String) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   (*
     if (ta>td) OR (tb<tc)
     then
        goto fLabel
     else
        goto tLabel
     fi
   *)
   DoJump(location, BuildGreaterThan(location, ta, td), NIL, string(fLabel)) ;
   DoJump(location, BuildLessThan(location, tb, tc), NIL, string(fLabel)) ;
   BuildGoto(location, string(tLabel)) ;
   IF CascadedDebugging
   THEN
      printf1('label used %s\n', tLabel) ;
      printf1('label used %s\n', fLabel)
   END
END DoIsIntersection ;


(*
   BuildCascadedIfThenElsif - mustCheck contains a list of variables which
                              must be checked against the address of (proc, param, i).
                              If the address matches we make a copy of the unbounded
                              parameter (proc, param) and quit further checking.
*)

PROCEDURE BuildCascadedIfThenElsif (tokenno: CARDINAL;
                                    mustCheck: List;
                                    proc, param: CARDINAL) ;
VAR
   ta, tb,
   tc, td  : tree ;
   n, j    : CARDINAL ;
   tLabel,
   fLabel,
   nLabel  : String ;
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   n := NoOfItemsInList(mustCheck) ;
   (* want a sequence of if then elsif statements *)
   IF n>0
   THEN
      INC(UnboundedLabelNo) ;
      j := 1 ;
      ta := GetAddressOfUnbounded(location, param) ;
      tb := BuildConvert(TokenToLocation(tokenno),
                         GetPointerType(),
                         BuildAddAddress(location, ta, GetSizeOfHighFromUnbounded(tokenno, param)),
                         FALSE) ;
      WHILE j<=n DO
         IF j>1
         THEN
            nLabel := CreateLabelProcedureN(proc, "n", UnboundedLabelNo, j) ;
            IF CascadedDebugging
            THEN
               printf1('label declared %s\n', nLabel)
            END ;
            DeclareLabel(location, string(nLabel)) ;
         END ;
         tc := GetParamAddress(location, proc, GetItemFromList(mustCheck, j)) ;
         td := BuildConvert(TokenToLocation(tokenno),
                            GetPointerType(),
                            BuildAddAddress(location, tc, GetParamSize(tokenno, param)),
                            FALSE) ;
         tLabel := CreateLabelProcedureN(proc, "t", UnboundedLabelNo, j+1) ;
         fLabel := CreateLabelProcedureN(proc, "f", UnboundedLabelNo, j+1) ;
         DoIsIntersection(tokenno, ta, tb, tc, td, tLabel, fLabel) ;
         IF CascadedDebugging
         THEN
            printf1('label declared %s\n', tLabel)
         END ;
         DeclareLabel (location, string (tLabel)) ;
         MakeCopyUse (tokenno, param) ;
         IF j<n
         THEN
            nLabel := CreateLabelProcedureN(proc, "n", UnboundedLabelNo, n+1) ;
            BuildGoto(location, string(nLabel)) ;
            IF CascadedDebugging
            THEN
               printf1('goto %s\n', nLabel)
            END
         END ;
         IF CascadedDebugging
         THEN
            printf1('label declared %s\n', fLabel)
         END ;
         DeclareLabel(location, string(fLabel)) ;
         INC(j)
      END ;
(*
      nLabel := CreateLabelProcedureN(proc, "fin", UnboundedLabelNo, n+1) ;
      IF CascadedDebugging
      THEN
         printf1('label declared %s\n', nLabel)
      END ;
      DeclareLabel(location, string(nLabel))
*)
   END
END BuildCascadedIfThenElsif ;


(*
   CheckUnboundedNonVarParameter - if non var unbounded parameter is written to
                                   then
                                      make a copy of the contents of this parameter
                                      and use the copy
                                   else if param
                                      is type compatible with any parameter, symv
                                      and at runtime its address matches symv
                                   then
                                      make a copy of the contents of this parameter
                                      and use the copy
                                   fi
*)

PROCEDURE CheckUnboundedNonVarParameter (tokenno: CARDINAL;
                                         trashed: List;
                                         proc, param: CARDINAL) ;
VAR
   mustCheck   : List ;
   paramTrashed,
   n, j        : CARDINAL ;
   f           : String ;
   l           : CARDINAL ;
   n1, n2      : Name ;
BEGIN
   IF IsUnboundedWrittenTo(proc, param)
   THEN
      MakeCopyUse (tokenno, param)
   ELSE
      InitList(mustCheck) ;
      n := NoOfItemsInList(trashed) ;
      j := 1 ;
      WHILE j<=n DO
         paramTrashed := GetItemFromList(trashed, j) ;
         IF IsAssignmentCompatible(GetLowestType(param), GetLowestType(paramTrashed))
         THEN
            (* we must check whether this unbounded parameter has the same
               address as the trashed parameter *)
            IF VerboseUnbounded
            THEN
               n1 := GetSymName(paramTrashed) ;
               n2 := GetSymName(proc) ;
               f := FindFileNameFromToken(GetDeclaredMod(paramTrashed), 0) ;
               l := TokenToLineNo(GetDeclaredMod(paramTrashed), 0) ;
               printf4('%s:%d:must check at runtime the address of parameter, %a, in procedure, %a, whose contents will be trashed\n',
                       f, l, n1, n2) ;
               n1 := GetSymName(param) ;
               n2 := GetSymName(paramTrashed) ;
               printf4('%s:%d:against address of parameter, %a, possibly resulting in a copy of parameter, %a\n',
                       f, l, n1, n2)
            END ;
            PutItemIntoList(mustCheck, paramTrashed)
         END ;
         INC(j)
      END ;
      (* now we build a sequence of if then { elsif then } end to check addresses *)
      BuildCascadedIfThenElsif (tokenno, mustCheck, proc, param) ;
      KillList(mustCheck)
   END
END CheckUnboundedNonVarParameter ;


(*
   IsParameterWritten - returns TRUE if a parameter, sym, is written to.
*)

PROCEDURE IsParameterWritten (proc: CARDINAL; sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsParameter(sym)
   THEN
      sym := GetLocalSym(proc, GetSymName(sym))
   END ;
   IF IsVar(sym)
   THEN
      (* unbounded arrays will appear as vars *)
      RETURN GetVarWritten(sym)
   END ;
   InternalError ('expecting IsVar to return TRUE')
END IsParameterWritten ;


(*
   SaveNonVarUnboundedParameters - for each var parameter, symv, do
                                      (* not just unbounded var parameters, but _all_
                                         parameters *)
                                      if symv is written to
                                      then
                                         add symv to a compile list
                                      fi
                                   done

                                   for each parameter of procedure, symu, do
                                      if non var unbounded parameter is written to
                                      then
                                         make a copy of the contents of this parameter
                                         and use the copy
                                      else if
                                         symu is type compatible with any parameter, symv
                                         and at runtime its address matches symv
                                      then
                                         make a copy of the contents of this parameter
                                         and use the copy
                                      fi
                                   done
*)

PROCEDURE SaveNonVarUnboundedParameters (tokenno: CARDINAL; proc: CARDINAL) ;
VAR
   i, p   : CARDINAL ;
   trashed: List ;
   f      : String ;
   sym    : CARDINAL ;
   l      : CARDINAL ;
   n1, n2 : Name ;
BEGIN
   InitList(trashed) ;
   i := 1 ;
   p := NoOfParamAny (proc) ;
   WHILE i<=p DO
      sym := GetNthParamAny (proc, i) ;
      IF IsParameterWritten(proc, sym)
      THEN
         IF VerboseUnbounded
         THEN
            n1 := GetSymName(sym) ;
            n2 := GetSymName(proc) ;
            f := FindFileNameFromToken(GetDeclaredMod(sym), 0) ;
            l := TokenToLineNo(GetDeclaredMod(sym), 0) ;
            printf4('%s:%d:parameter, %a, in procedure, %a, is trashed\n',
                    f, l, n1, n2)
         END ;
         PutItemIntoList(trashed, sym)
      END ;
      INC(i)
   END ;
   (* now see whether we need to copy any unbounded array parameters *)
   i := 1 ;
   p := NoOfParamAny (proc) ;
   WHILE i<=p DO
      IF IsUnboundedParamAny (proc, i) AND (NOT IsVarParamAny (proc, i))
      THEN
         CheckUnboundedNonVarParameter (tokenno, trashed, proc, GetNth (proc, i))
      END ;
      INC(i)
   END ;
   KillList(trashed)
END SaveNonVarUnboundedParameters ;


(*
   AutoInitVariable -
*)

PROCEDURE AutoInitVariable (location: location_t; sym: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   IF (NOT IsParameter (sym)) AND IsVar (sym) AND
      (NOT IsTemporary (sym))
   THEN
      (* PrintSym (sym) ; *)
      type := SkipType (GetType (sym)) ;
      (* the type SYSTEM.ADDRESS is a pointer type.  *)
      IF IsPointer (type)
      THEN
         BuildAssignmentStatement (location,
                                   Mod2Gcc (sym),
                                   BuildConvert (location,
                                                 Mod2Gcc (GetType (sym)),
                                                 GetPointerZero (location),
                                                 TRUE))
      END
   END
END AutoInitVariable ;


(*
   AutoInitialize - scope will be a procedure, module or defimp.  All pointer
                    variables are assigned to NIL.
*)

PROCEDURE AutoInitialize (location: location_t; scope: CARDINAL) ;
VAR
   i, n: CARDINAL ;
BEGIN
   IF AutoInit
   THEN
      n := NoOfVariables (scope) ;
      i := 1 ;
      IF IsProcedure (scope)
      THEN
         (* the parameters are stored as local variables.  *)
         INC (i, NoOfParamAny (scope))
      END ;
      WHILE i <= n DO
         AutoInitVariable (location, GetNth (scope, i)) ;
         INC (i)
      END
   END
END AutoInitialize ;


(*
   CodeNewLocalVar - Builds a new frame on the stack to contain the procedure
                     local variables.
*)

PROCEDURE CodeNewLocalVar (tokenno, CurrentProcedure: CARDINAL) ;
VAR
   begin, end: CARDINAL ;
BEGIN
   (* callee saves non var unbounded parameter contents *)
   SaveNonVarUnboundedParameters (tokenno, CurrentProcedure) ;
   BuildPushFunctionContext ;
   GetProcedureBeginEnd (CurrentProcedure, begin, end) ;
   CurrentQuadToken := begin ;
   SetBeginLocation (TokenToLocation (begin)) ;
   AutoInitialize (TokenToLocation (begin), CurrentProcedure) ;
   ForeachProcedureDo (CurrentProcedure, CodeBlock) ;
   ForeachInnerModuleDo (CurrentProcedure, CodeBlock) ;
   BuildPopFunctionContext ;
   ForeachInnerModuleDo (CurrentProcedure, CallInnerInit)
END CodeNewLocalVar ;


(*
   CodeKillLocalVar - removes local variables and returns to previous scope.
*)

PROCEDURE CodeKillLocalVar (CurrentProcedure: CARDINAL) ;
VAR
   begin, end: CARDINAL ;
   proc      : tree ;
BEGIN
   GetProcedureBeginEnd (CurrentProcedure, begin, end) ;
   CurrentQuadToken := end ;
   proc := NIL ;
   IF IsCtor (CurrentProcedure)
   THEN
      proc := DeclareModuleCtor (Mod2Gcc (CurrentProcedure))
   END ;
   BuildEndFunctionCode (TokenToLocation (end),
                         Mod2Gcc (CurrentProcedure),
                         IsProcedureGccNested (CurrentProcedure)) ;
   IF IsCtor (CurrentProcedure) AND (proc # NIL)
   THEN
      BuildModuleCtor (proc)
   END ;
   PoisonSymbols (CurrentProcedure) ;
   removeStmtNote () ;
   PopScope
END CodeKillLocalVar ;


(*
   CodeProcedureScope -
*)

PROCEDURE CodeProcedureScope (CurrentProcedure: CARDINAL) ;
VAR
   begin, end: CARDINAL ;
BEGIN
   removeStmtNote () ;
   GetProcedureBeginEnd (CurrentProcedure, begin, end) ;
   BuildStartFunctionCode (TokenToLocation (begin),
                           Mod2Gcc (CurrentProcedure),
                           IsExportedGcc (CurrentProcedure),
                           IsProcedureInline (CurrentProcedure)) ;
   StartDeclareScope (CurrentProcedure) ;
   PushScope (CurrentProcedure) ;
   (* DeclareParameters(CurrentProcedure) *)
END CodeProcedureScope ;


(*
   CodeReturnValue - places the operand into the return value space
                     allocated by the function call.
*)

PROCEDURE CodeReturnValue (quad: CARDINAL) ;
VAR
   op                                  : QuadOperator ;
   constExpr,
   overflowChecking                    : BOOLEAN ;
   expr, none, procedure               : CARDINAL ;
   combinedpos,
   returnpos, exprpos, nonepos, procpos: CARDINAL ;
   value, length                       : tree ;
   location                            : location_t ;
BEGIN
   GetQuadOtok (quad, returnpos, op, expr, none, procedure,
                overflowChecking, constExpr,
                exprpos, nonepos, procpos) ;
   combinedpos := MakeVirtualTok (returnpos, returnpos, exprpos) ;
   location := TokenToLocation (combinedpos) ;
   TryDeclareConstant (exprpos, expr) ;  (* checks to see whether it is a constant and declares it *)
   TryDeclareConstructor (exprpos, expr) ;
   IF IsConstString (expr) AND (SkipTypeAndSubrange (GetType (procedure)) # Char)
   THEN
      IF NOT PrepareCopyString (returnpos, length, value, expr, GetType (procedure))
      THEN
         MetaErrorT3 (MakeVirtualTok (returnpos, returnpos, exprpos),
                      'string constant {%1Ea} is too large to be returned from procedure {%2a} via the {%3d} {%3a}',
                      expr, procedure, GetType (procedure))
      END ;
      value := BuildArrayStringConstructor (location,
                                            Mod2Gcc (GetType (procedure)),
                                            value, length)
   ELSE
      value := Mod2Gcc (expr)
   END ;
   BuildReturnValueCode (location, Mod2Gcc (procedure), value)
END CodeReturnValue ;


(*
   CodeCall - determines whether the procedure call is a direct call
              or an indirect procedure call.
*)

PROCEDURE CodeCall (tokenno: CARDINAL; procedure: CARDINAL) ;
VAR
   callTree: tree ;
   location: location_t ;
BEGIN
   IF IsProcedure (procedure)
   THEN
      DeclareParameters (procedure) ;
      callTree := CodeDirectCall (tokenno, procedure)
   ELSIF IsProcType (SkipType (GetType (procedure)))
   THEN
      DeclareParameters (SkipType (GetType (procedure))) ;
      callTree := CodeIndirectCall (tokenno, procedure) ;
      procedure := SkipType (GetType (procedure))
   ELSE
      InternalError ('expecting Procedure or ProcType')
   END ;
   IF GetType (procedure) = NulSym
   THEN
      location := TokenToLocation (tokenno) ;
      AddStatement (location, callTree)
   ELSE
      (* leave tree alone - as it will be picked up when processing FunctValue *)
   END
END CodeCall ;


(*
   UseBuiltin - returns a Tree containing the builtin function
                and parameters. It should only be called if
                CanUseBuiltin or IsProcedureBuiltinAvailable returns TRUE.
*)

PROCEDURE UseBuiltin (tokenno: CARDINAL; Sym: CARDINAL) : tree ;
BEGIN
   IF BuiltinExists(KeyToCharStar(GetProcedureBuiltin(Sym)))
   THEN
      RETURN( BuildBuiltinTree(TokenToLocation (tokenno), KeyToCharStar (GetProcedureBuiltin (Sym))) )
   ELSE
      RETURN( BuildBuiltinTree(TokenToLocation (tokenno), KeyToCharStar (GetSymName (Sym))) )
   END
END UseBuiltin ;


(*
   CodeDirectCall - calls a function/procedure.
*)

PROCEDURE CodeDirectCall (tokenno: CARDINAL; procedure: CARDINAL) : tree ;
VAR
   location: location_t ;
   call    : tree ;
BEGIN
   location := TokenToLocation (tokenno) ;
   IF IsProcedureBuiltinAvailable (procedure)
   THEN
      call := UseBuiltin (tokenno, procedure) ;
      IF call # NIL
      THEN
         call := BuildBuiltinCallTree (call)
      END
   ELSE
      call := NIL
   END ;
   IF call = NIL
   THEN
      IF GetType (procedure) = NulSym
      THEN
         call := BuildProcedureCallTree (location, Mod2Gcc (procedure), NIL)
      ELSE
         call := BuildProcedureCallTree (location, Mod2Gcc (procedure), Mod2Gcc (GetType (procedure)))
      END
   END ;
   IF GetType (procedure) = NulSym
   THEN
      SetLastFunction (NIL)
   ELSE
      SetLastFunction (call)
   END ;
   RETURN call
END CodeDirectCall ;


(*
   CodeIndirectCall - calls a function/procedure indirectly.
*)

PROCEDURE CodeIndirectCall (tokenno: CARDINAL; ProcVar: CARDINAL) : tree ;
VAR
   ReturnType: tree ;
   proc      : CARDINAL ;
   location  : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   proc := SkipType(GetType(ProcVar)) ;
   IF GetType(proc)=NulSym
   THEN
      ReturnType := tree(NIL)
   ELSE
      ReturnType := tree(Mod2Gcc(GetType(proc)))
   END ;

   (* now we dereference the lvalue if necessary *)

   IF GetMode(ProcVar)=LeftValue
   THEN
      RETURN BuildIndirectProcedureCallTree(location,
                                             BuildIndirect(location, Mod2Gcc(ProcVar), Mod2Gcc(proc)),
                                             ReturnType)
   ELSE
      RETURN BuildIndirectProcedureCallTree(location, Mod2Gcc(ProcVar), ReturnType)
   END
END CodeIndirectCall ;


(*
   StringToChar - if type=Char and str is a string (of size <= 1)
                  then convert the string into a character constant.
*)

PROCEDURE StringToChar (t: tree; type, str: CARDINAL) : tree ;
VAR
   s: String ;
   n: Name ;
   tokenno : CARDINAL ;
   location: location_t ;
BEGIN
   tokenno := GetDeclaredMod(str) ;
   location := TokenToLocation(tokenno) ;
   type := SkipType (type) ;
   IF (type=Char) AND IsConstString(str)
   THEN
      Assert (IsConstStringKnown (str)) ;
      IF GetStringLength (tokenno, str) = 0
      THEN
         s := InitString('') ;
         t := BuildCharConstant(location, s) ;
         s := KillString(s) ;
      ELSIF GetStringLength (tokenno, str)>1
      THEN
         n := GetSymName(str) ;
         WriteFormat1("type incompatibility, attempting to use a string ('%a') when a CHAR is expected", n) ;
         s := InitString('') ;  (* do something safe *)
         t := BuildCharConstant(location, s)
      END ;
      s := InitStringCharStar(KeyToCharStar(GetString(str))) ;
      s := Slice(s, 0, 1) ;
      t := BuildCharConstant(location, string(s)) ;
      s := KillString(s) ;
   END ;
   RETURN( t )
END StringToChar ;


(*
   ConvertTo - convert gcc tree, t, (which currently represents Modula-2 op3) into
               a symbol of, type.
*)

PROCEDURE ConvertTo (t: tree; type, op3: CARDINAL) : tree ;
BEGIN
   IF SkipType(type)#SkipType(GetType(op3))
   THEN
      IF IsConst(op3) AND (NOT IsConstString(op3))
      THEN
         PushValue(op3) ;
         RETURN( BuildConvert(TokenToLocation(GetDeclaredMod(op3)),
                              Mod2Gcc(type), t, FALSE) )
      END
   END ;
   RETURN( t )
END ConvertTo ;


(*
   ConvertRHS - convert (t, rhs) into, type.  (t, rhs) refer to the
                same entity t is a GCC Tree and, rhs, is a Modula-2
                symbol.  It checks for char and strings
                first and then the remaining types.
*)

PROCEDURE ConvertRHS (t: tree; type, rhs: CARDINAL) : tree ;
BEGIN
   t := StringToChar (Mod2Gcc (rhs), type, rhs) ;
   RETURN ConvertTo (t, type, rhs)
END ConvertRHS ;


(*
   IsCoerceableParameter - returns TRUE if symbol, sym, is a
                           coerceable parameter.
*)

PROCEDURE IsCoerceableParameter (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          IsSet(sym) OR
          (IsOrdinalType(sym) AND (sym#Boolean) AND (NOT IsEnumeration(sym))) OR
          IsComplexType(sym) OR IsRealType(sym) OR
          IsComplexN(sym) OR IsRealN(sym) OR IsSetN(sym)
         )
END IsCoerceableParameter ;


(*
   IsConstProcedure - returns TRUE if, p, is a const procedure.
*)

PROCEDURE IsConstProcedure (p: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsConst(p) AND (GetType(p)#NulSym) AND IsProcType(GetType(p)) )
END IsConstProcedure ;


(*
   IsConstant - returns TRUE if symbol, p, is either a const or procedure.
*)

PROCEDURE IsConstant (p: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsConst (p) OR IsProcedure (p)
END IsConstant ;


(*
   CheckConvertCoerceParameter -
*)

PROCEDURE CheckConvertCoerceParameter (tokenno: CARDINAL; op1, op2, op3: CARDINAL) : tree ;
VAR
   OperandType,
   ParamType  : CARDINAL ;
   location   : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   IF GetNthParamAny (op2, op1)=NulSym
   THEN
      (* We reach here if the argument is being passed to a C vararg function.  *)
      RETURN( Mod2Gcc(op3) )
   ELSE
      OperandType := SkipType(GetType(op3)) ;
      ParamType := SkipType(GetType(GetNthParamAny (op2, op1)))
   END ;
   IF IsProcType(ParamType)
   THEN
      IF IsProcedure(op3) OR IsConstProcedure(op3) OR (OperandType = ParamType)
      THEN
         RETURN( Mod2Gcc(op3) )
      ELSE
         RETURN( BuildConvert(location, Mod2Gcc(ParamType), Mod2Gcc(op3), FALSE) )
      END
   ELSIF IsRealType(OperandType) AND IsRealType(ParamType) AND
      (ParamType#OperandType)
   THEN
      (* SHORTREAL, LONGREAL and REAL conversion during parameter passing *)
      RETURN( BuildConvert(location, Mod2Gcc(ParamType),
                           Mod2Gcc(op3), FALSE) )
   ELSIF (OperandType#NulSym) AND IsSet(OperandType) AND IsConst(op3)
   THEN
      RETURN( DeclareKnownConstant(location,
                                   Mod2Gcc(ParamType),
                                   Mod2Gcc(op3)) )
   ELSIF IsConst(op3) AND
         (IsOrdinalType(ParamType) OR IsSystemType(ParamType))
   THEN
      RETURN( BuildConvert(location, Mod2Gcc(ParamType),
                           StringToChar(Mod2Gcc(op3), ParamType, op3),
                           FALSE) )
   ELSIF IsConstString(op3) OR ((OperandType#NulSym) AND IsCoerceableParameter(OperandType) AND (OperandType#ParamType))
   THEN
      RETURN( BuildConvert(location, Mod2Gcc(ParamType), Mod2Gcc(op3), FALSE) )
   ELSE
      RETURN( Mod2Gcc(op3) )
   END
END CheckConvertCoerceParameter ;


(*
   CheckConstant - checks to see whether we should declare the constant.
*)

PROCEDURE CheckConstant (tokenno: CARDINAL; des, expr: CARDINAL) : tree ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   IF IsProcedure(expr)
   THEN
      RETURN( Mod2Gcc(expr) )
   ELSE
      RETURN( DeclareKnownConstant(location, Mod2Gcc(GetType(des)), Mod2Gcc(expr)) )
   END
END CheckConstant ;


(*
   CodeMakeAdr - code the function MAKEADR.
*)

PROCEDURE CodeMakeAdr (q: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   r       : CARDINAL ;
   n       : CARDINAL ;
   type    : CARDINAL ;
   op      : QuadOperator ;
   bits,
   max,
   tmp,
   res,
   val     : tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   n := q ;
   REPEAT
      IF op1 > 0
      THEN
         DeclareConstant (CurrentQuadToken, op3)
      END ;
      n := GetNextQuad (n) ;
      GetQuad (n, op, r, op2, op3)
   UNTIL op = FunctValueOp ;

   n := q ;
   GetQuad (n, op, op1, op2, op3) ;
   res := Mod2Gcc (r) ;
   max := GetSizeOfInBits (Mod2Gcc(Address)) ;
   bits := GetIntegerZero (location) ;
   val := GetPointerZero (location) ;
   REPEAT
      location := TokenToLocation (CurrentQuadToken) ;
      IF (op = ParamOp) AND (op1 > 0)
      THEN
         IF GetType (op3) = NulSym
         THEN
            WriteFormat0 ('must supply typed constants to MAKEADR')
         ELSE
            type := GetType (op3) ;
            tmp := BuildConvert (location, GetPointerType (), Mod2Gcc (op3), FALSE) ;
            IF CompareTrees (bits, GetIntegerZero (location)) > 0
            THEN
               tmp := BuildLSL (location, tmp, bits, FALSE)
            END ;
            bits := BuildAdd (location, bits, GetSizeOfInBits (Mod2Gcc (type)), FALSE) ;
            val := BuildLogicalOrAddress (location, val, tmp, FALSE)
         END
      END ;
      SubQuad (n) ;
      n := GetNextQuad (n) ;
      GetQuad (n, op, op1, op2, op3)
   UNTIL op=FunctValueOp ;
   IF CompareTrees(bits, max) > 0
   THEN
      MetaErrorT0 (CurrentQuadToken,
                   'total number of bits specified as parameters to {%kMAKEADR} exceeds address width')
   END ;
   SubQuad(n) ;
   BuildAssignmentStatement (location, res, val)
END CodeMakeAdr ;


(*
   CodeBuiltinFunction - attempts to inline a function. Currently it only
                         inlines the SYSTEM function MAKEADR.
*)

PROCEDURE CodeBuiltinFunction (q: CARDINAL; nth, func, parameter: CARDINAL) ;
BEGIN
   IF nth = 0
   THEN
      InitBuiltinSyms (BuiltinTokenNo) ;
      IF func = MakeAdr
      THEN
         CodeMakeAdr (q, nth, func, parameter)
      END
   END
END CodeBuiltinFunction ;


(*
   FoldMakeAdr - attempts to fold the function MAKEADR.
*)

PROCEDURE FoldMakeAdr (tokenno: CARDINAL; p: WalkAction;
                       q: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   resolved: BOOLEAN ;
   r       : CARDINAL ;
   n       : CARDINAL ;
   op      : QuadOperator ;
   type    : CARDINAL ;
   bits,
   max,
   tmp,
   val     : tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation (tokenno) ;
   resolved := TRUE ;
   n := q ;
   r := op1 ;
   REPEAT
      IF r>0
      THEN
         TryDeclareConstant (tokenno, op3) ;
         IF NOT GccKnowsAbout (op3)
         THEN
            resolved := FALSE
         END
      END ;
      n := GetNextQuad (n) ;
      GetQuad (n, op, r, op2, op3)
   UNTIL op = FunctValueOp ;

   IF resolved AND IsConst (r)
   THEN
      n := q ;
      GetQuad (n, op, op1, op2, op3) ;
      max := GetSizeOfInBits (Mod2Gcc(Address)) ;
      bits := GetIntegerZero (location) ;
      val := GetPointerZero (location) ;
      REPEAT
         location := TokenToLocation (tokenno) ;
         IF (op = ParamOp) AND (op1 > 0)
         THEN
            IF GetType (op3) = NulSym
            THEN
               MetaErrorT0 (tokenno,
                            'constants passed to {%kMAKEADR} must be typed')
            ELSE
               type := GetType (op3) ;
               tmp := BuildConvert (location, GetPointerType (), Mod2Gcc (op3), FALSE) ;
               IF CompareTrees (bits, GetIntegerZero (location)) > 0
               THEN
                  tmp := BuildLSL (location, tmp, bits, FALSE)
               END ;
	       bits := BuildAdd (location, bits, GetSizeOfInBits (Mod2Gcc (type)), FALSE) ;
               val := BuildLogicalOrAddress (location, val, tmp, FALSE)
            END
         END ;
         SubQuad (n) ;
         n := GetNextQuad (n) ;
         GetQuad (n, op, op1, op2, op3)
      UNTIL op = FunctValueOp ;
      IF CompareTrees (bits, max) > 0
      THEN
         MetaErrorT0 (tokenno,
                      'total number of bits specified as parameters to {%kMAKEADR} exceeds address width')
      END ;
      PutConst (r, Address) ;
      AddModGcc (r, DeclareKnownConstant (location, Mod2Gcc (Address), val)) ;
      p (r) ;
      NoChange := FALSE ;
      SubQuad (n)
   END
END FoldMakeAdr ;


(*
   doParam - builds the parameter, op3, which is to be passed to
             procedure, op2.  The number of the parameter is op1.
*)

PROCEDURE doParam (quad: CARDINAL; paramtok: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (paramtok) ;
   DeclareConstant (paramtok, op3) ;
   DeclareConstructor (paramtok, quad, op3) ;
   BuildParam (location, CheckConvertCoerceParameter (paramtok, op1, op2, op3))
END doParam ;


(*
   FoldBuiltin - attempts to fold the gcc builtin function.
*)

PROCEDURE FoldBuiltin (tokenno: CARDINAL; p: WalkAction; q: CARDINAL) ;
VAR
   resolved  : BOOLEAN ;
   procedure,
   r         : CARDINAL ;
   n         : CARDINAL ;
   op1, op2,
   op3       : CARDINAL ;
   op        : QuadOperator ;
   val, call : tree ;
   location  : location_t ;
BEGIN
   GetQuad (q, op, op1, op2, op3) ;
   resolved := TRUE ;
   procedure := NulSym ;
   n := q ;
   r := op1 ;
   REPEAT
      IF r>0
      THEN
         TryDeclareConstant(tokenno, op3) ;
         IF NOT GccKnowsAbout(op3)
         THEN
            resolved := FALSE
         END
      END ;
      IF (op=CallOp) AND (NOT IsProcedure(op3))
      THEN
         (* cannot fold an indirect procedure function call *)
         resolved := FALSE
      END ;
      n := GetNextQuad(n) ;
      GetQuad(n, op, r, op2, op3)
   UNTIL op=FunctValueOp ;

   IF resolved AND IsConst(r)
   THEN
      n := q ;
      GetQuad(n, op, op1, op2, op3) ;
      REPEAT
         IF (op=ParamOp) AND (op1>0)
         THEN
            doParam (tokenno, n, op1, op2, op3)
         ELSIF op=CallOp
         THEN
            procedure := op3
         END ;
         SubQuad(n) ;
         n := GetNextQuad(n) ;
         GetQuad(n, op, op1, op2, op3)
      UNTIL op=FunctValueOp ;

      IF IsProcedureBuiltinAvailable (procedure)
      THEN
         location := TokenToLocation(tokenno) ;
         call := UseBuiltin (tokenno, procedure) ;
         val := BuildFunctValue (location, call) ;
         val := FoldAndStrip (val) ;
         PutConst(r, GetType(procedure)) ;
         AddModGcc(r, DeclareKnownConstant(location, Mod2Gcc(GetType(procedure)), val)) ;
         p(r) ;
         SetLastFunction(NIL)
      ELSE
         MetaErrorT1 (tokenno, 'gcc builtin procedure {%1Ead} cannot be used in a constant expression', procedure) ;
      END ;
      NoChange := FALSE ;
      SubQuad(n)
   END
END FoldBuiltin ;


(*
   FoldBuiltinFunction - attempts to inline a function. Currently it only
                         inlines the SYSTEM function MAKEADR.
*)

PROCEDURE FoldBuiltinFunction (tokenno: CARDINAL; p: WalkAction;
                               q: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF op1=0
   THEN
      (* must be a function as op1 is the return parameter *)
      IF op3=MakeAdr
      THEN
         FoldMakeAdr (tokenno, p, q, op1, op2, op3)
      ELSIF IsProcedure (op3) AND IsProcedureBuiltinAvailable (op3)
      THEN
         FoldBuiltin (tokenno, p, q)
      END
   END
END FoldBuiltinFunction ;


(*
   CodeParam - builds a parameter list.
               Note that we can ignore ModeOfAddr as any lvalue will
               have been created in a preceeding quadruple.
*)

PROCEDURE CodeParam (quad: CARDINAL) ;
VAR
   nopos,
   procedure,
   parameter,
   parampos  : CARDINAL ;
   nth       : CARDINAL ;
   compatible,
   constExpr,
   overflow  : BOOLEAN ;
   op        : QuadOperator ;
BEGIN
   GetQuadOtok (quad, parampos, op,
                nth, procedure, parameter,
                overflow, constExpr,
                nopos, nopos, nopos) ;
   compatible := TRUE ;
   IF nth=0
   THEN
      CodeBuiltinFunction (quad, nth, procedure, parameter)
   ELSE
      IF StrictTypeChecking
      THEN
         IF (nth <= NoOfParamAny (procedure))
         THEN
            compatible := ParameterTypeCompatible (parampos,
                                                   'parameter incompatibility when attempting to pass actual parameter {%2ad} to a {%kVAR} formal parameter {%3Ead} during call to procedure {%1ad}',
                                                   procedure, GetNthParamAny (procedure, nth),
                                                   parameter, nth, IsVarParamAny (procedure, nth))
         END
      END ;

      IF (nth <= NoOfParamAny (procedure)) AND
         IsVarParamAny (procedure, nth) AND IsConst (parameter)
      THEN
         MetaErrorT1 (parampos,
                      'cannot pass a constant {%1Ead} as a VAR parameter', parameter)
      ELSIF IsAModula2Type (parameter)
      THEN
         MetaErrorT2 (parampos,
                      'cannot pass a type {%1Ead} as a parameter to procedure {%2ad}',
                      parameter, procedure)
      ELSIF compatible
      THEN
         doParam (quad, parampos, nth, procedure, parameter)
      END
   END
END CodeParam ;


(*
   Replace - replace the entry for sym in the double entry bookkeeping with sym/tree.
*)

PROCEDURE Replace (sym: CARDINAL; gcc: tree) ;
BEGIN
   IF GccKnowsAbout (sym)
   THEN
      RemoveMod2Gcc (sym)
   END ;
   AddModGcc (sym, gcc)
END Replace ;


(*
   CodeFunctValue - retrieves the function return value and assigns it
                    into a variable.
*)

PROCEDURE CodeFunctValue (location: location_t; op1: CARDINAL) ;
VAR
   call,
   value: tree ;
BEGIN
   (*
      operator : FunctValueOp
      op1 : The Returned Variable
      op3 : The Function Returning this Variable
   *)
   IF EnableSSA AND IsVariableSSA (op1)
   THEN
      call := GetLastFunction () ;
      SetLastFunction (NIL) ;
      Replace (op1, call)
   ELSE
      value := BuildFunctValue (location, Mod2Gcc (op1)) ;
      (* AddStatement (location, CheckCleanup (location, op3, value, call))  *)
      AddStatement (location, value)
   END
END CodeFunctValue ;


(*
   FoldStringLength -
*)

PROCEDURE FoldStringLength (quad: CARDINAL; p: WalkAction) ;
VAR
   op              : QuadOperator ;
   des, none, expr : CARDINAL ;
   stroppos,
   despos, nonepos,
   exprpos         : CARDINAL ;
   constExpr,
   overflowChecking: BOOLEAN ;
   location        : location_t ;
BEGIN
   GetQuadOtok (quad, stroppos, op, des, none, expr,
                overflowChecking, constExpr,
                despos, nonepos, exprpos) ;
   IF IsConstStr (expr) AND IsConstStrKnown (expr)
   THEN
      location := TokenToLocation (stroppos) ;
      PushCard (GetStringLength (exprpos, expr)) ;
      AddModGcc (des, BuildConvert (location, Mod2Gcc (GetType (des)), PopIntegerTree (), FALSE)) ;
      RemoveQuad (p, des, quad)
   END
END FoldStringLength ;


(*
   FoldStringConvertM2nul - attempt to assign the des with the string contents from expr.
                            It also marks the des as a m2 string which must be nul terminated.
                            The front end uses double book keeping and it is easier to have
                            different m2 string symbols each of which map onto a slightly different
                            gcc string tree.
*)

PROCEDURE FoldStringConvertM2nul (quad: CARDINAL; p: WalkAction) ;
VAR
   op              : QuadOperator ;
   des, none, expr : CARDINAL ;
   stroppos,
   despos, nonepos,
   exprpos         : CARDINAL ;
   s               : String ;
   constExpr,
   overflowChecking: BOOLEAN ;
BEGIN
   GetQuadOtok (quad, stroppos, op, des, none, expr,
                overflowChecking, constExpr,
                despos, nonepos, exprpos) ;
   IF IsConstStr (expr) AND IsConstStrKnown (expr)
   THEN
      s := GetStr (exprpos, expr) ;
      PutConstStringKnown (stroppos, des, makekey (string (s)), FALSE, TRUE) ;
      TryDeclareConstant (despos, des) ;
      p (des) ;
      NoChange := FALSE ;
      SubQuad (quad) ;
      s := KillString (s)
   END
END FoldStringConvertM2nul ;


(*
   FoldStringConvertCnul -attempt to assign the des with the string contents from expr.
                          It also marks the des as a C string which must be nul terminated.
*)

PROCEDURE FoldStringConvertCnul (quad: CARDINAL; p: WalkAction) ;
VAR
   op              : QuadOperator ;
   des, none, expr : CARDINAL ;
   stroppos,
   despos, nonepos,
   exprpos         : CARDINAL ;
   s               : String ;
   constExpr,
   overflowChecking: BOOLEAN ;
BEGIN
   GetQuadOtok (quad, stroppos, op, des, none, expr,
                overflowChecking, constExpr,
                despos, nonepos, exprpos) ;
   IF IsConstStr (expr) AND IsConstStrKnown (expr)
   THEN
      s := GetStr (exprpos, expr) ;
      PutConstStringKnown (stroppos, des, makekey (string (s)), TRUE, TRUE) ;
      TryDeclareConstant (despos, des) ;
      p (des) ;
      NoChange := FALSE ;
      SubQuad (quad) ;
      s := KillString (s)
   END
END FoldStringConvertCnul ;


(*
   Addr Operator - generates the address of a variable (op1 = &op3).
*)

PROCEDURE CodeAddr (tokenno: CARDINAL; quad: CARDINAL; op1, op3: CARDINAL) ;
VAR
   value   : tree ;
   type    : CARDINAL ;
   location: location_t ;
BEGIN
   IF IsConst(op3) AND (NOT IsConstString(op3))
   THEN
      MetaErrorT1 (tokenno, 'error in expression, trying to find the address of a constant {%1Ead}', op3)
   ELSE
      IF IsConstString (op3) AND (NOT IsConstStringKnown (op3))
      THEN
         printf1 ("failure in quad: %d\n", quad)
      END ;
      location := TokenToLocation (tokenno) ;
      type := SkipType (GetType (op3)) ;
      DeclareConstant (tokenno, op3) ;  (* we might be asked to find the address of a constant string *)
      DeclareConstructor (tokenno, quad, op3) ;
      IF (IsConst (op3) AND (type=Char)) OR IsConstString (op3)
      THEN
         value := BuildStringConstant (KeyToCharStar (GetString (op3)), GetStringLength (tokenno, op3))
      ELSE
         value := Mod2Gcc (op3)
      END ;
      BuildAssignmentStatement (location,
                                Mod2Gcc (op1),
                                BuildAddr (location, value, FALSE))
   END
END CodeAddr ;


PROCEDURE stop ; BEGIN END stop ;

PROCEDURE CheckStop (q: CARDINAL) ;
BEGIN
   IF q=3827
   THEN
      stop
   END
END CheckStop ;


(*
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
*)

PROCEDURE FoldBecomes (p: WalkAction; bb: BasicBlock; quad: CARDINAL) ;
VAR
   op            : QuadOperator ;
   des, op2, expr: CARDINAL ;
BEGIN
   IF DeclaredOperandsBecomes (p, quad)
   THEN
      IF (NOT IsConditionalBooleanQuad (quad)) OR IsBasicBlockFirst (bb)
      THEN
         IF TypeCheckBecomes (p, quad)
         THEN
            PerformFoldBecomes (p, quad)
         ELSE
            GetQuad (quad, op, des, op2, expr) ;
            RemoveQuad (p, des, quad)
         END
      END
   END
END FoldBecomes ;


(*
   TryDeclareConst -
*)

PROCEDURE TryDeclareConst (tokenno: CARDINAL; sym: CARDINAL) ;
BEGIN
   (* Check whether expr is a constant literal and if so declare it.  *)
   TryDeclareConstant (tokenno, sym) ;
   (* Check whether expr is a const constructor and if so declare it.  *)
   TryDeclareConstructor (tokenno, sym)
END TryDeclareConst ;


(*
   RemoveQuad - remove quad and ensure p (des) is called.
*)

PROCEDURE RemoveQuad (p: WalkAction; des: CARDINAL; quad: CARDINAL) ;
BEGIN
   p (des) ;
   NoChange := FALSE ;
   SubQuad (quad)
END RemoveQuad ;


(*
   DeclaredOperandsBecomes -
*)

PROCEDURE DeclaredOperandsBecomes (p: WalkAction; quad: CARDINAL) : BOOLEAN ;
VAR
   des, op2, expr     : CARDINAL ;
   constExpr,
   overflowChecking   : BOOLEAN ;
   despos, op2pos,
   exprpos, becomespos: CARDINAL ;
   op                 : QuadOperator ;
BEGIN
   GetQuadOtok (quad, becomespos, op,
                des, op2, expr,
                overflowChecking, constExpr,
                despos, op2pos, exprpos) ;
   Assert (op2pos = UnknownTokenNo) ;
   TryDeclareConst (exprpos, expr) ;
   IF IsConst (des) AND IsConstant (expr)
   THEN
      (* Constant folding taking place, but have we resolved op3 yet?  *)
      IF GccKnowsAbout (expr)
      THEN
         (* Now we can tell gcc about the relationship between des and expr.  *)
         (* RemoveSSAPlaceholder (quad, des) ;  *)
         IF GccKnowsAbout (des)
         THEN
            MetaErrorT1 (despos, 'constant {%1Ead} should not be reassigned', des) ;
            RemoveQuad (p, des, quad) ;
            RETURN FALSE
         ELSE
            RETURN TRUE
         END
      END
   END ;
   RETURN FALSE
END DeclaredOperandsBecomes ;


(*
   TypeCheckBecomes - returns TRUE if the type check succeeds.
*)

PROCEDURE TypeCheckBecomes (p: WalkAction; quad: CARDINAL) : BOOLEAN ;
VAR
   des, op2, expr     : CARDINAL ;
   constExpr,
   overflowChecking   : BOOLEAN ;
   despos, op2pos,
   exprpos, becomespos: CARDINAL ;
   op                 : QuadOperator ;
BEGIN
   GetQuadOtok (quad, becomespos, op,
                des, op2, expr,
                overflowChecking, constExpr,
                despos, op2pos, exprpos) ;
   Assert (op2pos = UnknownTokenNo) ;
   IF StrictTypeChecking AND
      (NOT AssignmentTypeCompatible (despos, "", des, expr))
   THEN
      MetaErrorT2 (MakeVirtualTok (becomespos, despos, exprpos),
                   'assignment check caught mismatch between {%1Ead} and {%2ad}',
                   des, expr) ;
      RemoveQuad (p, des, quad) ;
      RETURN FALSE
   END ;
   RETURN TRUE
END TypeCheckBecomes ;


(*
   PerformFoldBecomes - attempts to fold quad.  It propagates constant strings
                        and attempts to declare des providing it is a constant
                        and expr is resolved.
*)

PROCEDURE PerformFoldBecomes (p: WalkAction; quad: CARDINAL) ;
VAR
   des, op2, expr     : CARDINAL ;
   constExpr,
   overflowChecking   : BOOLEAN ;
   despos, op2pos,
   exprpos, becomespos,
   virtpos            : CARDINAL ;
   op                 : QuadOperator ;
BEGIN
   GetQuadOtok (quad, becomespos, op,
                des, op2, expr,
                overflowChecking, constExpr,
                despos, op2pos, exprpos) ;
   Assert (op2pos = UnknownTokenNo) ;
   IF IsConst (des) AND IsConstString (expr)
   THEN
      IF IsConstStringKnown (expr) AND (NOT IsConstStringKnown (des))
      THEN
         CopyConstString (exprpos, des, expr)
      END
   ELSIF GetType (des) = NulSym
   THEN
      Assert (GetType (expr) # NulSym) ;
      PutConst (des, GetType (expr))
   END ;
   IF GetType (expr) = NulSym
   THEN
      CheckOrResetOverflow (exprpos, Mod2Gcc (expr), MustCheckOverflow (quad)) ;
      AddModGcc (des, Mod2Gcc (expr))
   ELSE
      IF NOT GccKnowsAbout (GetType (des))
      THEN
         RETURN
      END ;
      IF IsProcedure (expr)
      THEN
         AddModGcc (des,
                    BuildConvert (TokenToLocation (exprpos),
                                  Mod2Gcc (GetType (des)),
                                  BuildAddr (TokenToLocation (exprpos),
                                             Mod2Gcc (expr), FALSE), TRUE))
      ELSIF IsValueSolved (expr)
      THEN
         PushValue (expr) ;
         IF IsValueTypeReal ()
         THEN
            CheckOrResetOverflow (exprpos, PopRealTree (), MustCheckOverflow (quad)) ;
            PushValue (expr) ;
            AddModGcc (des, PopRealTree ())
         ELSIF IsValueTypeSet ()
         THEN
            PopValue (des) ;
            PutConstSet (des)
         ELSIF IsValueTypeConstructor () OR IsValueTypeArray () OR IsValueTypeRecord ()
         THEN
            PopValue (des) ;
            PutConstructor (des)
         ELSIF IsValueTypeComplex ()
         THEN
            CheckOrResetOverflow (exprpos, PopComplexTree (), MustCheckOverflow (quad)) ;
            PushValue (expr) ;
            PopValue (des)
         ELSE
            CheckOrResetOverflow (exprpos, PopIntegerTree (), MustCheckOverflow (quad)) ;
            IF GetType (des) = NulSym
            THEN
               PushValue (expr) ;
               AddModGcc (des, PopIntegerTree ())
            ELSE
               virtpos := MakeVirtualTok (becomespos, despos, exprpos) ;
               PushValue (expr) ;
               AddModGcc (des, BuildConvert (TokenToLocation (virtpos),
                                             Mod2Gcc (GetType (des)), PopIntegerTree (), FALSE))
            END
         END
      ELSE
         virtpos := MakeVirtualTok (becomespos, despos, exprpos) ;
         CheckOrResetOverflow (exprpos, Mod2Gcc (des), MustCheckOverflow (quad)) ;
         AddModGcc (des,
                    BuildConvert (TokenToLocation (virtpos),
                                  Mod2Gcc (GetType (des)),
                                  DeclareKnownConstant (TokenToLocation (virtpos),
                                                        Mod2Gcc (GetType (expr)),
                                                        Mod2Gcc (expr)), FALSE))
      END
   END ;
   RemoveQuad (p, des, quad) ;
   Assert (RememberConstant(Mod2Gcc (des)) = Mod2Gcc (des))
END PerformFoldBecomes ;


VAR
   tryBlock: tree ;    (* this must be placed into gccgm2 and it must follow the
                          current function scope - ie it needs work with nested procedures *)
   handlerBlock: tree ;


(*
   CodeTry - starts building a GCC 'try' node.
*)

PROCEDURE CodeTry ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   handlerBlock := NIL ;
   tryBlock := BuildTryBegin (location)
END CodeTry ;


(*
   CodeThrow - builds a GCC 'throw' node.
*)

PROCEDURE CodeThrow (value: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   IF value = NulSym
   THEN
      AddStatement (location, BuildThrow (location, tree (NIL)))
   ELSE
      DeclareConstant (CurrentQuadToken, value) ;  (* checks to see whether it is a constant and declares it *)
      AddStatement (location, BuildThrow (location, BuildConvert (location,
                                                                  GetIntegerType (),
                                                                  Mod2Gcc (value), FALSE)))
   END
END CodeThrow ;


PROCEDURE CodeRetry (destQuad: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   BuildGoto (location, string (CreateLabelName (destQuad)))
END CodeRetry ;


PROCEDURE CodeCatchBegin ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   BuildTryEnd (tryBlock) ;
   handlerBlock := BuildCatchBegin (location)
END CodeCatchBegin ;


PROCEDURE CodeCatchEnd ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   tryBlock := BuildCatchEnd (location, handlerBlock, tryBlock) ;
   AddStatement (location, tryBlock)
END CodeCatchEnd ;


(*
   DescribeTypeError -
*)

PROCEDURE DescribeTypeError (token: CARDINAL;
                             op1, op2: CARDINAL) ;
BEGIN
   MetaErrorT2(token, 'incompatible set types in assignment, assignment between {%1ERad} and {%2ad}', op1, op2) ;
   MetaError2('set types are {%1CDtsad} and {%2Dtsad}', op1, op2)
END DescribeTypeError ;


(*
   DefaultConvertGM2 - provides a simple mapping between
                       front end data types and GCC equivalents.
                       This is only used to aid assignment of
                       typed constants.
*)

PROCEDURE DefaultConvertGM2 (sym: CARDINAL) : tree ;
BEGIN
   sym := SkipType (sym) ;
   IF sym=Bitset
   THEN
      RETURN( GetWordType() )
   ELSE
      RETURN( Mod2Gcc(sym) )
   END
END DefaultConvertGM2 ;


(*
   FoldConstBecomes - returns a Tree containing op3.
                      The tree will have been folded and
                      type converted if necessary.
*)

PROCEDURE FoldConstBecomes (tokenno: CARDINAL;
                            op1, op3: CARDINAL) : tree ;
VAR
   t, type : tree ;
   location: location_t ;
BEGIN
   IF IsConstSet(op3) OR ((SkipType(GetType(op3))#NulSym) AND
                          IsSet(SkipType(GetType(op3))))
   THEN
      (* we have not checked set compatibility in
         M2Quads.mod:BuildAssignmentTree so we do it here.
      *)
(*
      IF (Iso AND (SkipType(GetType(op1))#SkipType(GetType(op3)))) OR
         (Pim AND ((SkipType(GetType(op1))#SkipType(GetType(op3))) AND
                   (SkipType(GetType(op1))#Bitset) AND
                   (SkipType(GetType(op3))#Bitset)))
*)
      IF SkipType(GetTypeMode(op1))#SkipType(GetTypeMode(op3))
      THEN
         DescribeTypeError (tokenno, op1, op3) ;
         RETURN( Mod2Gcc (op1) ) (* we might crash if we execute the BuildAssignmentTree with op3 *)
      END
   END ;
   location := TokenToLocation (tokenno) ;
   TryDeclareConstant (tokenno, op3) ;
   t := Mod2Gcc (op3) ;
   Assert (t#NIL) ;
   IF IsConstant (op3)
   THEN
      IF IsProcedure (op3)
      THEN
         RETURN t
	 (*
         t := BuildConvert(location, Mod2Gcc(GetType(op1)), BuildAddr(location, Mod2Gcc(op3), FALSE), TRUE)
         *)
      ELSIF (NOT IsConstString (op3)) AND (NOT IsConstSet (op3)) AND
         (SkipType (GetType (op3)) # SkipType (GetType (op1)))
      THEN
         type := DefaultConvertGM2 (GetType(op1)) ;  (* do we need this now? --fixme-- *)
         t := ConvertConstantAndCheck (location, type, t)
      ELSIF GetType (op1) # NulSym
      THEN
         t := StringToChar (Mod2Gcc (op3), GetType (op1), op3)
      END
   END ;
   RETURN( t )
END FoldConstBecomes ;


(*
   PrepareCopyString - returns two trees:
                       length    number of bytes to be copied (including the nul if room)
                       srcTreeType the new string type (with the extra nul character).

                       Pre condition:  destStrType the dest type string.
                                       src is the original string (without a nul)
                                       to be copied.
                       Post condition: TRUE or FALSE is returned.
                                       if true length and srcTreeType will be assigned
                                       else length is set to the maximum length to be
                                            copied and srcTree is set to the max length
                                            which fits in dest.
*)

PROCEDURE PrepareCopyString (tokenno: CARDINAL; VAR length, srcTree: tree;
                             src, destStrType: CARDINAL) : BOOLEAN ;
VAR
   location : location_t ;
   intLength: INTEGER ;
BEGIN
   location := TokenToLocation (tokenno) ;
   Assert (IsArray (SkipType (destStrType))) ;
   (* Handle string assignments:
      VAR
         str: ARRAY [0..10] OF CHAR ;
         ch : CHAR ;

         str := 'abcde' but not ch := 'a'
   *)
   IF GetType (src) = Char
   THEN
      (*
       *  Create string from char and add nul to the end, nul is
       *  added by BuildStringConstant.  In modula-2 an array must
       *  have at least one element.
       *)
      length := GetIntegerOne (location) ;
      PushIntegerTree (FindSize (tokenno, src)) ;
      PushIntegerTree (FindSize (tokenno, destStrType)) ;
      IF Less (tokenno)
      THEN
         (* There is room for the extra <nul> character.  *)
         length := BuildAdd (location, length,
                             GetIntegerOne (location), FALSE)
      END
   ELSE
      PushIntegerTree (FindSize (tokenno, src)) ;
      PushIntegerTree (FindSize (tokenno, destStrType)) ;
      IF Less (tokenno)
      THEN
         (* There is room for the extra <nul> character.  *)
         length := BuildAdd (location, FindSize (tokenno, src),
                             GetIntegerOne (location), FALSE) ;
         srcTree := Mod2Gcc (src)
      ELSE
         (* We need to truncate the <nul> at least.  *)
         length := FindSize (tokenno, destStrType) ;
         PushIntegerTree (FindSize (tokenno, src)) ;
         PushIntegerTree (length) ;
         (* Greater or Equal so return max characters in the array.  *)
         IF Gre (tokenno)
         THEN
            (* Create a new string without non nul characters to be gimple safe.
               But return FALSE indicating an overflow.  *)
            intLength := GetCstInteger (length) ;
            srcTree := BuildStringConstant (KeyToCharStar (GetString (src)), intLength) ;
            srcTree := ConvertString (Mod2Gcc (destStrType), srcTree) ;
            RETURN FALSE
         END
      END
   END ;
   intLength := GetCstInteger (length) ;
   srcTree := BuildStringConstant (KeyToCharStar (GetString (src)), intLength) ;
   srcTree := ConvertString (Mod2Gcc (destStrType), srcTree) ;
   RETURN TRUE
END PrepareCopyString ;


(*
   checkArrayElements - return TRUE if des or expr are not arrays.
                        If they are arrays and have different number of
                        elements return FALSE, otherwise TRUE.
*)

PROCEDURE checkArrayElements (des, expr: CARDINAL; virtpos, despos, exprpos: CARDINAL) : BOOLEAN ;
VAR
   e1, e3: tree ;
   t1, t3: CARDINAL ;
BEGIN
   t1 := GetType (des) ;
   t3 := GetType (expr) ;
   IF (t1 # NulSym) AND (t3 # NulSym) AND
      IsArray (SkipType (GetType (expr))) AND IsArray (SkipType (GetType (des)))
   THEN
      (* both arrays continue checking *)
      e1 := GetArrayNoOfElements (TokenToLocation (despos),
                                  Mod2Gcc (SkipType (GetType (des)))) ;
      e3 := GetArrayNoOfElements (TokenToLocation (exprpos),
                                  Mod2Gcc (SkipType (GetType (expr)))) ;
      IF CompareTrees (e1, e3) # 0
      THEN
         MetaErrorT2 (virtpos,
                      'not allowed to assign array {%2Ead} to {%1ad} as they have a different number of elements',
                      des, expr) ;
         RETURN( FALSE )
      END
   END ;
   RETURN( TRUE )
END checkArrayElements ;


(*
   CodeInitAddress -
*)

PROCEDURE CodeInitAddress (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   DeclareConstant (CurrentQuadToken, op3) ;  (* checks to see whether it is a constant and declares it *)
   DeclareConstructor (CurrentQuadToken, quad, op3) ;

   location := TokenToLocation (CurrentQuadToken) ;

   Assert (op2 = NulSym) ;
   Assert (GetMode (op1) = LeftValue) ;
   BuildAssignmentStatement (location,
                             Mod2Gcc (op1),
                             BuildConvert (location, GetPointerType (), Mod2Gcc (op3), FALSE))
END CodeInitAddress ;


(*
   checkRecordTypes - returns TRUE if des is not a record or if the record
                      is the same type as expr.
*)

PROCEDURE checkRecordTypes (des, expr: CARDINAL; virtpos: CARDINAL) : BOOLEAN ;
VAR
   t1, t2: CARDINAL ;
BEGIN
   IF (GetType (des) = NulSym) OR (GetMode (des) = LeftValue)
   THEN
      RETURN( TRUE )
   ELSE
      t1 := SkipType (GetType (des)) ;
      IF IsRecord (t1)
      THEN
         IF GetType (expr) = NulSym
         THEN
            MetaErrorT2 (virtpos,
                         'cannot assign an operand of type {%1Ets} to a record type {%2tsa}',
                         expr, des) ;
            RETURN( FALSE )
         ELSE
            t2 := SkipType (GetType (expr)) ;
	    IF t1 = t2
            THEN
               RETURN( TRUE )
            ELSE
               MetaErrorT2 (virtpos,
                            'cannot assign an operand of type {%1ts} to a record type {%2tsa}',
                            expr, des) ;
	       RETURN( FALSE )
            END
         END
      END
   END ;
   RETURN( TRUE )
END checkRecordTypes ;


(*
   checkIncorrectMeta - checks to see if des and expr are assignment compatible is allows
                        generic system types to be assigned.
*)

PROCEDURE checkIncorrectMeta (des, expr: CARDINAL; virtpos: CARDINAL) : BOOLEAN ;
VAR
   t1, t2: CARDINAL ;
BEGIN
   t1 := SkipType (GetType (des)) ;
   t2 := SkipType (GetType (expr)) ;
   IF (t1 = NulSym) OR (GetMode(des) = LeftValue) OR
      (t2 = NulSym) OR (GetMode(expr) = LeftValue)
   THEN
      RETURN( TRUE )
   ELSIF (t1 # t2) AND (NOT IsGenericSystemType (t1)) AND (NOT IsGenericSystemType (t2))
   THEN
      IF IsArray (t1) OR IsSet (t1) OR IsRecord (t1)
      THEN
         IF NOT IsAssignmentCompatible (t1, t2)
         THEN
            ErrorMessageDecl (virtpos,
                              'illegal assignment error between {%1Etad} and {%2tad}',
                              des, expr, TRUE) ;
	    RETURN( FALSE )
         END
      END
   END ;
   RETURN( TRUE )
END checkIncorrectMeta ;


(*
   checkBecomes - returns TRUE if the checks pass.
*)

PROCEDURE checkBecomes (des, expr: CARDINAL; virtpos, despos, exprpos: CARDINAL) : BOOLEAN ;
BEGIN
   IF (NOT checkArrayElements (des, expr, virtpos, despos, exprpos)) OR
      (NOT checkRecordTypes (des, expr, virtpos)) OR
      (NOT checkIncorrectMeta (des, expr, virtpos))
   THEN
      RETURN FALSE
   END ;
   RETURN TRUE
END checkBecomes ;


(*
   checkDeclare - checks to see if sym is declared and if it is not then declare it.
*)

PROCEDURE checkDeclare (sym: CARDINAL) ;
BEGIN
   IF IsTemporary (sym) AND IsVariableSSA (sym) AND (NOT GccKnowsAbout (sym))
   THEN
      DeclareLocalVariable (sym)
   END
END checkDeclare ;


(*
------------------------------------------------------------------------------
   := Operator
------------------------------------------------------------------------------
   Sym1<I> := Sym3<I>           := produces a constant
   Sym1<O> := Sym3<O>           := has the effect Mem[Sym1<I>] := Mem[Sym3<I>]
*)

PROCEDURE CodeBecomes (quad: CARDINAL) ;
VAR
   constExpr,
   overflowChecking: BOOLEAN ;
   op              : QuadOperator ;
   des, op2, expr   : CARDINAL ;
   virtpos,
   becomespos,
   despos,
   op2pos,
   exprpos          : CARDINAL ;
   length,
   exprt            : tree ;
   location        : location_t ;
BEGIN
   GetQuadOtok (quad, becomespos, op, des, op2, expr,
                overflowChecking, constExpr,
                despos, op2pos, exprpos) ;
   Assert (op2pos = UnknownTokenNo) ;
   DeclareConstant (exprpos, expr) ;  (* Check to see whether expr is a constant and declare it.  *)
   DeclareConstructor (exprpos, quad, expr) ;
   virtpos := MakeVirtualTok (becomespos, despos, exprpos) ;
   location := TokenToLocation (virtpos) ;

   IF StrictTypeChecking AND
      (NOT AssignmentTypeCompatible (virtpos, "", des, expr))
   THEN
      ErrorMessageDecl (virtpos,
                        'assignment check caught mismatch between {%1Ead} and {%2ad}',
                        des, expr, TRUE)
   END ;
   IF IsConstString (expr) AND (NOT IsConstStringKnown (expr))
   THEN
      MetaErrorT2 (virtpos,
                   'internal error: CodeBecomes {%1Aad} in quad {%2n}', des, quad)
   END ;
   IF IsConst (des) AND (NOT GccKnowsAbout (des))
   THEN
      ConstantKnownAndUsed (des, CheckConstant (virtpos, des, expr))
   ELSIF IsConstString (expr) AND (SkipTypeAndSubrange (GetType (des)) # Char)
   THEN
      checkDeclare (des) ;
      IF NOT PrepareCopyString (becomespos, length, exprt, expr, SkipType (GetType (des)))
      THEN
         ErrorMessageDecl (virtpos,
                           'string constant {%1Ea} is too large to be assigned to the array {%2ad}',
                           expr, des, TRUE)
      END ;
      AddStatement (location,
                    MaybeDebugBuiltinMemcpy (location,
                                             BuildAddr (location, Mod2Gcc (des), FALSE),
                                             BuildAddr (location, exprt, FALSE),
                                             length))
   ELSE
      IF ((IsGenericSystemType(SkipType(GetType(des))) #
           IsGenericSystemType(SkipType(GetType(expr)))) OR
          (IsUnbounded(SkipType(GetType(des))) AND
           IsUnbounded(SkipType(GetType(expr))) AND
           (IsGenericSystemType(SkipType(GetType(GetType(des)))) #
            IsGenericSystemType(SkipType(GetType(GetType(expr))))))) AND
         (NOT IsConstant(expr))
      THEN
         checkDeclare (des) ;
         AddStatement (location,
                       MaybeDebugBuiltinMemcpy (location,
                                                BuildAddr(location, Mod2Gcc (des), FALSE),
                                                BuildAddr(location, Mod2Gcc (expr), FALSE),
                                                BuildSize(location, Mod2Gcc (des), FALSE)))
      ELSE
         IF checkBecomes (des, expr, virtpos, despos, exprpos)
         THEN
            IF IsVar (des) AND IsVariableSSA (des)
            THEN
               Replace (des, FoldConstBecomes (virtpos, des, expr))
            ELSE
               BuildAssignmentStatement (location,
                                         Mod2Gcc (des),
                                         FoldConstBecomes (virtpos, des, expr))
            END
         ELSE
            SubQuad (quad)  (* We don't want multiple errors for the quad.  *)
         END
      END
   END
END CodeBecomes ;


(*
   LValueToGenericPtr - returns a Tree representing symbol, sym.
                        It coerces a lvalue into an internal pointer type
*)

PROCEDURE LValueToGenericPtr (location: location_t; sym: CARDINAL) : tree ;
VAR
   t: tree ;
BEGIN
   t := Mod2Gcc (sym) ;
   IF t = NIL
   THEN
      InternalError ('expecting symbol to be resolved')
   END ;
   IF GetMode (sym) = LeftValue
   THEN
      t := BuildConvert (location, GetPointerType (), t, FALSE)
   END ;
   RETURN t
END LValueToGenericPtr ;


(*
   LValueToGenericPtrOrConvert - if sym is an lvalue then convert to pointer type
                                 else convert to type, type. Return the converted tree.
*)

PROCEDURE LValueToGenericPtrOrConvert (sym: CARDINAL; type: tree) : tree ;
VAR
   n       : tree ;
   location: location_t ;
BEGIN
   n := Mod2Gcc (sym) ;
   location := TokenToLocation (GetDeclaredMod (sym)) ;
   IF n = NIL
   THEN
      InternalError ('expecting symbol to be resolved')
   END ;
   IF GetMode (sym) = LeftValue
   THEN
      n := BuildConvert (location, GetPointerType (), n, FALSE)
   ELSE
      n := BuildConvert (location, type, n, FALSE)
   END ;
   RETURN n
END LValueToGenericPtrOrConvert ;


(*
   ZConstToTypedConst - checks whether op1 and op2 are constants and
                        coerces, t, appropriately.
*)

PROCEDURE ZConstToTypedConst (t: tree; op1, op2: CARDINAL) : tree ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(op2)) ;
   IF IsConst(op1) AND IsConst(op2)
   THEN
      (* leave, Z type, alone *)
      RETURN( t )
   ELSIF IsConst(op1)
   THEN
      IF GetMode(op2)=LeftValue
      THEN
         (* convert, Z type const into type of non constant operand *)
         RETURN( BuildConvert(location, GetPointerType(), t, FALSE) )
      ELSE
         (* convert, Z type const into type of non constant operand *)
         RETURN( BuildConvert(location, Mod2Gcc(FindType(op2)), t, FALSE) )
      END
   ELSIF IsConst(op2)
   THEN
      IF GetMode(op1)=LeftValue
      THEN
         (* convert, Z type const into type of non constant operand *)
         RETURN( BuildConvert(location, GetPointerType(), t, FALSE) )
      ELSE
         (* convert, Z type const into type of non constant operand *)
         RETURN( BuildConvert(location, Mod2Gcc(FindType(op1)), t, FALSE) )
      END
   ELSE
      (* neither operands are constants, leave alone *)
      RETURN( t )
   END
END ZConstToTypedConst ;


(*
   FoldBinary - check whether we can fold the binop operation.
*)

PROCEDURE FoldBinary (tokenno: CARDINAL; p: WalkAction; binop: BuildBinProcedure;
                      quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   tl, tr, tv, resType: tree ;
   location           : location_t ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant(tokenno, op3) ;
   TryDeclareConstant(tokenno, op2) ;
   location := TokenToLocation(tokenno) ;
   IF IsConst(op2) AND IsConst(op3)
   THEN
      IF GccKnowsAbout(op2) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            Assert(MixTypes(FindType(op3), FindType(op2), tokenno)#NulSym) ;
            PutConst(op1, MixTypes(FindType(op3), FindType(op2), tokenno)) ;

            tl := LValueToGenericPtr(location, op2) ;
            tr := LValueToGenericPtr(location, op3) ;

            IF GetType(op1)=NulSym
            THEN
               resType := GetM2ZType()
            ELSE
               resType := Mod2Gcc(GetType(op1))
            END ;

            tl := BuildConvert(location, resType, tl, FALSE) ;
            tr := BuildConvert(location, resType, tr, FALSE) ;

            tv := binop(location, tl, tr, TRUE) ;
            CheckOrResetOverflow(tokenno, tv, MustCheckOverflow(quad)) ;

            AddModGcc(op1, DeclareKnownConstant(location, resType, tv)) ;

            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         ELSE
            (* we can still fold the expression, but not the assignment,
               however, we will not do this here but in CodeBinary
             *)
         END
      END
   END
END FoldBinary ;


(*
   ConvertBinaryOperands -
*)

PROCEDURE ConvertBinaryOperands (location: location_t; VAR tl, tr: tree; type, op2, op3: CARDINAL) ;
BEGIN
   tl := NIL ;
   tr := NIL ;
   IF GetMode(op2)=LeftValue
   THEN
      tl := LValueToGenericPtr(location, op2) ;
      type := Address
   END ;
   IF GetMode(op3)=LeftValue
   THEN
      tr := LValueToGenericPtr(location, op3) ;
      type := Address
   END ;
   IF (tl=NIL) AND (tr=NIL)
   THEN
      tl := BuildConvert(location, Mod2Gcc(type), Mod2Gcc(op2), FALSE) ;
      tr := BuildConvert(location, Mod2Gcc(type), Mod2Gcc(op3), FALSE)
   ELSIF tl=NIL
   THEN
      tl := BuildConvert(location, Mod2Gcc(type), Mod2Gcc(op2), FALSE)
   ELSIF tr=NIL
   THEN
      tr := BuildConvert(location, Mod2Gcc(type), Mod2Gcc(op3), FALSE)
   END
END ConvertBinaryOperands ;


(*
   CodeBinaryCheck - encode a binary arithmetic operation.
*)

PROCEDURE CodeBinaryCheck (binop: BuildBinCheckProcedure; quad: CARDINAL) ;
VAR
   op        : QuadOperator ;
   op1, op2,
   op3       : CARDINAL ;
   op1pos,
   op2pos,
   op3pos,
   lowestType,
   type      : CARDINAL ;
   min, max,
   lowest,
   tv,
   tl, tr    : tree ;
   location  : location_t ;
BEGIN
   (* firstly ensure that constant literals are declared.  *)
   GetQuadtok (quad, op, op1, op2, op3, op1pos, op2pos, op3pos) ;
   DeclareConstant (op3pos, op3) ;
   DeclareConstant (op2pos, op2) ;
   location := TokenToLocation (op1pos) ;

   type := MixTypesBinary (op2, op3, op1pos, MustCheckOverflow (quad)) ;
   ConvertBinaryOperands (location, tl, tr, type, op2, op3) ;

   lowestType := GetLType (op1) ;
   lowest := Mod2Gcc (lowestType) ;
   IF GetMinMax (CurrentQuadToken, lowestType, min, max)
   THEN
      tv := binop (location, tl, tr, lowest, min, max)
   ELSE
      tv := binop (location, tl, tr, NIL, NIL, NIL)
   END ;
   CheckOrResetOverflow (op1pos, tv, MustCheckOverflow (quad)) ;
   IF IsConst (op1)
   THEN
      (* still have a constant which was not resolved, pass it to gcc.  *)
      Assert (MixTypes (FindType (op3), FindType (op2), op3pos) # NulSym) ;

      PutConst (op1, MixTypes (FindType (op3), FindType (op2), op3pos)) ;
      ConstantKnownAndUsed (op1, DeclareKnownConstant (location, Mod2Gcc (GetType (op3)), tv))
   ELSE
      IF EnableSSA AND IsVariableSSA (op1)
      THEN
         Replace (op1, tv)
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (op1), tv)
      END
   END
END CodeBinaryCheck ;


(*
   MixTypesBinary - depending upon overflowCheck do not check pointer arithmetic.
*)

PROCEDURE MixTypesBinary (left, right: CARDINAL;
                          tokpos: CARDINAL; overflowCheck: BOOLEAN) : CARDINAL ;
BEGIN
   IF (NOT overflowCheck) AND
      (IsPointer (GetTypeMode (left)) OR IsPointer (GetTypeMode (right)))
   THEN
      RETURN Address
   ELSE
      RETURN MixTypesDecl (left, right, FindType (left), FindType (right), tokpos)
   END
END MixTypesBinary ;


(*
   CodeBinary - encode a binary arithmetic operation.
*)

PROCEDURE CodeBinary (binop: BuildBinProcedure; quad: CARDINAL) ;
VAR
   op      : QuadOperator ;
   op1, op2,
   op3     : CARDINAL ;
   op1pos,
   op2pos,
   op3pos,
   type    : CARDINAL ;
   tv,
   tl, tr  : tree ;
   location: location_t ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   GetQuadtok (quad, op, op1, op2, op3, op1pos, op2pos, op3pos) ;
   DeclareConstant (op3pos, op3) ;
   DeclareConstant (op2pos, op2) ;
   location := TokenToLocation (op1pos) ;

   type := MixTypesBinary (op2, op3, op1pos, MustCheckOverflow (quad)) ;
   ConvertBinaryOperands (location, tl, tr, type, op2, op3) ;

   tv := binop (location, tl, tr, FALSE) ;
   CheckOrResetOverflow (op1pos, tv, MustCheckOverflow(quad)) ;
   IF IsConst (op1)
   THEN
      (* still have a constant which was not resolved, pass it to gcc *)
      Assert(MixTypes(FindType(op3), FindType(op2), op1pos)#NulSym) ;

      PutConst (op1, MixTypes (FindType (op3), FindType (op2), op1pos)) ;
      ConstantKnownAndUsed (op1, DeclareKnownConstant (location, Mod2Gcc(GetType(op3)), tv))
   ELSE
      IF EnableSSA AND IsVariableSSA (op1)
      THEN
         Replace (op1, tv)
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (op1), tv)
      END
   END
END CodeBinary ;


(*
   NoWalkProcedure -
*)

PROCEDURE NoWalkProcedure (param: CARDINAL <* unused *>) ;
BEGIN
END NoWalkProcedure ;


(*
   CheckBinaryExpressionTypes - returns TRUE if all expression checks pass.
                                If the expression check fails quad is removed,
                                the walk procedure (des) is called and NoChange is
                                set to FALSE.
*)

PROCEDURE CheckBinaryExpressionTypes (quad: CARDINAL; p: WalkAction) : BOOLEAN ;
VAR
   lefttype,
   righttype,
   des, left, right: CARDINAL ;
   typeChecking,
   constExpr,
   overflowChecking: BOOLEAN ;
   despos, leftpos,
   rightpos,
   operatorpos,
   subexprpos      : CARDINAL ;
   op              : QuadOperator ;
BEGIN
   GetQuadOTypetok (quad, operatorpos, op,
                    des, left, right,
                    overflowChecking, typeChecking, constExpr,
                    despos, leftpos, rightpos) ;
   IF typeChecking AND (op # LogicalRotateOp) AND (op # LogicalShiftOp)
   THEN
      subexprpos := MakeVirtualTok (operatorpos, leftpos, rightpos) ;
      lefttype := GetType (left) ;
      righttype := GetType (right) ;
      IF StrictTypeChecking AND
         (NOT ExpressionTypeCompatible (subexprpos, "", lefttype, righttype,
                                        StrictTypeChecking, FALSE))
      THEN
         MetaErrorT2 (subexprpos,
                      'expression mismatch between {%1Etad} and {%2tad}',
                      left, right) ;
         NoChange := FALSE ;
         SubQuad (quad) ;
         p (des) ;
         RETURN FALSE
      END ;
      (* --fixme-- the ExpressionTypeCompatible above should be enough
         and the code below can be removed once ExpressionTypeCompatible
         is bug free.  *)
      IF NOT IsExpressionCompatible (lefttype, righttype)
      THEN
         ErrorMessageDecl (subexprpos,
                           'expression mismatch between {%1Etad} and {%2tad}',
                           left, right, TRUE) ;
         NoChange := FALSE ;
         SubQuad (quad) ;
         p (des) ;
         RETURN FALSE
      END
   END ;
   RETURN TRUE
END CheckBinaryExpressionTypes ;


(*
   CheckElementSetTypes - returns TRUE if all expression checks pass.
                          If the expression check fails quad is removed,
                          the walk procedure (des) is called and NoChange is
                          set to FALSE.
*)

PROCEDURE CheckElementSetTypes (quad: CARDINAL) : BOOLEAN ;
VAR
   lefttype,
   righttype,
   ignore, left, right: CARDINAL ;
   constExpr,
   overflowChecking: BOOLEAN ;
   ignorepos,
   leftpos,
   rightpos,
   operatorpos,
   subexprpos      : CARDINAL ;
   op              : QuadOperator ;
BEGIN
   GetQuadOtok (quad, operatorpos, op,
                left, right, ignore,
                overflowChecking, constExpr,
                leftpos, rightpos, ignorepos) ;
   subexprpos := MakeVirtualTok (operatorpos, leftpos, rightpos) ;
   lefttype := GetType (left) ;
   righttype := GetType (right) ;
   (* --fixme-- the ExpressionTypeCompatible below does not always catch
      type errors, it needs to be fixed and then some of the subsequent tests
      can be removed (and/or this procedure function rewritten).  *)
   IF StrictTypeChecking AND
      (NOT ExpressionTypeCompatible (subexprpos, "", lefttype, righttype,
                                     StrictTypeChecking, TRUE))
   THEN
      MetaErrorT2 (subexprpos,
                   'the types used in expression {%1Etad} {%kIN} {%2tad} are incompatible',
                   left, right) ;
      NoChange := FALSE ;
      SubQuad (quad) ;
      RETURN FALSE
   END ;
   IF (righttype = NulSym) OR (NOT IsSet (SkipType (righttype)))
   THEN
      MetaErrorT1 (rightpos,
                   'an {%kIN} expression is expecting {%1Etad} to be a {%kSET} type',
                   right) ;
      NoChange := FALSE ;
      SubQuad (quad) ;
      RETURN FALSE
   END ;
   righttype := GetType (SkipType (righttype)) ;
   (* Now fall though and compare the set element left against the type of set righttype.  *)
   IF NOT IsExpressionCompatible (lefttype, righttype)
   THEN
      ErrorMessageDecl (subexprpos,
                        'the types used in expression {%1Etad} {%kIN} {%2tad} are incompatible',
                        left, right, TRUE) ;
      NoChange := FALSE ;
      SubQuad (quad) ;
      RETURN FALSE
   END ;
   RETURN TRUE
END CheckElementSetTypes ;


(*
   CodeBinarySet - encode a binary set arithmetic operation.
                   Set operands may be longer than a word.
*)

PROCEDURE CodeBinarySet (binop: BuildBinProcedure; doOp: DoProcedure;
                         quad: CARDINAL) ;
VAR
   location        : location_t ;
   constExpr,
   overflowChecking: BOOLEAN ;
   op              : QuadOperator ;
   virttoken,
   virtexpr,
   des,
   left,
   right,
   despos,
   leftpos,
   rightpos,
   operatorpos     : CARDINAL ;
BEGIN
   GetQuadOtok (quad, operatorpos, op, des, left, right,
                overflowChecking, constExpr,
                despos, leftpos, rightpos) ;

   (* Firstly ensure that constant literals are declared.  *)
   DeclareConstant (rightpos, right) ;
   DeclareConstant (leftpos, left) ;
   DeclareConstructor (rightpos, quad, right) ;
   DeclareConstructor (leftpos, quad, left) ;

   virttoken := MakeVirtualTok (operatorpos, despos, rightpos) ;
   location := TokenToLocation (virttoken) ;
   IF CheckBinaryExpressionTypes (quad, NoWalkProcedure)
   THEN
      IF IsConst (des)
      THEN
         virtexpr := MakeVirtualTok (operatorpos, leftpos, rightpos) ;
         IF IsValueSolved (left) AND IsValueSolved (right)
         THEN
            Assert (MixTypes (FindType (right), FindType (left), virtexpr) # NulSym) ;
            PutConst (des, FindType (right)) ;
            PushValue (left) ;
            PushValue (right) ;
            doOp (virttoken) ;
            PopValue (des) ;
            PutConstSet (des)
         ELSE
            MetaErrorT0 (virtexpr, '{%E}constant expression cannot be evaluated')
         END
      ELSE
         checkDeclare (des) ;
         BuildBinaryForeachWordDo (location,
                                   Mod2Gcc (SkipType (GetType (des))),
                                   Mod2Gcc (des), Mod2Gcc (left), Mod2Gcc (right), binop,
                                   GetMode (des) = LeftValue,
                                   GetMode (left) = LeftValue,
                                   GetMode (right) = LeftValue,
                                   IsConst (des),
                                   IsConst (left),
                                   IsConst (right))
      END
   END
END CodeBinarySet ;


(*
   CheckUnaryOperand - checks to see whether operand is using a generic type.
*)

PROCEDURE CheckUnaryOperand (quad: CARDINAL; operand: CARDINAL) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   s, op : String ;
BEGIN
   type := SkipType (GetType (operand)) ;
   IF (Word=type) OR IsWordN (type) OR (Byte=type) OR (Loc=type)
   THEN
      op := GetM2OperatorDesc (GetQuadOp (quad)) ;
      s := InitString ('operand of type {%1Ets} is not allowed in an unary expression') ;
      IF op # NIL
      THEN
         s := ConCatChar (s, ' ') ;
         s := ConCat (s, Mark (op))
      END ;
      MetaErrorStringT1 (CurrentQuadToken, s, operand) ;
      RETURN FALSE
   END ;
   RETURN TRUE
END CheckUnaryOperand ;


(*
   UnaryOperand - returns TRUE if operand is acceptable for
                  unary operator: + -.  If FALSE
                  is returned, an error message will be generated
                  and the quad is deleted.
*)

PROCEDURE UnaryOperand (quad: CARDINAL; operand: CARDINAL) : BOOLEAN ;
BEGIN
   IF NOT CheckUnaryOperand (quad, operand)
   THEN
      SubQuad (quad) ;  (* We do not want multiple copies of the same error.  *)
      RETURN FALSE
   END ;
   RETURN TRUE
END UnaryOperand ;


(*
   CheckBinaryOperand - checks to see whether operand is using a generic type.
*)

PROCEDURE CheckBinaryOperand (quad: CARDINAL; isleft: BOOLEAN;
                              operand: CARDINAL; result: BOOLEAN) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   qop   : QuadOperator ;
   op1,
   op2,
   op3,
   op1pos,
   op2pos,
   op3pos: CARDINAL ;
   s, op : String ;
BEGIN
   type := SkipType (GetType (operand)) ;
   IF (Word=type) OR IsWordN (type) OR (Byte=type) OR (Loc=type)
   THEN
      GetQuadtok (quad, qop, op1, op2, op3,
                  op1pos, op2pos, op3pos) ;
         op := GetM2OperatorDesc (GetQuadOp (quad)) ;
      IF isleft
      THEN
         s := InitString ('left operand {%1Ea} of type {%1Ets} is not allowed in binary expression')
      ELSE
         s := InitString ('right operand {%1Ea} of type {%1Ets} is not allowed in binary expression')
      END ;
      IF op # NIL
      THEN
         s := ConCatChar (s, ' ') ;
         s := ConCat (s, Mark (op))
      END ;
      MetaErrorStringT1 (op1pos, s, operand) ;
      RETURN FALSE
   END ;
   RETURN result
END CheckBinaryOperand ;


(*
   BinaryOperands - returns TRUE if, l, and, r, are acceptable for
                    binary operator: + - / * and friends.  If FALSE
                    is returned, an error message will be generated
                    and the, quad, is deleted.
*)

PROCEDURE BinaryOperands (quad: CARDINAL; l, r: CARDINAL) : BOOLEAN ;
VAR
   result: BOOLEAN ;
BEGIN
   result := CheckBinaryOperand (quad, TRUE, l, TRUE) ;
   result := CheckBinaryOperand (quad, FALSE, r, result) ;
   IF NOT result
   THEN
      SubQuad (quad)   (* We do not want multiple copies of the same error.  *)
   END ;
   RETURN result
END BinaryOperands ;


(*
   IsConstStr - returns TRUE if sym is a constant string or a char constant.
*)

PROCEDURE IsConstStr (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsConstString (sym) OR (IsConst (sym) AND (GetSType (sym) = Char))
END IsConstStr ;


(*
   IsConstStrKnown - returns TRUE if sym is a constant string or a char constant
                     which is known.
*)

PROCEDURE IsConstStrKnown (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (IsConstString (sym) AND IsConstStringKnown (sym)) OR
          (IsConst (sym) AND (GetSType (sym) = Char))
END IsConstStrKnown ;


(*
   GetStr - return a string containing a constant string value associated with sym.
            A nul char constant will return an empty string.
*)

PROCEDURE GetStr (tokenno: CARDINAL; sym: CARDINAL) : String ;
VAR
   ch: CHAR ;
BEGIN
   Assert (IsConst (sym)) ;
   IF IsConstString (sym)
   THEN
      RETURN InitStringCharStar (KeyToCharStar (GetString (sym)))
   ELSE
      Assert (GetSType (sym) = Char) ;
      PushValue (sym) ;
      ch := PopChar (tokenno) ;
      RETURN InitStringChar (ch)
   END
END GetStr ;


(*
   FoldAdd - check addition for constant folding.  It checks for conststrings
             overloading the +.
*)

PROCEDURE FoldAdd (tokenno: CARDINAL; p: WalkAction;
                   quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   s: String ;
BEGIN
   IF IsConstStr (op2) AND IsConstStr (op3)
   THEN
      IF IsConstStrKnown (op2) AND IsConstStrKnown (op3)
      THEN
         (* Handle special addition for constant strings.  *)
         s := Dup (GetStr (tokenno, op2)) ;
         s := ConCat (s, GetStr (tokenno, op3)) ;
         PutConstStringKnown (tokenno, op1, makekey (string (s)), FALSE, TRUE) ;
         TryDeclareConstant (tokenno, op1) ;
         p (op1) ;
         NoChange := FALSE ;
         SubQuad (quad) ;
         s := KillString (s)
      END
   ELSE
      FoldArithAdd (tokenno, p, quad, op1, op2, op3)
   END
END FoldAdd ;


(*
   FoldArithAdd - check arithmetic addition for constant folding.
*)

PROCEDURE FoldArithAdd (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      FoldBinary (tokenno, p, BuildAdd, quad, op1, op2, op3)
   END
END FoldArithAdd ;


(*
   CodeAddChecked - code an addition instruction, determine whether checking
                    is required.
*)

PROCEDURE CodeAddChecked (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF MustCheckOverflow (quad)
   THEN
      CodeAddCheck (quad, left, right)
   ELSE
      CodeAdd (quad, left, right)
   END
END CodeAddChecked ;


(*
   CodeAddCheck - encode addition but check for overflow.
*)

PROCEDURE CodeAddCheck (quad, left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinaryCheck (BuildAddCheck, quad)
   END
END CodeAddCheck ;


(*
   CodeAdd - encode addition.
*)

PROCEDURE CodeAdd (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinary (BuildAdd, quad)
   END
END CodeAdd ;


(*
   FoldSub - check subtraction for constant folding.
*)

PROCEDURE FoldSub (tokenno: CARDINAL; p: WalkAction;
                   quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      FoldBinary(tokenno, p, BuildSub, quad, op1, op2, op3)
   END
END FoldSub ;


(*
   CodeSubChecked - code a subtract instruction, determine whether checking
                    is required.
*)

PROCEDURE CodeSubChecked (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF MustCheckOverflow (quad)
   THEN
      CodeSubCheck (quad, left, right)
   ELSE
      CodeSub (quad, left, right)
   END
END CodeSubChecked ;


(*
   CodeSubCheck - encode subtraction but check for overflow.
*)

PROCEDURE CodeSubCheck (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinaryCheck (BuildSubCheck, quad)
   END
END CodeSubCheck ;


(*
   CodeSub - encode subtraction.
*)

PROCEDURE CodeSub (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinary (BuildSub, quad)
   END
END CodeSub ;


(*
   FoldMult - check multiplication for constant folding.
*)

PROCEDURE FoldMult (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      FoldBinary(tokenno, p, BuildMult, quad, op1, op2, op3)
   END
END FoldMult ;


(*
   CodeMultChecked - code a multiplication instruction, determine whether checking
                     is required.
*)

PROCEDURE CodeMultChecked (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF MustCheckOverflow (quad)
   THEN
      CodeMultCheck (quad, left, right)
   ELSE
      CodeMult (quad, left, right)
   END
END CodeMultChecked ;


(*
   CodeMultCheck - encode multiplication but check for overflow.
*)

PROCEDURE CodeMultCheck (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinaryCheck (BuildMultCheck, quad)
   END
END CodeMultCheck ;


(*
   CodeMult - encode multiplication.
*)

PROCEDURE CodeMult (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinary (BuildMult, quad)
   END
END CodeMult ;


(*
   CodeDivM2Checked - code a divide instruction, determine whether checking
                      is required.
*)

PROCEDURE CodeDivM2Checked (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF MustCheckOverflow (quad)
   THEN
      CodeDivM2Check (quad, left, right)
   ELSE
      CodeDivM2 (quad, left, right)
   END
END CodeDivM2Checked ;


(*
   CodeDivM2Check - encode addition but check for overflow.
*)

PROCEDURE CodeDivM2Check (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinaryCheck (BuildDivM2Check, quad)
   END
END CodeDivM2Check ;


(*
   CodeModM2Checked - code a modulus instruction, determine whether checking
                      is required.
*)

PROCEDURE CodeModM2Checked (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF MustCheckOverflow (quad)
   THEN
      CodeModM2Check (quad, left, right)
   ELSE
      CodeModM2 (quad, left, right)
   END
END CodeModM2Checked ;


(*
   CodeModM2Check - encode addition but check for overflow.
*)

PROCEDURE CodeModM2Check (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinaryCheck (BuildModM2Check, quad)
   END
END CodeModM2Check ;


(*
   BinaryOperandRealFamily -
*)

PROCEDURE BinaryOperandRealFamily (op: CARDINAL) : BOOLEAN ;
VAR
   t: CARDINAL ;
BEGIN
   t := SkipType(GetType(op)) ;
   RETURN( IsComplexType(t) OR IsComplexN(t) OR
           IsRealType(t) OR IsRealN(t) )
END BinaryOperandRealFamily ;


(*
   FoldDivM2 - check division for constant folding.
*)

PROCEDURE FoldDivM2 (tokenno: CARDINAL; p: WalkAction;
                     quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      IF BinaryOperandRealFamily(op2) OR BinaryOperandRealFamily(op3)
      THEN
         FoldBinary(tokenno, p, BuildRDiv, quad, op1, op2, op3)
      ELSE
         FoldBinary(tokenno, p, BuildDivM2, quad, op1, op2, op3)
      END
   END
END FoldDivM2 ;


(*
   CodeDivM2 - encode division.
*)

PROCEDURE CodeDivM2 (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      IF BinaryOperandRealFamily (left) OR BinaryOperandRealFamily (right)
      THEN
         CodeBinary (BuildRDiv, quad)
      ELSE
         CodeBinary (BuildDivM2, quad)
      END
   END
END CodeDivM2 ;


(*
   FoldModM2 - check modulus for constant folding.
*)

PROCEDURE FoldModM2 (tokenno: CARDINAL; p: WalkAction;
                     quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      FoldBinary(tokenno, p, BuildModM2, quad, op1, op2, op3)
   END
END FoldModM2 ;


(*
   CodeModM2 - encode modulus.
*)

PROCEDURE CodeModM2 (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinary (BuildModM2, quad)
   END
END CodeModM2 ;


(*
   FoldDivTrunc - check division for constant folding.
*)

PROCEDURE FoldDivTrunc (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      IF BinaryOperandRealFamily(op2) OR BinaryOperandRealFamily(op3)
      THEN
         FoldBinary(tokenno, p, BuildRDiv, quad, op1, op2, op3)
      ELSE
         FoldBinary(tokenno, p, BuildDivTrunc, quad, op1, op2, op3)
      END
   END
END FoldDivTrunc ;


(*
   CodeDivTrunc - encode multiplication.
*)

PROCEDURE CodeDivTrunc (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      IF BinaryOperandRealFamily (left) OR BinaryOperandRealFamily (right)
      THEN
         CodeBinary (BuildRDiv, quad)
      ELSE
         CodeBinary (BuildDivTrunc, quad)
      END
   END
END CodeDivTrunc ;


(*
   FoldModTrunc - check modulus for constant folding.
*)

PROCEDURE FoldModTrunc (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      FoldBinary(tokenno, p, BuildModTrunc, quad, op1, op2, op3)
   END
END FoldModTrunc ;


(*
   CodeModTrunc - encode modulus.
*)

PROCEDURE CodeModTrunc (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinary (BuildModTrunc, quad)
   END
END CodeModTrunc ;


(*
   FoldDivCeil - check division for constant folding.
*)

PROCEDURE FoldDivCeil (tokenno: CARDINAL; p: WalkAction;
                       quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      IF BinaryOperandRealFamily(op2) OR BinaryOperandRealFamily(op3)
      THEN
         FoldBinary(tokenno, p, BuildRDiv, quad, op1, op2, op3)
      ELSE
         FoldBinary(tokenno, p, BuildDivCeil, quad, op1, op2, op3)
      END
   END
END FoldDivCeil ;


(*
   CodeDivCeil - encode multiplication.
*)

PROCEDURE CodeDivCeil (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      IF BinaryOperandRealFamily (left) OR BinaryOperandRealFamily (right)
      THEN
         CodeBinary (BuildRDiv, quad)
      ELSE
         CodeBinary (BuildDivCeil, quad)
      END
   END
END CodeDivCeil ;


(*
   FoldModCeil - check modulus for constant folding.
*)

PROCEDURE FoldModCeil (tokenno: CARDINAL; p: WalkAction;
                       quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      FoldBinary(tokenno, p, BuildModCeil, quad, op1, op2, op3)
   END
END FoldModCeil ;


(*
   CodeModCeil - encode multiplication.
*)

PROCEDURE CodeModCeil (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinary (BuildModCeil, quad)
   END
END CodeModCeil ;


(*
   FoldDivFloor - check division for constant folding.
*)

PROCEDURE FoldDivFloor (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      IF BinaryOperandRealFamily(op2) OR BinaryOperandRealFamily(op3)
      THEN
         FoldBinary(tokenno, p, BuildRDiv, quad, op1, op2, op3)
      ELSE
         FoldBinary(tokenno, p, BuildDivFloor, quad, op1, op2, op3)
      END
   END
END FoldDivFloor ;


(*
   CodeDivFloor - encode multiplication.
*)

PROCEDURE CodeDivFloor (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      IF BinaryOperandRealFamily (left) OR BinaryOperandRealFamily (right)
      THEN
         CodeBinary (BuildRDiv, quad)
      ELSE
         CodeBinary (BuildDivFloor, quad)
      END
   END
END CodeDivFloor ;


(*
   FoldModFloor - check modulus for constant folding.
*)

PROCEDURE FoldModFloor (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, op2, op3)
   THEN
      FoldBinary(tokenno, p, BuildModFloor, quad, op1, op2, op3)
   END
END FoldModFloor ;


(*
   CodeModFloor - encode modulus.
*)

PROCEDURE CodeModFloor (quad: CARDINAL; left, right: CARDINAL) ;
BEGIN
   IF BinaryOperands (quad, left, right)
   THEN
      CodeBinary (BuildModFloor, quad)
   END
END CodeModFloor ;


(*
   FoldBuiltinConst -
*)

PROCEDURE FoldBuiltinConst (tokenno: CARDINAL; p: WalkAction;
                            quad: CARDINAL; result, constDesc: CARDINAL) ;
VAR
   value: tree ;
BEGIN
   value := GetBuiltinConst (KeyToCharStar (Name (constDesc))) ;
   IF value = NIL
   THEN
      MetaErrorT1 (tokenno, 'unknown built in constant {%1Ead}', constDesc)
   ELSE
      AddModGcc (result, value) ;
      p (result) ;
      NoChange := FALSE ;
      SubQuad (quad)
   END
END FoldBuiltinConst ;


(*
   FoldBuiltinTypeInfo - attempts to fold a builtin attribute value on type op2.
*)

PROCEDURE FoldBuiltinTypeInfo (tokenno: CARDINAL; p: WalkAction;
                               quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t       : tree ;
   location: location_t ;
BEGIN
   IF GccKnowsAbout(op2) AND CompletelyResolved(op2)
   THEN
      location := TokenToLocation(tokenno) ;
      t := GetBuiltinTypeInfo(location, Mod2Gcc(op2), KeyToCharStar(Name(op3))) ;
      IF t=NIL
      THEN
         MetaErrorT2 (tokenno, 'unknown built in constant {%1Ead} attribute for type {%2ad}', op3, op2)
      ELSE
         AddModGcc(op1, t) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   END
END FoldBuiltinTypeInfo ;


(*
   FoldStandardFunction - attempts to fold a standard function.
*)

PROCEDURE FoldStandardFunction (tokenno: CARDINAL; p: WalkAction;
                                quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   s       : String ;
   type,
   d,
   result  : CARDINAL ;
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   IF GetSymName(op2)=MakeKey('Length')
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            IF IsConstString(op3)
            THEN
               AddModGcc(op1, FindSize(tokenno, op3)) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            ELSE
               MetaErrorT1 (tokenno, 'parameter to LENGTH must be a string {%1Ead}', op3)
            END
         ELSE
            (* rewrite the quad to use becomes.  *)
            d := GetStringLength (tokenno, op3) ;
            s := Sprintf1 (Mark (InitString ("%d")), d) ;
            result := MakeConstLit (tokenno, makekey (string (s)), Cardinal) ;
            s := KillString (s) ;
            TryDeclareConstant (tokenno, result) ;
            PutQuad (quad, BecomesOp, op1, NulSym, result)
         END
      END
   ELSIF GetSymName(op2)=MakeKey('CAP')
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            IF (IsConstString(op3) AND (GetStringLength (tokenno, op3) = 1)) OR
               (GetType(op3)=Char)
            THEN
               AddModGcc(op1, BuildCap(location, Mod2Gcc(op3))) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            ELSE
               MetaErrorT1 (tokenno, 'parameter to CAP must be a single character {%1Ead}', op3)
            END
         END
      END
   ELSIF GetSymName(op2)=MakeKey('ABS')
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            AddModGcc(op1, BuildAbs(location, Mod2Gcc(op3))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   ELSIF op2=Im
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            AddModGcc(op1, BuildIm(Mod2Gcc(op3))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   ELSIF op2=Re
   THEN
      TryDeclareConstant(tokenno, op3) ;
      IF IsConst(op3) AND GccKnowsAbout(op3)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            AddModGcc(op1, BuildRe(Mod2Gcc(op3))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   ELSIF op2=Cmplx
   THEN
      TryDeclareConstant(tokenno, GetNth(op3, 1)) ;
      TryDeclareConstant(tokenno, GetNth(op3, 2)) ;
      IF IsConst(GetNth(op3, 1)) AND GccKnowsAbout(GetNth(op3, 1)) AND
         IsConst(GetNth(op3, 2)) AND GccKnowsAbout(GetNth(op3, 2))
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst(op1)
         THEN
            type := GetCmplxReturnType(GetType(GetNth(op3, 1)), GetType(GetNth(op3, 2))) ;
            IF type=NulSym
            THEN
               MetaErrorT2 (tokenno, 'real {%1Eatd} and imaginary {%2atd} types are incompatible',
                            GetNth(op3, 1), GetNth(op3, 2))
            ELSE
               AddModGcc(op1, BuildCmplx(location,
                                         Mod2Gcc(type),
                                         Mod2Gcc(GetNth(op3, 1)),
                                         Mod2Gcc(GetNth(op3, 2)))) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            END
         END
      END
   ELSIF op2=TBitSize
   THEN
      IF GccKnowsAbout(op3)
      THEN
         AddModGcc(op1, BuildTBitSize(location, Mod2Gcc(op3))) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   ELSE
      InternalError ('only expecting LENGTH, CAP, ABS, IM, RE')
   END
END FoldStandardFunction ;


(*
   CodeStandardFunction -
*)

PROCEDURE CodeStandardFunction (quad: CARDINAL; result, function, param: CARDINAL) ;
VAR
   type    : CARDINAL ;
   location: location_t ;
BEGIN
   DeclareConstant (CurrentQuadToken, param) ;
   DeclareConstructor (CurrentQuadToken, quad, param) ;
   location := TokenToLocation (CurrentQuadToken) ;

   IF (function # NulSym) AND (GetSymName (function) = MakeKey ('Length'))
   THEN
      IF IsConst (result)
      THEN
         InternalError ('LENGTH function should already have been folded')
      END
   ELSIF (function # NulSym) AND (GetSymName (function) = MakeKey ('CAP'))
   THEN
      IF IsConst (result)
      THEN
         InternalError ('CAP function should already have been folded')
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (result), BuildCap (location, Mod2Gcc (param)))
      END
   ELSIF (function # NulSym) AND (GetSymName (function) = MakeKey('ABS'))
   THEN
      IF IsConst (result)
      THEN
         InternalError ('ABS function should already have been folded')
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (result), BuildAbs (location, Mod2Gcc (param)))
      END
   ELSIF function = Im
   THEN
      IF IsConst (result)
      THEN
         InternalError ('IM function should already have been folded')
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (result), BuildIm (Mod2Gcc (param)))
      END
   ELSIF function = Re
   THEN
      IF IsConst (result)
      THEN
         InternalError ('RE function should already have been folded')
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (result), BuildRe (Mod2Gcc (param)))
      END
   ELSIF function = Cmplx
   THEN
      IF IsConst (result)
      THEN
         InternalError ('CMPLX function should already have been folded')
      ELSE
         type := GetCmplxReturnType (GetType (GetNth (param, 1)), GetType (GetNth (param, 2))) ;
         IF type = NulSym
         THEN
            MetaErrorT2 (CurrentQuadToken,
                         'real {%1Eatd} and imaginary {%2atd} types are incompatible',
                         GetNth (param, 1), GetNth (param, 2))
         ELSE
            BuildAssignmentStatement (location, Mod2Gcc (result), BuildCmplx(location,
                                                                             Mod2Gcc (type),
                                                                             Mod2Gcc (GetNth (param, 1)),
                                                                             Mod2Gcc (GetNth (param, 2))))
         END
      END
   ELSIF function = TBitSize
   THEN
      IF IsConst (result)
      THEN
         InternalError ('TBITSIZE function should already have been folded')
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (result), BuildTBitSize (location, Mod2Gcc (param)))
      END
   ELSE
      InternalError ('expecting LENGTH, CAP, ABS, IM')
   END
END CodeStandardFunction ;


(*
   CodeSavePriority - checks to see whether op2 is reachable and is directly accessible
                      externally. If so then it saves the current interrupt priority
                      in op1 and sets the current priority to that determined by
                      appropriate module.

                      op1 := op3(GetModuleScope(op2))
*)

PROCEDURE CodeSavePriority (oldValue, scopeSym, procedureSym: CARDINAL) ;
VAR
   funcTree: tree ;
   mod     : CARDINAL ;
   n       : Name ;
   location: location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsModule (scopeSym) OR IsDefImp (scopeSym) OR
      (IsProcedure (scopeSym) AND GetNeedSavePriority (scopeSym))
   THEN
      IF IsProcedure (scopeSym)
      THEN
         mod := GetModuleScope (scopeSym) ;
      ELSE
         Assert (IsModule(scopeSym) OR IsDefImp (scopeSym)) ;
         mod := scopeSym
      END ;
      IF GetPriority (mod) # NulSym
      THEN
         IF PriorityDebugging
         THEN
            n := GetSymName (scopeSym) ;
            printf1 ('procedure <%a> needs to save interrupts\n', n)
         END ;
         DeclareConstant (CurrentQuadToken, GetPriority (mod)) ;
         BuildParam (location, Mod2Gcc (GetPriority (mod))) ;
         funcTree := BuildProcedureCallTree (location, Mod2Gcc (procedureSym), Mod2Gcc (GetType (procedureSym))) ;
         funcTree := BuildFunctValue (location, Mod2Gcc (oldValue)) ;
         AddStatement (location, funcTree)
      END
   END
END CodeSavePriority ;


(*
   CodeRestorePriority - checks to see whether op2 is reachable and is directly accessible
                         externally. If so then it restores the previous interrupt priority
                         held in op1.

                         op1 := op3(op1)
*)

PROCEDURE CodeRestorePriority (oldValue, scopeSym, procedureSym: CARDINAL) ;
VAR
   funcTree: tree ;
   mod     : CARDINAL ;
   n       : Name ;
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;

   IF IsModule (scopeSym) OR IsDefImp (scopeSym) OR
      (IsProcedure (scopeSym) AND GetNeedSavePriority (scopeSym))
   THEN
      IF IsProcedure (scopeSym)
      THEN
         mod := GetModuleScope (scopeSym) ;
      ELSE
         Assert (IsModule (scopeSym) OR IsDefImp (scopeSym)) ;
         mod := scopeSym
      END ;
      IF GetPriority (mod) # NulSym
      THEN
         IF PriorityDebugging
         THEN
            n := GetSymName (scopeSym) ;
            printf1 ('procedure <%a> needs to restore interrupts\n', n)
         END ;
         BuildParam (location, Mod2Gcc (oldValue)) ;
         funcTree := BuildProcedureCallTree (location, Mod2Gcc (procedureSym), Mod2Gcc (GetType (procedureSym))) ;
         funcTree := BuildFunctValue (location, Mod2Gcc (oldValue)) ;
         AddStatement(location, funcTree)
      END
   END
END CodeRestorePriority ;


(*
   FoldBinarySet - attempts to fold set arithmetic it removes the quad if successful.
*)

PROCEDURE FoldBinarySet (tokenno: CARDINAL; p: WalkAction; op: DoProcedure;
                         quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   (* firstly try and ensure that constants are declared *)
   TryDeclareConstant(tokenno, op2) ;
   TryDeclareConstant(tokenno, op3) ;
   location := TokenToLocation(tokenno) ;

   IF GccKnowsAbout(op2) AND GccKnowsAbout(op3)
   THEN
      IF CheckBinaryExpressionTypes (quad, p)
      THEN
         IF IsConst(op2) AND IsConstSet(op2) AND
            IsConst(op3) AND IsConstSet(op3) AND
            IsConst(op1)
         THEN
            IF IsValueSolved(op2) AND IsValueSolved(op3)
            THEN
               Assert(MixTypes(FindType(op3), FindType(op2), tokenno)#NulSym) ;
               PutConst(op1, MixTypes(FindType(op3), FindType(op2), tokenno)) ;
               PushValue(op2) ;
               PushValue(op3) ;
               op(tokenno) ;
               PopValue(op1) ;
               PushValue(op1) ;
               PutConstSet(op1) ;
               AddModGcc(op1,
                         DeclareKnownConstant(location,
                                              Mod2Gcc(GetType(op3)),
                                              PopSetTree(tokenno))) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            END
         END
      END
   END
END FoldBinarySet ;


(*
   FoldSetOr - check whether we can fold a set arithmetic or.
*)

PROCEDURE FoldSetOr (tokenno: CARDINAL; p: WalkAction;
                     quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet (tokenno, p, SetOr, quad, op1, op2, op3)
END FoldSetOr ;


(*
   CodeSetOr - encode set arithmetic or.
*)

PROCEDURE CodeSetOr (quad: CARDINAL) ;
BEGIN
   CodeBinarySet (BuildLogicalOr, SetOr, quad)
END CodeSetOr ;


(*
   FoldSetAnd - check whether we can fold a logical and.
*)

PROCEDURE FoldSetAnd (tokenno: CARDINAL; p: WalkAction;
                      quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetAnd, quad, op1, op2, op3)
END FoldSetAnd ;


(*
   CodeSetAnd - encode set arithmetic and.
*)

PROCEDURE CodeSetAnd (quad: CARDINAL) ;
BEGIN
   CodeBinarySet (BuildLogicalAnd, SetAnd, quad)
END CodeSetAnd ;


(*
   CodeBinarySetShift - encode a binary set arithmetic operation.
                        The set maybe larger than a machine word
                        and the value of one word may effect the
                        values of another - ie shift and rotate.
                        Set sizes of a word or less are evaluated
                        with binop, whereas multiword sets are
                        evaluated by M2RTS.
*)

PROCEDURE CodeBinarySetShift (binop: BuildSetProcedure;
                              doOp : DoProcedure;
                              var, left, right: Name;
                              quad: CARDINAL;
                              op1, op2, op3: CARDINAL) ;
VAR
   nBits,
   unbounded,
   leftproc,
   rightproc,
   varproc  : tree ;
   location : location_t ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, op3) ;
   DeclareConstant(CurrentQuadToken, op2) ;
   DeclareConstructor(CurrentQuadToken, quad, op3) ;
   DeclareConstructor(CurrentQuadToken, quad, op2) ;
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsConst(op1)
   THEN
      IF IsValueSolved(op2) AND IsValueSolved(op3)
      THEN
         Assert(MixTypes(FindType(op3),
                         FindType(op2), CurrentQuadToken)#NulSym) ;
         PutConst(op1, FindType(op3)) ;
         PushValue(op2) ;
         PushValue(op3) ;
         doOp(CurrentQuadToken) ;
         PopValue(op1) ;
         PutConstSet(op1)
      ELSE
         MetaErrorT0 (CurrentQuadToken, '{%E}constant expression cannot be evaluated')
      END
   ELSE
      varproc := Mod2Gcc(FromModuleGetSym(CurrentQuadToken, var, System)) ;
      leftproc := Mod2Gcc(FromModuleGetSym(CurrentQuadToken, left, System)) ;
      rightproc := Mod2Gcc(FromModuleGetSym(CurrentQuadToken, right, System)) ;
      unbounded := Mod2Gcc(GetType(GetNthParamAny (FromModuleGetSym(CurrentQuadToken,
                                                                var, System), 1))) ;
      PushValue(GetTypeMax(SkipType(GetType(op1)))) ;
      PushIntegerTree(BuildConvert(location, GetM2ZType(), PopIntegerTree(), FALSE)) ;

      PushValue(GetTypeMin(SkipType(GetType(op1)))) ;
      PushIntegerTree(BuildConvert(location, GetM2ZType(), PopIntegerTree(), FALSE)) ;
      Sub ;
      PushCard(1) ;
      PushIntegerTree(BuildConvert(location, GetM2ZType(), PopIntegerTree(), FALSE)) ;
      Addn ;
      nBits := PopIntegerTree() ;
      BuildBinarySetDo(location,
                       Mod2Gcc(SkipType(GetType(op1))),
                       Mod2Gcc(op1),
                       Mod2Gcc(op2),
                       Mod2Gcc(op3),
                       binop,
                       GetMode(op1)=LeftValue,
                       GetMode(op2)=LeftValue,
                       GetMode(op3)=LeftValue,
                       nBits,
                       unbounded,
                       varproc, leftproc, rightproc)
   END
END CodeBinarySetShift ;


(*
   FoldSetShift - check whether we can fold a logical shift.
*)

PROCEDURE FoldSetShift (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetShift, quad, op1, op2, op3)
END FoldSetShift ;


(*
   CodeSetShift - encode set arithmetic shift.
*)

PROCEDURE CodeSetShift (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinarySetShift (BuildLogicalShift,
                       SetShift,
                       MakeKey('ShiftVal'),
                       MakeKey('ShiftLeft'),
                       MakeKey('ShiftRight'),
                       quad, op1, op2, op3)
END CodeSetShift ;


(*
   FoldSetRotate - check whether we can fold a logical rotate.
*)

PROCEDURE FoldSetRotate (tokenno: CARDINAL; p: WalkAction;
                        quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetRotate, quad, op1, op2, op3)
END FoldSetRotate ;


(*
   CodeSetRotate - encode set arithmetic rotate.
*)

PROCEDURE CodeSetRotate (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   CodeBinarySetShift (BuildLogicalRotate,
                       SetRotate,
                       MakeKey ('RotateVal'),
                       MakeKey ('RotateLeft'),
                       MakeKey ('RotateRight'),
                       quad, op1, op2, op3)
END CodeSetRotate ;


(*
   FoldSetLogicalDifference - check whether we can fold a logical difference.
*)

(*
PROCEDURE FoldSetLogicalDifference (tokenno: CARDINAL; p: WalkAction;
                                    quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet(tokenno, p, SetDifference, quad, op1, op2, op3)
END FoldSetLogicalDifference ;
*)


(*
   CodeSetLogicalDifference - encode set arithmetic logical difference.
*)

PROCEDURE CodeSetLogicalDifference (quad: CARDINAL) ;
BEGIN
   CodeBinarySet (BuildLogicalDifference, SetDifference, quad)
END CodeSetLogicalDifference ;


(*
   FoldSymmetricDifference - check whether we can fold a logical difference.
*)

PROCEDURE FoldSymmetricDifference (tokenno: CARDINAL; p: WalkAction;
                                   quad: CARDINAL; op1, op2, op3: CARDINAL) ;
BEGIN
   FoldBinarySet (tokenno, p, SetSymmetricDifference, quad, op1, op2, op3)
END FoldSymmetricDifference ;


(*
   CodeSetSymmetricDifference - code set difference.
*)

PROCEDURE CodeSetSymmetricDifference (quad: CARDINAL) ;
BEGIN
   CodeBinarySet (BuildSymmetricDifference, SetSymmetricDifference, quad)
END CodeSetSymmetricDifference ;


(*
   CodeUnarySet - encode a unary set arithmetic operation.
                  Set operands may be longer than a word.
*)

PROCEDURE CodeUnarySet (unop: BuildUnarySetFunction; constop: DoUnaryProcedure;
                        quad: CARDINAL; result, expr: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant (CurrentQuadToken, expr) ;
   DeclareConstructor (CurrentQuadToken, quad, expr) ;
   location := TokenToLocation (CurrentQuadToken) ;

   IF IsConst (result)
   THEN
      IF IsValueSolved (expr)
      THEN
         Assert (FindType (expr) # NulSym) ;
         PutConst (result, FindType (expr)) ;
         PushValue (expr) ;
         constop (CurrentQuadToken) ;
         PopValue (result) ;
         PushValue (result) ;
         PutConstSet (result) ;
         ConstantKnownAndUsed (result,
                               DeclareKnownConstant(location,
                                                    Mod2Gcc (GetType (expr)),
                                                    PopSetTree (CurrentQuadToken)))
      ELSE
         MetaErrorT0 (CurrentQuadToken,
                      '{%E}constant expression cannot be evaluated')
      END
   ELSE
      checkDeclare (result) ;
      BuildUnaryForeachWordDo (location,
                               Mod2Gcc (GetType (result)), Mod2Gcc (result), Mod2Gcc (expr), unop,
                               GetMode(result) = LeftValue, GetMode(expr) = LeftValue,
                               IsConst (result), IsConst (expr))
   END
END CodeUnarySet ;


(*
   FoldIncl - check whether we can fold the InclOp.
              result := result + (1 << expr)
*)

PROCEDURE FoldIncl (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; result, expr: CARDINAL) ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant (tokenno, expr) ;
   IF IsConst (result) AND IsConst (expr)
   THEN
      IF GccKnowsAbout (expr) AND IsValueSolved (result)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         PushValue (result) ;
         AddBit (tokenno, expr) ;
         AddModGcc (result, PopSetTree(tokenno)) ;
         p (result) ;
         NoChange := FALSE ;
         SubQuad (quad)
      END
   END
END FoldIncl ;


(*
   FoldIfLess - check to see if it is possible to evaluate
                if op1 < op2 then goto op3.
*)

PROCEDURE FoldIfLess (tokenno: CARDINAL;
                      quad: CARDINAL; left, right, destQuad: CARDINAL) ;
BEGIN
   (* Firstly ensure that constant literals are declared.  *)
   TryDeclareConstant(tokenno, left) ;
   TryDeclareConstant(tokenno, right) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      IF IsValueSolved (left) AND IsValueSolved (right)
      THEN
         (* We can take advantage of the known values and evaluate the condition.  *)
         PushValue (left) ;
         PushValue (right) ;
         IF Less (tokenno)
         THEN
            PutQuad (quad, GotoOp, NulSym, NulSym, destQuad)
         ELSE
            SubQuad (quad)
         END ;
         NoChange := FALSE
      END
   END
END FoldIfLess ;


(*
   FoldIfGre - check to see if it is possible to evaluate
               if op1 > op2 then goto op3.
*)

PROCEDURE FoldIfGre (tokenno: CARDINAL;
                     quad: CARDINAL;
                     left, right, destQuad: CARDINAL) ;
BEGIN
   (* Firstly ensure that constant literals are declared.  *)
   TryDeclareConstant(tokenno, left) ;
   TryDeclareConstant(tokenno, right) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      IF IsValueSolved (left) AND IsValueSolved (right)
      THEN
         (* We can take advantage of the known values and evaluate the condition.  *)
         PushValue (left) ;
         PushValue (right) ;
         IF Gre (tokenno)
         THEN
            PutQuad (quad, GotoOp, NulSym, NulSym, destQuad)
         ELSE
            SubQuad (quad)
         END ;
         NoChange := FALSE
      END
   END
END FoldIfGre ;


(*
   FoldIfLessEqu - check to see if it is possible to evaluate
                   if op1 <= op2 then goto op3.
*)

PROCEDURE FoldIfLessEqu (tokenno: CARDINAL;
                         quad: CARDINAL;
                         left, right, destQuad: CARDINAL) ;
BEGIN
   (* Firstly ensure that constant literals are declared.  *)
   TryDeclareConstant(tokenno, left) ;
   TryDeclareConstant(tokenno, right) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      IF IsValueSolved (left) AND IsValueSolved (right)
      THEN
         (* We can take advantage of the known values and evaluate the condition.  *)
         PushValue (left) ;
         PushValue (right) ;
         IF LessEqu (tokenno)
         THEN
            PutQuad (quad, GotoOp, NulSym, NulSym, destQuad)
         ELSE
            SubQuad (quad)
         END ;
         NoChange := FALSE
      END
   END
END FoldIfLessEqu ;


(*
   FoldIfGreEqu - check to see if it is possible to evaluate
                  if op1 >= op2 then goto op3.
*)

PROCEDURE FoldIfGreEqu (tokenno: CARDINAL;
                        quad: CARDINAL;
                        left, right, destQuad: CARDINAL) ;
BEGIN
   (* Firstly ensure that constant literals are declared.  *)
   TryDeclareConstant(tokenno, left) ;
   TryDeclareConstant(tokenno, right) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      IF IsValueSolved (left) AND IsValueSolved (right)
      THEN
         (* We can take advantage of the known values and evaluate the condition.  *)
         PushValue (left) ;
         PushValue (right) ;
         IF GreEqu (tokenno)
         THEN
            PutQuad (quad, GotoOp, NulSym, NulSym, destQuad)
         ELSE
            SubQuad (quad)
         END ;
         NoChange := FALSE
      END
   END
END FoldIfGreEqu ;


(*
   FoldIfIn - check whether we can fold the IfInOp
              if op1 in op2 then goto op3
*)

PROCEDURE FoldIfIn (tokenno: CARDINAL;
                    quad: CARDINAL;
                    left, right, destQuad: CARDINAL) ;
BEGIN
   (* Firstly ensure that constant literals are declared.  *)
   TryDeclareConstant (tokenno, left) ;
   TryDeclareConstant (tokenno, right) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      IF IsValueSolved (left) AND IsValueSolved (right)
      THEN
         IF CheckBinaryExpressionTypes (quad, NoWalkProcedure)
         THEN
            (* We can take advantage of the known values and evaluate the condition.  *)
            PushValue (right) ;
            IF SetIn (tokenno, left)
            THEN
               PutQuad (quad, GotoOp, NulSym, NulSym, destQuad) ;
            ELSE
               SubQuad (quad)
            END
         ELSE
            SubQuad (quad)
         END ;
         NoChange := FALSE
      END
   END
END FoldIfIn ;


(*
   FoldIfNotIn - check whether we can fold the IfNotInOp
                 if not (op1 in op2) then goto op3
*)

PROCEDURE FoldIfNotIn (tokenno: CARDINAL;
                       quad: CARDINAL;
                       left, right, destQuad: CARDINAL) ;
BEGIN
   (* Firstly ensure that constant literals are declared.  *)
   TryDeclareConstant (tokenno, left) ;
   TryDeclareConstant (tokenno, right) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      IF IsValueSolved (left) AND IsValueSolved (right)
      THEN
         IF CheckBinaryExpressionTypes (quad, NoWalkProcedure)
         THEN
            (* We can take advantage of the known values and evaluate the
               condition.  *)
            PushValue (right) ;
            IF NOT SetIn (tokenno, left)
            THEN
               PutQuad (quad, GotoOp, NulSym, NulSym, destQuad)
            ELSE
               SubQuad (quad)
            END
         ELSE
            SubQuad (quad)
         END ;
         NoChange := FALSE
      END
   END
END FoldIfNotIn ;


(*
   FoldIfEqu - check to see if it is possible to evaluate
               if op1 = op2 then goto op3.
*)

PROCEDURE FoldIfEqu (tokenno: CARDINAL;
                     quad: CARDINAL;
                     left, right, destQuad: CARDINAL) ;
BEGIN
   (* Firstly ensure that constant literals are declared.  *)
   TryDeclareConstant(tokenno, left) ;
   TryDeclareConstant(tokenno, right) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      IF IsValueSolved (left) AND IsValueSolved (right)
      THEN
         (* We can take advantage of the known values and evaluate the
            condition.  *)
         PushValue (left) ;
         PushValue (right) ;
         IF Equ (tokenno)
         THEN
            PutQuad (quad, GotoOp, NulSym, NulSym, destQuad)
         ELSE
            SubQuad (quad)
         END ;
         NoChange := FALSE
      END
   END
END FoldIfEqu ;


(*
   FoldIfNotEqu - check to see if it is possible to evaluate
                  if op1 # op2 then goto op3.
*)

PROCEDURE FoldIfNotEqu (tokenno: CARDINAL;
                        quad: CARDINAL;
                        left, right, destQuad: CARDINAL) ;
BEGIN
   (* Firstly ensure that constant literals are declared.  *)
   TryDeclareConstant(tokenno, left) ;
   TryDeclareConstant(tokenno, right) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      IF IsValueSolved (left) AND IsValueSolved (right)
      THEN
         (* We can take advantage of the known values and evaluate the
            condition.  *)
         PushValue (left) ;
         PushValue (right) ;
         IF NotEqu (tokenno)
         THEN
            PutQuad (quad, GotoOp, NulSym, NulSym, destQuad)
         ELSE
            SubQuad (quad)
         END ;
         NoChange := FALSE
      END
   END
END FoldIfNotEqu ;


(*
   GetSetLimits - assigns low and high to the limits of the declared, set.
*)

PROCEDURE GetSetLimits (set: CARDINAL; VAR low, high: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetType(set) ;
   IF IsSubrange(type)
   THEN
      GetSubrange(type, high, low) ;
   ELSE
      low := GetTypeMin(type) ;
      high := GetTypeMax(type)
   END
END GetSetLimits ;


(*
   GetFieldNo - returns the field number in the, set, which contains, element.
*)

PROCEDURE GetFieldNo (tokenno: CARDINAL; element: CARDINAL; set: CARDINAL; VAR offset: tree) : INTEGER ;
VAR
   low, high, bpw, c: CARDINAL ;
   location         : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   bpw := GetBitsPerBitset() ;
   GetSetLimits(set, low, high) ;

   (* check element is legal *)

   PushValue(element) ;
   PushValue(low) ;
   IF Less(tokenno)
   THEN
      (* out of range *)
      RETURN( -1 )
   ELSE
      PushValue(element) ;
      PushValue(high) ;
      IF Gre(tokenno)
      THEN
         RETURN( -1 )
      END
   END ;

   (* all legal *)

   PushValue(low) ;
   offset := PopIntegerTree() ;
   c := 0 ;
   PushValue(element) ;
   PushValue(low) ;
   PushIntegerTree(ToCardinal(location, PopIntegerTree())) ;
   PushCard(bpw) ;
   PushIntegerTree(ToCardinal(location, PopIntegerTree())) ;
   Addn ;
   WHILE GreEqu(tokenno) DO
      INC(c) ;   (* move onto next field *)
      PushValue(element) ;
      PushIntegerTree(ToCardinal(location, PopIntegerTree())) ;
      PushCard((c+1)*bpw) ;
      PushValue(low) ;
      PushIntegerTree(ToCardinal(location, PopIntegerTree())) ;
      Addn ;
      PushIntegerTree(offset) ;
      PushIntegerTree(ToCardinal(location, PopIntegerTree())) ;
      PushCard(bpw) ;
      PushIntegerTree(ToCardinal(location, PopIntegerTree())) ;
      Addn ;
      offset := PopIntegerTree()
   END ;
   RETURN( VAL(INTEGER, c) )
END GetFieldNo ;


(*
   CodeIncl - encode an InclOp:
              result := result + (1 << expr)
*)

PROCEDURE CodeIncl (result, expr: CARDINAL) ;
VAR
   low,
   high    : CARDINAL ;
   offset  : tree ;
   fieldno : INTEGER ;
   location: location_t ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant (CurrentQuadToken, expr) ;
   location := TokenToLocation (CurrentQuadToken) ;

   IF IsConst (result)
   THEN
      IF IsConst (expr)
      THEN
         InternalError ('this quadruple should have been removed by FoldIncl')
      ELSE
         InternalError ('should not get to here (why are we generating <incl const, var> ?)')
      END
   ELSE
      IF IsConst (expr)
      THEN
         fieldno := GetFieldNo (CurrentQuadToken, expr, GetType (result), offset) ;
         IF fieldno >= 0
         THEN
            PushValue (expr) ;
            PushIntegerTree (offset) ;
            Sub ;
            BuildIncludeVarConst (location,
                                  Mod2Gcc (GetType (result)),
                                  Mod2Gcc (result),
                                  PopIntegerTree (),
                                  GetMode (result) = LeftValue, fieldno)
         ELSE
            MetaErrorT1 (CurrentQuadToken, 'bit exceeded the range of set {%1Eatd}', result)
         END
      ELSE
         GetSetLimits (GetType (result), low, high) ;
         BuildIncludeVarVar (location,
                             Mod2Gcc (GetType(result)),
                             Mod2Gcc (result), Mod2Gcc(expr), GetMode(result) = LeftValue, Mod2Gcc (low))
      END
   END
END CodeIncl ;


(*
   FoldExcl - check whether we can fold the InclOp.
              op1 := op1 - (1 << op3)
*)

PROCEDURE FoldExcl (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; result, expr: CARDINAL) ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   TryDeclareConstant (tokenno, expr) ;
   IF IsConst (result) AND IsConst (expr)
   THEN
      IF GccKnowsAbout (expr) AND IsValueSolved (result)
      THEN
         PushValue (result) ;
         SubBit (tokenno, expr) ;
         AddModGcc (result, PopSetTree (tokenno)) ;
         p (result) ;
         NoChange := FALSE ;
         SubQuad(quad)
      END
   END
END FoldExcl ;


(*
   CodeExcl - encode an ExclOp:
              result := result - (1 << expr)
*)

PROCEDURE CodeExcl (result, expr: CARDINAL) ;
VAR
   low,
   high    : CARDINAL ;
   offset  : tree ;
   fieldno : INTEGER ;
   location: location_t ;
BEGIN
   (* firstly ensure that constant literals are declared *)
   DeclareConstant (CurrentQuadToken, expr) ;
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsConst (result)
   THEN
      InternalError ('should not get to here (if we do we should consider calling FoldInclOp)')
   ELSE
      IF IsConst (expr)
      THEN
         fieldno := GetFieldNo (CurrentQuadToken, expr, GetType (result), offset) ;
         IF fieldno >= 0
         THEN
            PushValue (expr) ;
            PushIntegerTree (offset) ;
            Sub ;
            BuildExcludeVarConst (location,
                                  Mod2Gcc (GetType (result)),
                                  Mod2Gcc (result), PopIntegerTree (),
                                  GetMode (result)=LeftValue, fieldno)
         ELSE
            MetaErrorT1 (CurrentQuadToken, 'bit exceeded the range of set {%1Eatd}', result)
         END
      ELSE
         GetSetLimits (GetType (result), low, high) ;
         BuildExcludeVarVar (location,
                             Mod2Gcc (GetType(result)),
                             Mod2Gcc (result), Mod2Gcc(expr), GetMode(result) = LeftValue, Mod2Gcc (low))
      END
   END
END CodeExcl ;


(*
   FoldUnary - check whether we can fold the unop operation.
*)

PROCEDURE FoldUnary (tokenno: CARDINAL; p: WalkAction;
                     unop: BuildUnaryProcedure; ZConstToTypedConst: tree;
                     quad: CARDINAL; result, expr: CARDINAL) ;
VAR
   tv      : tree ;
   location: location_t ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   TryDeclareConstant (tokenno, expr) ;
   location := TokenToLocation (tokenno) ;

   IF IsConst (expr)
   THEN
      IF GccKnowsAbout (expr)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst (result)
         THEN
            IF ZConstToTypedConst = tree(NIL)
            THEN
               IF (GetType (expr) = NulSym) OR IsOrdinalType (SkipType (GetType (expr)))
               THEN
                  ZConstToTypedConst := GetM2ZType ()
               ELSIF IsRealType (SkipType (GetType (expr))) OR IsRealN (SkipType (GetType (expr)))
               THEN
                  ZConstToTypedConst := GetM2RType ()
               ELSIF IsComplexType (SkipType (GetType (expr))) OR
                     IsComplexN (SkipType (GetType (expr)))
               THEN
                  ZConstToTypedConst := GetM2CType ()
               END
            END ;
            IF GetType(result) = NulSym
            THEN
               PutConst (result, NegateType (GetType (expr) (* , tokenno *) ))
            END ;
            tv := unop (location, LValueToGenericPtrOrConvert (expr, ZConstToTypedConst), FALSE) ;
            CheckOrResetOverflow (tokenno, tv, MustCheckOverflow (quad)) ;

            AddModGcc (result, DeclareKnownConstant (location, ZConstToTypedConst, tv)) ;
            p (result) ;
            NoChange := FALSE ;
            SubQuad (quad)
         ELSE
            (* we can still fold the expression, but not the assignment, however, we will
               not do this here but in CodeUnary
             *)
         END
      END
   END
END FoldUnary ;


(*
   FoldUnarySet - check whether we can fold the doOp operation.
*)

PROCEDURE FoldUnarySet (tokenno: CARDINAL; p: WalkAction; doOp: DoUnaryProcedure;
                        quad: CARDINAL; result, expr: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   (* firstly try and ensure that constants are declared *)
   TryDeclareConstant (tokenno, expr) ;
   location := TokenToLocation (tokenno) ;

   IF IsConst (expr) AND IsConstSet (expr) AND
      IsConst (result)
   THEN
      IF IsValueSolved (expr) AND (GetType (expr) # NulSym)
      THEN
         PutConst (result, FindType (expr)) ;
         PushValue (expr) ;
         doOp (tokenno) ;
         PopValue (result) ;
         PushValue (result) ;
         PutConstSet (result) ;
         AddModGcc (result,
                    DeclareKnownConstant (location,
                                          Mod2Gcc (GetType (expr)),
                                          PopSetTree (tokenno))) ;
         p (result) ;
         NoChange := FALSE ;
         SubQuad (quad)
      END
   END
END FoldUnarySet ;


(*
   CodeUnaryCheck - encode a unary arithmetic operation.
*)

PROCEDURE CodeUnaryCheck (unop: BuildUnaryCheckProcedure; ZConstToTypedConst: tree;
                          quad: CARDINAL; result, expr: CARDINAL) ;
VAR
   lowestType: CARDINAL ;
   min, max,
   lowest,
   tv        : tree ;
   location  : location_t ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant(CurrentQuadToken, expr) ;
   DeclareConstructor(CurrentQuadToken, quad, expr) ;
   location := TokenToLocation(CurrentQuadToken) ;

   lowestType := GetLType (result) ;
   IF lowestType=NulSym
   THEN
      lowest := NIL ;
   ELSE
      lowest := Mod2Gcc (lowestType)
   END ;
   IF GetMinMax (CurrentQuadToken, lowestType, min, max)
   THEN
      tv := unop (location, LValueToGenericPtr (location, expr), lowest, min, max)
   ELSE
      tv := unop (location, LValueToGenericPtr (location, expr), NIL, NIL, NIL)
   END ;
   CheckOrResetOverflow (CurrentQuadToken, tv, MustCheckOverflow(quad)) ;
   IF IsConst (result)
   THEN
      IF ZConstToTypedConst = tree (NIL)
      THEN
         ZConstToTypedConst := tree (Mod2Gcc( GetType (expr)))
      END ;
      (* still have a constant which was not resolved, pass it to gcc *)
      PutConst (result, FindType (expr)) ;
      ConstantKnownAndUsed (result, DeclareKnownConstant (location, ZConstToTypedConst, tv))
   ELSE
      IF EnableSSA AND IsVariableSSA (result)
      THEN
         Replace (result, tv)
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (result), tv)
      END
   END
END CodeUnaryCheck ;


(*
   CodeUnary - encode a unary arithmetic operation.
*)

PROCEDURE CodeUnary (unop: BuildUnaryProcedure; ZConstToTypedConst: tree;
                     quad: CARDINAL; result, expr: CARDINAL) ;
VAR
   tv      : tree ;
   location: location_t ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   DeclareConstant (CurrentQuadToken, expr) ;
   DeclareConstructor (CurrentQuadToken, quad, expr) ;
   location := TokenToLocation (CurrentQuadToken) ;

   tv := unop(location, LValueToGenericPtr (location, expr), FALSE) ;
   CheckOrResetOverflow (CurrentQuadToken, tv, MustCheckOverflow (quad)) ;
   IF IsConst(result)
   THEN
      IF ZConstToTypedConst=tree(NIL)
      THEN
         ZConstToTypedConst := tree(Mod2Gcc(GetType(expr)))
      END ;
      (* still have a constant which was not resolved, pass it to gcc *)
      PutConst (result, FindType (expr)) ;
      ConstantKnownAndUsed (result, DeclareKnownConstant (location, ZConstToTypedConst, tv))
   ELSE
      IF EnableSSA AND IsVariableSSA (result)
      THEN
         Replace (result, tv)
      ELSE
         BuildAssignmentStatement (location, Mod2Gcc (result), tv)
      END
   END
END CodeUnary ;


(*
   FoldNegate - check unary negate for constant folding.
*)

PROCEDURE FoldNegate (tokenno: CARDINAL; p: WalkAction;
                      quad: CARDINAL; result, expr: CARDINAL) ;
BEGIN
   IF IsConstSet (expr)
   THEN
      FoldUnarySet (tokenno, p, SetNegate, quad, result, expr)
   ELSE
      FoldUnary (tokenno, p, BuildNegate, NIL, quad, result, expr)
   END
END FoldNegate ;


(*
   CodeNegateChecked - code a negate instruction, determine whether checking
                       is required.
*)

PROCEDURE CodeNegateChecked (quad: CARDINAL; op1, op3: CARDINAL) ;
BEGIN
   IF IsConstSet (op3) OR IsSet (GetType (op3))
   THEN
      CodeUnarySet (BuildSetNegate, SetNegate, quad, op1, op3)
   ELSIF UnaryOperand (quad, op3)
   THEN
      IF MustCheckOverflow (quad)
      THEN
         CodeUnaryCheck (BuildNegateCheck, NIL, quad, op1, op3)
      ELSE
         CodeUnary (BuildNegate, NIL, quad, op1, op3)
      END
   END
END CodeNegateChecked ;


(*
   FoldSize - check unary SIZE for constant folding.
*)

PROCEDURE FoldSize (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   t       : tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   IF IsConst(op1) AND CompletelyResolved(op3)
   THEN
      IF op2=NulSym
      THEN
         t := BuildSize(location, Mod2Gcc(op3), FALSE) ;
         PushIntegerTree(t) ;
         PopValue(op1) ;
         PutConst(op1, Cardinal) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad) ;
         t := RememberConstant(t)
      ELSIF GccKnowsAbout(op2)
      THEN
         (* ignore the chosen varients as we implement it as a C union *)
         t := BuildSize(location, Mod2Gcc(op3), FALSE) ;
         PushIntegerTree(t) ;
         PopValue(op1) ;
         PutConst(op1, Cardinal) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad) ;
         t := RememberConstant(t)
      END
   END
END FoldSize ;


(*
   CodeSize - encode the inbuilt SIZE function.
*)

PROCEDURE CodeSize (result, sym: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   PushIntegerTree (BuildSize (location, Mod2Gcc (sym), FALSE)) ;
   IF IsConst (result)
   THEN
      PopValue (result) ;
      PutConst (result, Cardinal) ;
      PushValue (result) ;
      ConstantKnownAndUsed (result,
                            DeclareKnownConstant (location,
                                                  GetIntegerType (),
                                                  PopIntegerTree ()))
   ELSE
      BuildAssignmentStatement (location, Mod2Gcc (result), PopIntegerTree ())
   END
END CodeSize ;


(*
   FoldRecordField - check whether we can fold an RecordFieldOp quadruple.
                     Very similar to FoldBinary, except that we need to
                     hard code a few parameters to the gcc backend.
*)

PROCEDURE FoldRecordField (tokenno: CARDINAL; p: WalkAction;
                           quad: CARDINAL; result, record, field: CARDINAL) ;
VAR
   recordType,
   fieldType : CARDINAL ;
   ptr       : tree ;
   location  : location_t ;
BEGIN
   RETURN ;  (* this procedure should no longer be called *)

   location := TokenToLocation(tokenno) ;
   (* firstly ensure that any constant literal is declared *)
   TryDeclareConstant(tokenno, record) ;
   IF IsRecordField(record) OR IsFieldVarient(record)
   THEN
      recordType := GetType (record) ;
      fieldType := GetType (field) ;
      IF GccKnowsAbout (record) AND GccKnowsAbout (field) AND
         GccKnowsAbout (recordType) AND GccKnowsAbout (fieldType) AND
         CompletelyResolved (recordType) AND CompletelyResolved (fieldType)
      THEN
         (* fine, we can take advantage of this and fold constants *)
         IF IsConst (result)
         THEN
            ptr := BuildComponentRef (location, Mod2Gcc (record), Mod2Gcc (field)) ;
            IF NOT IsValueSolved (result)
            THEN
               PushIntegerTree (ptr) ;
               PopValue (result)
            END ;
            PutConst (result, fieldType) ;
            AddModGcc (result, DeclareKnownConstant (location, Mod2Gcc (fieldType), ptr)) ;
            p (result) ;
            NoChange := FALSE ;
            SubQuad (quad)
         ELSE
            (* we can still fold the expression, but not the assignment, however, we will
               not do this here but in CodeOffset
             *)
         END
      END
   END
END FoldRecordField ;


(*
   CodeRecordField - encode a reference to a field within a record.
*)

PROCEDURE CodeRecordField (result, record, field: CARDINAL) ;
VAR
   recordType,
   fieldType : CARDINAL ;
   ptr       : tree ;
   location  : location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   (* firstly ensure that any constant literal is declared *)
   IF IsRecordField (field) OR IsFieldVarient (field)
   THEN
      recordType := GetType (record) ;
      fieldType := GetType (field) ;
      IF GccKnowsAbout (record) AND GccKnowsAbout (field) AND
         GccKnowsAbout (recordType) AND GccKnowsAbout (fieldType) AND
         CompletelyResolved (recordType) AND CompletelyResolved (fieldType)
      THEN
         IF GetMode(record)=LeftValue
         THEN
            ptr := BuildComponentRef (location,
                                      BuildIndirect (location, Mod2Gcc (record), Mod2Gcc (recordType)),
                                      Mod2Gcc (field))
         ELSE
            ptr := BuildComponentRef (location, Mod2Gcc (record), Mod2Gcc (field))
         END ;
         AddModGcc (result, ptr)
      ELSE
         InternalError ('symbol type should have been declared by now')
      END
   ELSE
      InternalError ('not expecting this type of symbol')
   END
END CodeRecordField ;


(*
   BuildHighFromChar -
*)

PROCEDURE BuildHighFromChar (operand: CARDINAL) : tree ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(operand)) ;
   IF IsConstString (operand) AND
      (IsConstStringM2nul (operand) OR IsConstStringCnul (operand))
   THEN
      RETURN GetCardinalOne (location)
   END ;
   RETURN GetCardinalZero (location)
END BuildHighFromChar ;


(*
   SkipToArray -
*)

PROCEDURE SkipToArray (operand, dim: CARDINAL) : CARDINAL ;
VAR
   type: CARDINAL ;
BEGIN
   WHILE dim>1 DO
      type := SkipType(GetType(operand)) ;
      IF IsArray(type)
      THEN
         operand := type
      END ;
      DEC(dim)
   END ;
   RETURN( operand )
END SkipToArray ;


(*
   BuildHighFromArray -
*)

PROCEDURE BuildHighFromArray (tokenno: CARDINAL; dim, operand: CARDINAL) : tree ;
VAR
   Type    : CARDINAL ;
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   Type := SkipType (GetType (SkipToArray (operand, dim))) ;
   RETURN BuildHighFromStaticArray (location, (* dim, *) Type)
END BuildHighFromArray ;


(*
   BuildHighFromStaticArray -
*)

PROCEDURE BuildHighFromStaticArray (location: location_t; (* dim, *) Type: CARDINAL) : tree ;
VAR
   High, Low: CARDINAL ;
   Subscript,
   Subrange : CARDINAL ;
BEGIN
   Assert (IsArray (Type)) ;
   Subscript := GetArraySubscript (Type) ;
   Subrange := SkipType (GetType (Subscript)) ;
   IF IsEnumeration (Subrange)
   THEN
      GetBaseTypeMinMax (Subrange, Low, High) ;
      IF GccKnowsAbout (High)
      THEN
         RETURN tree (Mod2Gcc (High))
      END
   ELSIF IsSubrange(Subrange)
   THEN
      GetSubrange (Subrange, High, Low) ;
      IF GccKnowsAbout (Low) AND GccKnowsAbout (High)
      THEN
         RETURN BuildSub (location, Mod2Gcc (High), Mod2Gcc (Low), TRUE)
      END
   ELSE
      MetaError1 ('array subscript {%1EDad:for} must be a subrange or enumeration type', Type) ;
      RETURN tree(NIL)
   END ;
   IF GccKnowsAbout (High)
   THEN
      RETURN tree (Mod2Gcc (High))
   ELSE
      RETURN tree (NIL)
   END
END BuildHighFromStaticArray ;


(*
   BuildHighFromString -
*)

PROCEDURE BuildHighFromString (operand: CARDINAL) : tree ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (GetDeclaredMod (operand)) ;
   IF GccKnowsAbout (operand) AND (StringLength (Mod2Gcc (operand)) > 0)
   THEN
      RETURN( BuildIntegerConstant (StringLength (Mod2Gcc (operand))-1) )
   ELSE
      RETURN( GetIntegerZero (location) )
   END
END BuildHighFromString ;


(*
   ResolveHigh - given an Modula-2 operand, it resolves the HIGH(operand)
                 and returns a GCC constant symbol containing the value of
                 HIGH(operand).
*)

PROCEDURE ResolveHigh (tokenno: CARDINAL; dim, operand: CARDINAL) : tree ;
VAR
   Type    : CARDINAL ;
   location: location_t ;
BEGIN
   Type := SkipType(GetType(operand)) ;
   location := TokenToLocation(tokenno) ;

   IF (Type=Char) AND (dim=1)
   THEN
      RETURN( BuildHighFromChar(operand) )
   ELSIF IsConstString(operand) AND (dim=1)
   THEN
      RETURN( BuildHighFromString(operand) )
   ELSIF IsArray(Type)
   THEN
      RETURN( BuildHighFromArray(tokenno, dim, operand) )
   ELSIF IsUnbounded(Type)
   THEN
      RETURN( GetHighFromUnbounded(location, dim, operand) )
   ELSE
      MetaErrorT1 (tokenno,
                   'base procedure HIGH expects a variable of type array or a constant string or CHAR as its parameter, rather than {%1Etad}',
                   operand) ;
      RETURN( GetIntegerZero(location) )
   END
END ResolveHigh ;


(*
   FoldHigh - if the array is not dynamic then we should be able to
              remove the HighOp quadruple and assign op1 with
              the known compile time HIGH(op3).
*)

PROCEDURE FoldHigh (tokenno: CARDINAL; p: WalkAction;
                    quad: CARDINAL; op1, dim, op3: CARDINAL) ;
VAR
   t       : tree ;
   location: location_t ;
BEGIN
   (* firstly ensure that any constant literal is declared *)
   TryDeclareConstant(tokenno, op3) ;
   location := TokenToLocation(tokenno) ;
   IF GccKnowsAbout(op3) AND CompletelyResolved(op3)
   THEN
      t := ResolveHigh(tokenno, dim, op3) ;
      (* fine, we can take advantage of this and fold constants *)
      IF IsConst(op1) AND (t#tree(NIL))
      THEN
         PutConst(op1, Cardinal) ;
         AddModGcc(op1,
                   DeclareKnownConstant(location, GetCardinalType(),
                                        ToCardinal(location, t))) ;
         p(op1) ;
         NoChange := FALSE ;
         SubQuad(quad)
      ELSE
         (* we can still fold the expression, but not the assignment, however, we will
            not do this here but in CodeHigh
         *)
      END
   END
END FoldHigh ;


(*
   CodeHigh - encode a unary arithmetic operation.
*)

PROCEDURE CodeHigh (result, dim, array: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;

   (* firstly ensure that any constant literal is declared *)
   DeclareConstant (CurrentQuadToken, array) ;
   IF IsConst (result)
   THEN
      (* still have a constant which was not resolved, pass it to gcc *)
      ConstantKnownAndUsed (result,
                            DeclareKnownConstant(location,
                                                 GetM2ZType (),
                                                 ResolveHigh (CurrentQuadToken, dim, array)))
   ELSE
      BuildAssignmentStatement (location,
                                Mod2Gcc (result),
                                BuildConvert (location,
                                              Mod2Gcc (GetType (result)),
                                              ResolveHigh (CurrentQuadToken, dim, array),
                                              FALSE))
   END
END CodeHigh ;


(*
   CodeUnbounded - codes the creation of an unbounded parameter variable.
                   places the address of op3 into *op1
*)

PROCEDURE CodeUnbounded (result, array: CARDINAL) ;
VAR
   Addr    : tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;

   DeclareConstant (CurrentQuadToken, array) ;
   IF IsConstString (array) OR (IsConst (array) AND (GetSType (array) = Char))
   THEN
      BuildAssignmentStatement (location, Mod2Gcc (result), BuildAddr (location, PromoteToString (CurrentQuadToken, array), FALSE))
   ELSIF IsConstructor (array)
   THEN
      BuildAssignmentStatement (location, Mod2Gcc (result), BuildAddr (location, Mod2Gcc (array), TRUE))
   ELSIF IsUnbounded (GetType (array))
   THEN
      IF GetMode(array) = LeftValue
      THEN
         Addr := BuildConvert (location, Mod2Gcc (GetType (result)), Mod2Gcc (array), FALSE)
      ELSE
         Addr := BuildComponentRef (location, Mod2Gcc (array), Mod2Gcc (GetUnboundedAddressOffset (GetType (array))))
      END ;
      BuildAssignmentStatement (location, Mod2Gcc (result), Addr)
   ELSIF GetMode(array) = RightValue
   THEN
      BuildAssignmentStatement (location, Mod2Gcc (result), BuildAddr (location, Mod2Gcc (array), FALSE))
   ELSE
      BuildAssignmentStatement (location, Mod2Gcc (result), Mod2Gcc (array))
   END
END CodeUnbounded ;


(*
   AreSubrangesKnown - returns TRUE if the subranges values used within, array, are known.
*)

PROCEDURE AreSubrangesKnown (array: CARDINAL) : BOOLEAN ;
VAR
   type,
   subscript,
   low, high: CARDINAL ;
BEGIN
   IF GccKnowsAbout(array)
   THEN
      subscript := GetArraySubscript(array) ;
      IF subscript=NulSym
      THEN
         InternalError ('not expecting a NulSym as a subscript')
      ELSE
         type := SkipType(GetType(subscript)) ;
         low  := GetTypeMin(type) ;
         high := GetTypeMax(type) ;
         RETURN( GccKnowsAbout(low) AND GccKnowsAbout(high) )
      END
   ELSE
      RETURN( FALSE )
   END
END AreSubrangesKnown ;


(*
   CodeArray - res is an lvalue which will point to the array element.
*)

PROCEDURE CodeArray (res, index, array: CARDINAL) ;
VAR
   resType,
   arrayDecl,
   type,
   low,
   subscript  : CARDINAL ;
   a, ta,
   ti, tl     : tree ;
   location   : location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;

   arrayDecl := SkipType (GetType (array)) ;
   IF AreSubrangesKnown (arrayDecl)
   THEN
      subscript := GetArraySubscript (arrayDecl) ;
      type := SkipType (GetType (subscript)) ;
      low  := GetTypeMin (type) ;
      resType := GetVarBackEndType(res) ;
      IF resType=NulSym
      THEN
         resType := SkipType(GetType(res))
      END ;
      ta := Mod2Gcc(SkipType(GetType(arrayDecl))) ;
      IF GetMode(array)=LeftValue
      THEN
         a := BuildIndirect(location, Mod2Gcc(array), Mod2Gcc(SkipType(GetType(array))))
      ELSE
         a := Mod2Gcc(array)
      END ;
      IF IsArrayLarge(arrayDecl)
      THEN
         tl := BuildConvert(location, Mod2Gcc(type), Mod2Gcc(low), FALSE) ;
         ti := BuildConvert(location, Mod2Gcc(type), Mod2Gcc(index), FALSE) ;
         ti := BuildConvert(location, GetIntegerType(), BuildSub(location, ti, tl, FALSE), FALSE) ;
         tl := GetIntegerZero(location)
      ELSE
         tl := BuildConvert(location, GetIntegerType(), Mod2Gcc(low), FALSE) ;
         ti := BuildConvert(location, GetIntegerType(), Mod2Gcc(index), FALSE)
      END ;
      (* ti := BuildConvert(location, GetIntegerType(), Mod2Gcc(high), FALSE) ; *)
      BuildAssignmentStatement (location,
                                Mod2Gcc (res),
                                BuildConvert (location,
                                              Mod2Gcc (resType),
                                              BuildAddr (location, BuildArray (location,
                                                                               ta, a, ti, tl),
                                                        FALSE),
                                              FALSE))
   ELSE
      InternalError ('subranges not yet resolved')
   END
END CodeArray ;


(*
   FoldElementSizeForArray - attempts to calculate the Subscript
                             multiplier for the index op3.
*)

PROCEDURE FoldElementSizeForArray (tokenno: CARDINAL; quad: CARDINAL;
                                   p: WalkAction;
                                   result, type: CARDINAL) ;
VAR
   Subscript: CARDINAL ;
   location : location_t ;
BEGIN
   location := TokenToLocation (tokenno) ;

   IF IsConst (result) AND (NOT GccKnowsAbout (result))
   THEN
      Subscript := GetArraySubscript (type) ;
      IF IsSizeSolved (Subscript)
      THEN
         PutConst (result, Integer) ;
         PushSize (Subscript) ;
         AddModGcc (result,
                    DeclareKnownConstant (location,
                                          GetCardinalType (),
                                          BuildConvert (location,
                                                        GetCardinalType (),
                                                        PopIntegerTree (),
                                                        TRUE))) ;
         p (result) ;
         NoChange := FALSE ;
         SubQuad (quad)
      END
   END
END FoldElementSizeForArray ;


(*
   FoldElementSizeForUnbounded - Unbounded arrays only have one index,
                                 therefore element size will be the
                                 TSIZE(Type) where Type is defined as:
                                 ARRAY OF Type.
*)

PROCEDURE FoldElementSizeForUnbounded (tokenno: CARDINAL; quad: CARDINAL;
                                       p: WalkAction;
                                       result, ArrayType: CARDINAL) ;
VAR
   Type    : CARDINAL ;
   location: location_t ;
BEGIN
   location := TokenToLocation (tokenno) ;

   IF IsConst (result)
   THEN
      IF GccKnowsAbout (result)
      THEN
         InternalError ('cannot assign a value twice to a constant')
      ELSE
         Assert (IsUnbounded (ArrayType)) ;
         Type := GetType (ArrayType) ;
         IF GccKnowsAbout (Type)
         THEN
            PutConst (result, Cardinal) ;
            AddModGcc (result,
                       DeclareKnownConstant (location,
                                             GetCardinalType (),
                                             BuildConvert (location,
                                                           GetCardinalType (),
                                                           FindSize (tokenno, Type),
                                                           TRUE))) ;
            p (result) ;
            NoChange := FALSE ;
            SubQuad (quad)
         END
      END
   END
END FoldElementSizeForUnbounded ;


(*
   FoldElementSize - folds the element size for an ArraySym or UnboundedSym.
                     ElementSize returns a constant which defines the
                     multiplier to be multiplied by this element index.
*)

PROCEDURE FoldElementSize (tokenno: CARDINAL; p: WalkAction;
                           quad: CARDINAL; result, type: CARDINAL) ;
BEGIN
   IF IsUnbounded (type)
   THEN
      FoldElementSizeForUnbounded (tokenno, quad, p, result, type)
   ELSIF IsArray (type)
   THEN
      FoldElementSizeForArray (tokenno, quad, p, result, type)
   ELSE
      InternalError ('expecting UnboundedSym or ArraySym')
   END
END FoldElementSize ;


(*
   PopKindTree - returns a Tree from M2ALU of the type implied by, op.
*)

PROCEDURE PopKindTree (op: CARDINAL; tokenno: CARDINAL) : tree ;
VAR
   type: CARDINAL ;
BEGIN
   IF IsConst (op) AND IsConstString (op)
   THEN
      (* Converting a nul char or char for example.  *)
      RETURN PopIntegerTree ()
   ELSE
      type := SkipType (GetType (op)) ;
      IF IsSet (type)
      THEN
         RETURN( PopSetTree (tokenno) )
      ELSIF IsRealType (type)
      THEN
         RETURN( PopRealTree () )
      ELSE
         RETURN( PopIntegerTree () )
      END
   END
END PopKindTree ;


(*
   FoldConvert - attempts to fold expr to type into result
                 providing that result and expr are constants.
                 If required convert will alter the machine representation
                 of expr to comply with type.
*)

PROCEDURE FoldConvert (tokenno: CARDINAL; p: WalkAction;
                       quad: CARDINAL; result, type, expr: CARDINAL) ;

VAR
   tl      : tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation (tokenno) ;
   (* First ensure that constant literals are declared.  *)
   TryDeclareConstant (tokenno, expr) ;
   IF IsConstant (expr)
   THEN
      IF GccKnowsAbout (type) AND
         (IsProcedure (expr) OR IsValueSolved (expr)) AND
         GccKnowsAbout (SkipType (type))
      THEN
         (* The type is known and expr is resolved so fold the convert.  *)
         IF IsConst (result)
         THEN
            PutConst (result, type) ;   (* Change result type just in case.  *)
            tl := Mod2Gcc (SkipType (type)) ;
            IF IsProcedure (expr)
            THEN
               AddModGcc (result, BuildConvert (location, tl, Mod2Gcc (expr), TRUE))
            ELSE
               PushValue (expr) ;
               IF IsConstSet (expr)
               THEN
                  IF IsSet (SkipType (type))
                  THEN
                     WriteFormat0 ('cannot convert values between sets')
                  ELSE
                     PushIntegerTree (FoldAndStrip (BuildConvert (location, tl, PopSetTree (tokenno), TRUE))) ;
                     PopValue (result) ;
                     PushValue (result) ;
                     AddModGcc (result, PopIntegerTree())
                  END
               ELSE
                  IF IsSet (SkipType (type))
                  THEN
                     PushSetTree (tokenno,
                                  FoldAndStrip (BuildConvert (location, tl, PopKindTree (expr, tokenno),
                                                              TRUE)), SkipType (type)) ;
                     PopValue (result) ;
                     PutConstSet (result) ;
                     PushValue (result) ;
                     AddModGcc (result, PopSetTree (tokenno))
                  ELSIF IsRealType (SkipType (type))
                  THEN
                     PushRealTree (FoldAndStrip (BuildConvert (location, tl, PopKindTree (expr, tokenno),
                                                               TRUE))) ;
                     PopValue (result) ;
                     PushValue (result) ;
                     AddModGcc (result, PopKindTree (result, tokenno))
                  ELSE
                     (* Let CheckOverflow catch a potential overflow rather than BuildConvert.  *)
                     PushIntegerTree (FoldAndStrip (BuildConvert (location, tl,
                                                                  PopKindTree (expr, tokenno),
                                                                  FALSE))) ;
                     PopValue (result) ;
                     PushValue (result) ;
                     CheckOrResetOverflow (tokenno, PopKindTree (result, tokenno), MustCheckOverflow (quad)) ;
                     PushValue (result) ;
                     AddModGcc (result, PopKindTree (result, tokenno))
                  END
               END
            END ;
            p (result) ;
            NoChange := FALSE ;
            SubQuad (quad)
         END
      END
   END
END FoldConvert ;


(*
   CodeConvert - Converts, rhs, to, type, placing the result into lhs.
                 Convert will, if need be, alter the machine representation
                 of op3 to comply with TYPE op2.
*)

PROCEDURE CodeConvert (quad: CARDINAL; lhs, type, rhs: CARDINAL) ;
VAR
   tl, tr  : tree ;
   location: location_t ;
BEGIN
   CheckStop(quad) ;

   (* firstly ensure that constant literals are declared *)
   DeclareConstant(CurrentQuadToken, rhs) ;
   DeclareConstructor(CurrentQuadToken, quad, rhs) ;
   location := TokenToLocation(CurrentQuadToken) ;

   tl := LValueToGenericPtr(location, type) ;
   IF IsProcedure(rhs)
   THEN
      tr := BuildAddr(location, Mod2Gcc(rhs), FALSE)
   ELSE
      tr := LValueToGenericPtr(location, rhs) ;
      tr := ConvertRHS(tr, type, rhs)
   END ;
   IF IsConst(lhs)
   THEN
      (* fine, we can take advantage of this and fold constant *)
      PutConst(lhs, type) ;
      tl := Mod2Gcc(SkipType(type)) ;
      ConstantKnownAndUsed (lhs,
                            BuildConvert (location, tl, Mod2Gcc (rhs), TRUE))
   ELSE
      BuildAssignmentStatement (location, Mod2Gcc (lhs), BuildConvert (location, tl, tr, TRUE)) ;
   END
END CodeConvert ;


(*
   CodeCoerce - Coerce op3 to type op2 placing the result into
                op1.
                Coerce will NOT alter the machine representation
                of op3 to comply with TYPE op2.
                Therefore it _insists_ that under all circumstances that the
                type sizes of op1 and op3 are the same.
                CONVERT will perform machine manipulation to change variable
                types, coerce does no such thing.
*)

PROCEDURE CodeCoerce (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   DeclareConstructor(CurrentQuadToken, quad, op3) ;
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsProcedure(op3)
   THEN
      IF AreConstantsEqual(FindSize(CurrentQuadToken, op1), FindSize(CurrentQuadToken, Address))
      THEN
         IF IsConst(op1)
         THEN
            ConstantKnownAndUsed(op1, CheckConstant(CurrentQuadToken, op1, op3))
         ELSE
            BuildAssignmentStatement (location, Mod2Gcc (op1), Mod2Gcc (op3))
         END
      ELSE
         MetaErrorT0 (CurrentQuadToken,
                      '{%E}procedure address can only be stored in an address sized operand')
      END
   ELSIF IsConst(op3) OR AreConstantsEqual(FindSize(CurrentQuadToken, op1), FindSize(CurrentQuadToken, op3))
   THEN
      IF IsConst(op1)
      THEN
         ConstantKnownAndUsed(op1,
                              DeclareKnownConstant(location,
                                                   Mod2Gcc(GetType(op1)),
                                                   Mod2Gcc(op3)))
      ELSE
         Assert(GccKnowsAbout(op2)) ;
         IF IsConst(op3)
         THEN
            BuildAssignmentStatement (location, Mod2Gcc(op1), Mod2Gcc(op3))
         ELSE
            (* does not work t := BuildCoerce(Mod2Gcc(op1), Mod2Gcc(op2), Mod2Gcc(op3)) *)
            checkDeclare (op1) ;
            AddStatement (location,
                          MaybeDebugBuiltinMemcpy(location,
                                                  BuildAddr(location, Mod2Gcc(op1), FALSE),
                                                  BuildAddr(location, Mod2Gcc(op3), FALSE),
                                                  FindSize(CurrentQuadToken, op2)))
         END
      END
   ELSE
      MetaErrorT0 (CurrentQuadToken,
                   'can only {%kCAST} objects of the same size')
   END
END CodeCoerce ;


(*
   FoldCoerce -
*)

PROCEDURE FoldCoerce (tokenno: CARDINAL; p: WalkAction;
                      quad, op1, op2, op3: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   TryDeclareConstant(tokenno, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   location := TokenToLocation(tokenno) ;

   IF GccKnowsAbout(op2) AND GccKnowsAbout(op3)
   THEN
      IF IsProcedure(op3)
      THEN
         IF AreConstantsEqual(FindSize(tokenno, op1), FindSize(tokenno, Address))
         THEN
            IF IsConst(op1)
            THEN
               AddModGcc(op1,
                         DeclareKnownConstant(location,
                                              Mod2Gcc(GetType(op1)),
                                              Mod2Gcc(op3))) ;
               p(op1) ;
               NoChange := FALSE ;
               SubQuad(quad)
            END
         ELSE
            MetaErrorT0 (CurrentQuadToken,
                         '{%E}procedure address can only be stored in a address sized operand')
         END
      ELSIF IsConst(op3)
      THEN
         IF IsConst(op1)
         THEN
            AddModGcc(op1,
                      DeclareKnownConstant(location,
                                           Mod2Gcc(GetType(op1)),
                                           Mod2Gcc(op3))) ;
            p(op1) ;
            NoChange := FALSE ;
            SubQuad(quad)
         END
      END
   END
END FoldCoerce ;


(*
   CanConvert - returns TRUE if we can convert variable, var, to a, type.
*)

PROCEDURE CanConvert (type, var: CARDINAL) : BOOLEAN ;
VAR
   svar,
   stype: CARDINAL ;
BEGIN
   stype := SkipType(type) ;
   svar := SkipType(GetType(var)) ;
   RETURN (IsBaseType(stype) OR IsOrdinalType(stype) OR IsSystemType(stype)) AND
          (IsBaseType(svar) OR IsOrdinalType(svar) OR IsSystemType(stype))
END CanConvert ;


(*
   CodeCast - Cast op3 to type op2 placing the result into op1.
              Cast will NOT alter the machine representation
              of op3 to comply with TYPE op2 as long as SIZE(op3)=SIZE(op2).
              If the sizes differ then Convert is called.
*)

PROCEDURE CodeCast (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   DeclareConstant(CurrentQuadToken, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   DeclareConstructor(CurrentQuadToken, quad, op3) ;
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsProcedure(op3)
   THEN
      IF AreConstantsEqual(FindSize(CurrentQuadToken, op1), FindSize(CurrentQuadToken, Address))
      THEN
         IF IsConst(op1)
         THEN
            ConstantKnownAndUsed(op1, CheckConstant(CurrentQuadToken, op1, op3))
         ELSE
            BuildAssignmentStatement (location, Mod2Gcc(op1), Mod2Gcc(op3))
         END
      ELSE
         MetaErrorT0 (CurrentQuadToken,
                      '{%E}procedure address can only be stored in an address sized operand')
      END
   ELSIF IsConst(op3) OR AreConstantsEqual(FindSize(CurrentQuadToken, op1), FindSize(CurrentQuadToken, op3))
   THEN
      CodeCoerce(quad, op1, op2, op3)
   ELSE
      IF PedanticCast AND (NOT CanConvert(op2, op3))
      THEN
         MetaError2 ('{%WkCAST} cannot copy a variable src {%2Dad} to a destination {%1Dad} as they are of different sizes and are not ordinal or real types',
                     op1, op3)
      END ;
      CodeConvert(quad, op1, op2, op3)
   END
END CodeCast ;


(*
   FoldCoerce -
*)

PROCEDURE FoldCast (tokenno: CARDINAL; p: WalkAction;
                    quad, op1, op2, op3: CARDINAL) ;
BEGIN
   TryDeclareConstant(tokenno, op3) ;  (* checks to see whether it is a constant literal and declares it *)
   IF GccKnowsAbout(op2) AND GccKnowsAbout(op3)
   THEN
      IF IsProcedure(op3)
      THEN
         IF AreConstantsEqual(FindSize(tokenno, op1), FindSize(tokenno, Address))
         THEN
            FoldCoerce(tokenno, p, quad, op1, op2, op3)
         ELSE
            MetaErrorT0 (tokenno,
                         '{%E}procedure address can only be stored in an address sized operand')
         END
      ELSIF IsConst(op3)
      THEN
         FoldCoerce(tokenno, p, quad, op1, op2, op3)
      END
   END
END FoldCast ;


(*
   CreateLabelProcedureN - creates a label using procedure name and
                           an integer.
*)

PROCEDURE CreateLabelProcedureN (proc: CARDINAL; leader: ARRAY OF CHAR;
                                 unboundedCount, n: CARDINAL) : String ;
VAR
   n1, n2: String ;
BEGIN
   n1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(proc)))) ;
   n2 := Mark(InitString(leader)) ;
   (* prefixed by .L unboundedCount and n to ensure that no Modula-2 identifiers clash *)
   RETURN( Sprintf4(Mark(InitString('.L%d.%d.unbounded.%s.%s')), unboundedCount, n, n1, n2) )
END CreateLabelProcedureN ;


(*
   CreateLabelName - creates a namekey from quadruple, q.
*)

PROCEDURE CreateLabelName (q: CARDINAL) : String ;
BEGIN
   (* prefixed by . to ensure that no Modula-2 identifiers clash *)
   RETURN( Sprintf1(Mark(InitString('.L%d')), q) )
END CreateLabelName ;


(*
   CodeGoto - creates a jump to a labeled quadruple.
*)

PROCEDURE CodeGoto (destquad: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;
   BuildGoto (location, string (CreateLabelName (destquad)))
END CodeGoto ;


(*
   CheckReferenced - checks to see whether this quadruple requires a label.
*)

PROCEDURE CheckReferenced (quad: CARDINAL; op: QuadOperator) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   (* we do not create labels for procedure entries *)
   IF (op#ProcedureScopeOp) AND (op#NewLocalVarOp) AND IsReferenced(quad)
   THEN
      DeclareLabel(location, string(CreateLabelName(quad)))
   END
END CheckReferenced ;


(*
   CodeIfSetLess -
*)

PROCEDURE CodeIfSetLess (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
   location  : location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError ('this should have been folded in the calling procedure')
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(CurrentQuadToken, settype), FindSize(CurrentQuadToken, Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(location,
             BuildIsNotSuperset(location,
                                BuildConvert(location, GetWordType(), Mod2Gcc(op1), FALSE),
                                BuildConvert(location, GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(location,
                                    Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildIsSuperset,
                                    falselabel) ;

      BuildGoto(location, string(CreateLabelName(op3))) ;
      DeclareLabel(location, falselabel)
   END
END CodeIfSetLess ;


(*
   PerformCodeIfLess - codes the quadruple if op1 < op2 then goto op3
*)

PROCEDURE PerformCodeIfLess (quad: CARDINAL) ;
VAR
   tl, tr  : tree ;
   location                   : location_t ;
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   GetQuadOtok (quad, combined, op,
                left, right, dest, overflow,
                constExpr,
                leftpos, rightpos, destpos) ;
   location := TokenToLocation (combined) ;

   IF IsConst(left) AND IsConst(right)
   THEN
      PushValue(left) ;
      PushValue(right) ;
      IF Less(CurrentQuadToken)
      THEN
         BuildGoto(location, string(CreateLabelName(dest)))
      ELSE
         (* Fall through.  *)
      END
   ELSIF IsConstSet(left) OR (IsVar(left) AND IsSet(SkipType(GetType(left)))) OR
         IsConstSet(right) OR (IsVar(right) AND IsSet(SkipType(GetType(right))))
   THEN
      CodeIfSetLess(quad, left, right, dest)
   ELSE
      IF IsComposite(GetType(left)) OR IsComposite(GetType(right))
      THEN
         MetaErrorT2 (combined,
                      'comparison tests between composite types not allowed {%1Eatd} and {%2atd}',
                      left, right)
      ELSE
         ConvertBinaryOperands (location,
                                tl, tr,
                                ComparisonMixTypes (left, right,
                                                    SkipType (GetType (left)),
                                                    SkipType (GetType (right)),
                                                    combined),
                                left, right) ;
         DoJump (location,
                 BuildLessThan (location, tl, tr), NIL, string (CreateLabelName (dest)))
      END
   END
END PerformCodeIfLess ;


(*
   CodeIfLess - codes the quadruple if op1 < op2 then goto op3
*)

PROCEDURE CodeIfLess (quad: CARDINAL) ;
BEGIN
   IF IsValidExpressionRelOp (quad, FALSE)
   THEN
      PerformCodeIfLess (quad)
   END
END CodeIfLess ;


(*
   CodeIfSetGre -
*)

PROCEDURE CodeIfSetGre (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
   location  : location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError ('this should have been folded in the calling procedure')
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(CurrentQuadToken, settype), FindSize(CurrentQuadToken, Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(location,
             BuildIsNotSubset(location,
                              BuildConvert(location, GetWordType(), Mod2Gcc(op1), FALSE),
                              BuildConvert(location, GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(location,
                                    Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildIsSubset,
                                    falselabel) ;

      BuildGoto(location, string(CreateLabelName(op3))) ;
      DeclareLabel(location, falselabel)
   END
END CodeIfSetGre ;


(*
   PerformCodeIfGre - codes the quadruple if op1 > op2 then goto op3
*)

PROCEDURE PerformCodeIfGre (quad: CARDINAL) ;
VAR
   tl, tr  : tree ;
   location                   : location_t ;
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   GetQuadOtok (quad, combined, op,
                left, right, dest, overflow, constExpr,
                leftpos, rightpos, destpos) ;
   location := TokenToLocation (combined) ;
   IF IsConst(left) AND IsConst(right)
   THEN
      PushValue(left) ;
      PushValue(right) ;
      IF Gre(combined)
      THEN
         BuildGoto(location, string(CreateLabelName(dest)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(left) OR (IsVar(left) AND IsSet(SkipType(GetType(left)))) OR
         IsConstSet(right) OR (IsVar(right) AND IsSet(SkipType(GetType(right))))
   THEN
      CodeIfSetGre(quad, left, right, dest)
   ELSE
      IF IsComposite(GetType(left)) OR IsComposite(GetType(right))
      THEN
         MetaErrorT2 (combined,
                      'comparison tests between composite types not allowed {%1Eatd} and {%2atd}',
                      left, right)
      ELSE
         ConvertBinaryOperands(location,
                               tl, tr,
                               ComparisonMixTypes (left, right,
                                                   SkipType (GetType (left)),
                                                   SkipType (GetType (right)),
                                                   combined),
                               left, right) ;
         DoJump(location, BuildGreaterThan(location, tl, tr), NIL, string(CreateLabelName(dest)))
      END
   END
END PerformCodeIfGre ;


(*
   CodeIfGre - codes the quadruple if op1 > op2 then goto op3
*)

PROCEDURE CodeIfGre (quad: CARDINAL) ;
BEGIN
   IF IsValidExpressionRelOp (quad, FALSE)
   THEN
      PerformCodeIfGre (quad)
   END
END CodeIfGre ;


(*
   CodeIfSetLessEqu -
*)

PROCEDURE CodeIfSetLessEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
   location  : location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError ('this should have been folded in the calling procedure')
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(CurrentQuadToken, settype), FindSize(CurrentQuadToken, Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(location,
             BuildIsSubset(location,
                           BuildConvert(location, GetWordType(), Mod2Gcc(op1), FALSE),
                           BuildConvert(location, GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(location,
                                    Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildIsNotSubset,
                                    falselabel) ;

      BuildGoto(location, string(CreateLabelName(op3))) ;
      DeclareLabel(location, falselabel)
   END
END CodeIfSetLessEqu ;


(*
   PerformCodeIfLessEqu - codes the quadruple if op1 <= op2 then goto op3
*)

PROCEDURE PerformCodeIfLessEqu (quad: CARDINAL) ;
VAR
   tl, tr  : tree ;
   location                   : location_t ;
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   GetQuadOtok (quad, combined, op,
                left, right, dest,
                overflow, constExpr,
                leftpos, rightpos, destpos) ;
   location := TokenToLocation (combined) ;
   IF IsConst(left) AND IsConst(right)
   THEN
      PushValue(left) ;
      PushValue(right) ;
      IF LessEqu(combined)
      THEN
         BuildGoto(location, string(CreateLabelName(dest)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet (left) OR (IsVar (left) AND IsSet (SkipType (GetType (left)))) OR
         IsConstSet (right) OR (IsVar (right) AND IsSet (SkipType (GetType (right))))
   THEN
      CodeIfSetLessEqu (quad, left, right, dest)
   ELSE
      IF IsComposite (GetType (left)) OR IsComposite (GetType (right))
      THEN
         MetaErrorT2 (combined,
                      'comparison tests between composite types not allowed {%1Eatd} and {%2atd}',
                      left, right)
      ELSE
         ConvertBinaryOperands (location,
                                tl, tr,
                                ComparisonMixTypes (left, right,
                                                    SkipType (GetType (left)),
                                                    SkipType (GetType (right)),
                                                    combined),
                                left, right) ;
         DoJump (location, BuildLessThanOrEqual (location, tl, tr),
                 NIL, string (CreateLabelName (dest)))
      END
   END
END PerformCodeIfLessEqu ;


(*
   CodeIfLessEqu - codes the quadruple if op1 <= op2 then goto op3
*)

PROCEDURE CodeIfLessEqu (quad: CARDINAL) ;
BEGIN
   IF IsValidExpressionRelOp (quad, FALSE)
   THEN
      PerformCodeIfLessEqu (quad)
   END
END CodeIfLessEqu ;


(*
   CodeIfSetGreEqu -
*)

PROCEDURE CodeIfSetGreEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
   location: location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError ('this should have been folded in the calling procedure')
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(CurrentQuadToken, settype), FindSize(CurrentQuadToken, Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(location,
             BuildIsSuperset(location,
                             BuildConvert(location, GetWordType(), Mod2Gcc(op1), FALSE),
                             BuildConvert(location, GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSE
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(location,
                                    Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildIsNotSuperset,
                                    falselabel) ;

      BuildGoto(location, string(CreateLabelName(op3))) ;
      DeclareLabel(location, falselabel)
   END
END CodeIfSetGreEqu ;


(*
   PerformCodeIfGreEqu - codes the quadruple if op1 >= op2 then goto op3
*)

PROCEDURE PerformCodeIfGreEqu (quad: CARDINAL) ;
VAR
   tl, tr: tree ;
   location                   : location_t ;
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   GetQuadOtok (quad, combined, op,
                left, right, dest,
                overflow, constExpr,
                leftpos, rightpos, destpos) ;
   location := TokenToLocation (combined) ;
   IF IsConst(left) AND IsConst(right)
   THEN
      PushValue(left) ;
      PushValue(right) ;
      IF GreEqu(combined)
      THEN
         BuildGoto(location, string(CreateLabelName(dest)))
      ELSE
         (* fall through *)
      END
   ELSIF IsConstSet(left) OR (IsVar(left) AND IsSet(SkipType(GetType(left)))) OR
         IsConstSet(right) OR (IsVar(right) AND IsSet(SkipType(GetType(right))))
   THEN
      CodeIfSetGreEqu(quad, left, right, dest)
   ELSE
      IF IsComposite(GetType(left)) OR IsComposite(GetType(right))
      THEN
         MetaErrorT2 (combined,
                      'comparison tests between composite types not allowed {%1Eatd} and {%2atd}',
                      left, right)
      ELSE
         ConvertBinaryOperands(location,
                               tl, tr,
                               ComparisonMixTypes (left, right,
                                                   SkipType (GetType (left)),
                                                   SkipType (GetType (right)),
                                                   combined),
                               left, right) ;
         DoJump(location, BuildGreaterThanOrEqual(location, tl, tr), NIL, string(CreateLabelName(dest)))
      END
   END
END PerformCodeIfGreEqu ;


(*
   CodeIfGreEqu - codes the quadruple if op1 >= op2 then goto op3
*)

PROCEDURE CodeIfGreEqu (quad: CARDINAL) ;
BEGIN
   IF IsValidExpressionRelOp (quad, FALSE)
   THEN
      PerformCodeIfGreEqu (quad)
   END
END CodeIfGreEqu ;


(*
   CodeIfSetEqu - codes if op1 = op2 then goto op3
                  Note that if op1 and op2 are not both constants
                  since this will have been evaluated in CodeIfEqu.
*)

PROCEDURE CodeIfSetEqu (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   settype   : CARDINAL ;
   falselabel: ADDRESS ;
   location  : location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsConst(op1) AND IsConst(op2)
   THEN
      InternalError ('this should have been folded in the calling procedure')
   ELSIF IsConst(op1)
   THEN
      settype := SkipType(GetType(op2))
   ELSE
      settype := SkipType(GetType(op1))
   END ;
   IF CompareTrees(FindSize(CurrentQuadToken, settype), FindSize(CurrentQuadToken, Word)) <= 0
   THEN
      (* word size sets *)
      DoJump(location,
             BuildEqualTo(location,
                          BuildConvert(location, GetWordType(), Mod2Gcc(op1), FALSE),
                          BuildConvert(location, GetWordType(), Mod2Gcc(op2), FALSE)),
             NIL, string(CreateLabelName(op3)))
   ELSIF GetSType(op1)=GetSType(op2)
   THEN
      falselabel := string(Sprintf1(Mark(InitString('.Lset%dcomp')), quad)) ;

      BuildForeachWordInSetDoIfExpr(location,
                                    Mod2Gcc(settype),
                                    Mod2Gcc(op1), Mod2Gcc(op2),
                                    GetMode(op1)=LeftValue,
                                    GetMode(op2)=LeftValue,
                                    IsConst(op1), IsConst(op2),
                                    BuildNotEqualTo,
                                    falselabel) ;

      BuildGoto(location, string(CreateLabelName(op3))) ;
      DeclareLabel(location, falselabel)
   ELSE
      MetaErrorT2 (CurrentQuadToken,
                   'set comparison is only allowed between the same set type, the set types used by {%1Eatd} and {%2atd} are different',
                   op1, op2)
   END
END CodeIfSetEqu ;


(*
   CodeIfSetNotEqu - codes if op1 # op2 then goto op3
                     Note that if op1 and op2 are not both constants
                     since this will have been evaluated in CodeIfNotEqu.
*)

PROCEDURE CodeIfSetNotEqu (left, right, destQuad: CARDINAL) ;
VAR
   settype  : CARDINAL ;
   truelabel: ADDRESS ;
   location  : location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   IF IsConst (left) AND IsConst (right)
   THEN
      InternalError ('this should have been folded in the calling procedure')
   ELSIF IsConst (left)
   THEN
      settype := SkipType (GetType (right))
   ELSE
      settype := SkipType (GetType (left))
   END ;
   IF CompareTrees (FindSize (CurrentQuadToken, settype), FindSize (CurrentQuadToken, Word)) <= 0
   THEN
      (* word size sets *)
      DoJump (location,
              BuildNotEqualTo(location,
                              BuildConvert (location, GetWordType (), Mod2Gcc (left), FALSE),
                              BuildConvert (location, GetWordType (), Mod2Gcc (right), FALSE)),
              NIL, string (CreateLabelName (destQuad)))
   ELSIF GetSType (left) = GetSType (right)
   THEN
      truelabel := string (CreateLabelName (destQuad)) ;

      BuildForeachWordInSetDoIfExpr (location,
                                     Mod2Gcc (settype),
                                     Mod2Gcc (left), Mod2Gcc (right),
                                     GetMode (left) = LeftValue,
                                     GetMode (right) = LeftValue,
                                     IsConst (left), IsConst (right),
                                     BuildNotEqualTo,
                                     truelabel)
   ELSE
      MetaErrorT2 (CurrentQuadToken,
                   'set comparison is only allowed between the same set type, the set types used by {%1Eatd} and {%2atd} are different',
                   left, right)
   END
END CodeIfSetNotEqu ;


(*
   ComparisonMixTypes -
*)

PROCEDURE ComparisonMixTypes (varleft, varright, left, right: CARDINAL; tokpos: CARDINAL) : CARDINAL ;
BEGIN
   IF IsGenericSystemType (left)
   THEN
      RETURN left
   ELSIF IsGenericSystemType (right)
   THEN
      RETURN right
   ELSE
      RETURN MixTypesDecl (varleft, varright, left, right, tokpos)
   END
END ComparisonMixTypes ;


(*
   PerformCodeIfEqu -
*)

PROCEDURE PerformCodeIfEqu (quad: CARDINAL) ;
VAR
   tl, tr                     : tree ;
   location                   : location_t ;
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   GetQuadOtok (quad, combined, op,
                left, right, dest,
                overflow, constExpr,
                leftpos, rightpos, destpos) ;
   location := TokenToLocation (combined) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      PushValue (left) ;
      PushValue (right) ;
      IF Equ (combined)
      THEN
         BuildGoto (location, string (CreateLabelName (dest)))
      ELSE
         (* Fall through.  *)
      END
   ELSIF IsConstSet (left) OR (IsVar (left) AND IsSet (SkipType (GetType (left)))) OR
         IsConstSet (right) OR (IsVar (right) AND IsSet (SkipType (GetType (right))))
   THEN
      CodeIfSetEqu (quad, left, right, dest)
   ELSE
      IF IsComposite (GetType (left)) OR IsComposite (GetType (right))
      THEN
         MetaErrorT2 (combined,
                      'equality tests between composite types not allowed {%1Eatd} and {%2atd}',
                      left, right)
      ELSE
         ConvertBinaryOperands (location,
                                tl, tr,
                                ComparisonMixTypes (left, right,
                                                    SkipType (GetType (left)),
                                                    SkipType (GetType (right)),
                                                    combined),
                               left, right) ;
         DoJump (location, BuildEqualTo (location, tl, tr), NIL,
                 string (CreateLabelName (dest)))
      END
   END
END PerformCodeIfEqu ;


(*
   PerformCodeIfNotEqu -
*)

PROCEDURE PerformCodeIfNotEqu (quad: CARDINAL) ;
VAR
   tl, tr                     : tree ;
   location                   : location_t ;
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   (* Ensure that any remaining undeclared constant literal is declared.  *)
   GetQuadOtok (quad, combined, op,
                left, right, dest,
                constExpr, overflow,
                leftpos, rightpos, destpos) ;
   location := TokenToLocation (combined) ;
   IF IsConst (left) AND IsConst (right)
   THEN
      PushValue (left) ;
      PushValue (right) ;
      IF NotEqu (combined)
      THEN
         BuildGoto (location, string (CreateLabelName (dest)))
      ELSE
         (* Fall through.  *)
      END
   ELSIF IsConstSet (left) OR (IsVar (left) AND IsSet (SkipType (GetType (left)))) OR
         IsConstSet (right) OR (IsVar (right) AND IsSet (SkipType (GetType (right))))
   THEN
      CodeIfSetNotEqu (left, right, dest)
   ELSE
      IF IsComposite (GetType (left)) OR IsComposite (GetType (right))
      THEN
         MetaErrorT2 (combined,
                      'inequality tests between composite types not allowed {%1Eatd} and {%2atd}',
                      left, right)
      ELSE
         ConvertBinaryOperands (location,
                                tl, tr,
                                ComparisonMixTypes (left, right,
                                                    SkipType (GetType (left)),
                                                    SkipType (GetType (right)),
                                                    combined),
                                left, right) ;
         DoJump (location, BuildNotEqualTo (location, tl, tr), NIL,
                 string (CreateLabelName (dest)))
      END
   END
END PerformCodeIfNotEqu ;


(*
   IsValidExpressionRelOp - declare left and right constants (if they are not already declared).
                            Check whether left and right are expression compatible.
*)

PROCEDURE IsValidExpressionRelOp (quad: CARDINAL; isin: BOOLEAN) : BOOLEAN ;
CONST
   Verbose = FALSE ;
VAR
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   (* Ensure that any remaining undeclared constant literal is declared.  *)
   GetQuadOtok (quad, combined, op,
                left, right, dest,
                constExpr, overflow,
                leftpos, rightpos, destpos) ;
   DeclareConstant (leftpos, left) ;
   DeclareConstant (rightpos, right) ;
   DeclareConstructor (leftpos, quad, left) ;
   DeclareConstructor (rightpos, quad, right) ;
   IF ExpressionTypeCompatible (combined, "", left, right,
                                StrictTypeChecking, isin)
   THEN
      RETURN TRUE
   ELSE
      IF Verbose
      THEN
         MetaErrorT2 (combined,
                      'expression mismatch between {%1Etad} and {%2tad} seen during comparison',
                      left, right)
      END ;
      RETURN FALSE
   END
END IsValidExpressionRelOp ;


(*
   CodeIfEqu - codes the quadruple if op1 = op2 then goto op3
*)

PROCEDURE CodeIfEqu (quad: CARDINAL) ;
BEGIN
   IF IsValidExpressionRelOp (quad, FALSE)
   THEN
      PerformCodeIfEqu (quad)
   END
END CodeIfEqu ;


(*
   CodeIfNotEqu - codes the quadruple if op1 # op2 then goto op3
*)

PROCEDURE CodeIfNotEqu (quad: CARDINAL) ;
BEGIN
   IF IsValidExpressionRelOp (quad, FALSE)
   THEN
      PerformCodeIfNotEqu (quad)
   END
END CodeIfNotEqu ;


(*
   MixTypes3 - returns a type compatible from, low, high, var.
*)

PROCEDURE MixTypes3 (low, high, var: CARDINAL; tokenno: CARDINAL) : CARDINAL ;
VAR
   type: CARDINAL ;
BEGIN
   type := MixTypes(SkipType(GetType(low)), SkipType(GetType(high)), tokenno) ;
   type := MixTypes(type, SkipType(GetType(var)), tokenno) ;
   RETURN( type )
END MixTypes3 ;


(*
   BuildIfVarInConstValue - if var in constsetvalue then goto trueexit
*)

PROCEDURE BuildIfVarInConstValue (location: location_t; tokenno: CARDINAL;
                                  constsetvalue: PtrToValue; var, trueexit: CARDINAL) ;
VAR
   vt, lt, ht  : tree ;
   type,
   low, high, n: CARDINAL ;
   truelabel   : String ;
BEGIN
   n := 1 ;
   truelabel := string(CreateLabelName(trueexit)) ;
   WHILE GetRange(constsetvalue, n, low, high) DO
      type := MixTypes3(low, high, var, tokenno) ;
      ConvertBinaryOperands(location, vt, lt, type, var, low) ;
      ConvertBinaryOperands(location, ht, lt, type, high, low) ;
      BuildIfInRangeGoto(location, vt, lt, ht, truelabel) ;
      INC(n)
   END
END BuildIfVarInConstValue ;


(*
   BuildIfNotVarInConstValue - if not (var in constsetvalue) then goto trueexit
*)

PROCEDURE BuildIfNotVarInConstValue (quad: CARDINAL; constsetvalue: PtrToValue; var, trueexit: CARDINAL) ;
VAR
   vt, lt, ht  : tree ;
   type,
   low, high, n: CARDINAL ;
   falselabel,
   truelabel   : String ;
   location    : location_t ;
BEGIN
   location := TokenToLocation(CurrentQuadToken) ;

   truelabel := string(CreateLabelName(trueexit)) ;
   n := 1 ;
   WHILE GetRange(constsetvalue, n, low, high) DO
      INC(n)
   END ;
   IF n=2
   THEN
      (* actually only one set range, so we invert it *)
      type := MixTypes3(low, high, var, CurrentQuadToken) ;
      ConvertBinaryOperands(location, vt, lt, type, var, low) ;
      ConvertBinaryOperands(location, ht, lt, type, high, low) ;
      BuildIfNotInRangeGoto(location, vt, lt, ht, truelabel)
   ELSE
      n := 1 ;
      falselabel := string(Sprintf1(Mark(InitString('.Lset%d')), quad)) ;
      WHILE GetRange(constsetvalue, n, low, high) DO
         type := MixTypes3(low, high, var, CurrentQuadToken) ;
         ConvertBinaryOperands(location, vt, lt, type, var, low) ;
         ConvertBinaryOperands(location, ht, lt, type, high, low) ;
         BuildIfInRangeGoto(location, vt, lt, ht, falselabel) ;
         INC(n)
      END ;
      BuildGoto(location, truelabel) ;
      DeclareLabel(location, falselabel)
   END
END BuildIfNotVarInConstValue ;


(*
   PerformCodeIfIn - code the quadruple: if op1 in op2 then goto op3
*)

PROCEDURE PerformCodeIfIn (quad: CARDINAL) ;
VAR
   low,
   high    : CARDINAL ;
   lowtree,
   hightree,
   offset  : tree ;
   fieldno : INTEGER ;
   location                   : location_t ;
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   (* Ensure that any remaining undeclared constant literal is declared.  *)
   GetQuadOtok (quad, combined, op,
                left, right, dest,
                constExpr, overflow,
                leftpos, rightpos, destpos) ;
   location := TokenToLocation (combined) ;
   IF IsConst(left) AND IsConst(right)
   THEN
      InternalError ('should not get to here (if we do we should consider calling FoldIfIn)')
   ELSIF CheckElementSetTypes (quad)
   THEN
      IF IsConst(left)
      THEN
         fieldno := GetFieldNo(combined, left, GetType(right), offset) ;
         IF fieldno>=0
         THEN
            PushValue(left) ;
            PushIntegerTree(offset) ;
            ConvertToType(GetType(left)) ;
            Sub ;
            BuildIfConstInVar(location,
                              Mod2Gcc(SkipType(GetType(right))),
                              Mod2Gcc(right), PopIntegerTree(),
                              GetMode(right)=LeftValue, fieldno,
                              string(CreateLabelName(dest)))
         ELSE
            MetaErrorT1 (combined, 'bit exceeded the range of set {%1Eatd}', left)
         END
      ELSIF IsConst(right)
      THEN
         (* builds a cascaded list of if statements *)
         PushValue(right) ;
         BuildIfVarInConstValue(location, combined, GetValue(combined), left, dest)
      ELSE
         GetSetLimits(SkipType(GetType(right)), low, high) ;

         PushValue(low) ;
         lowtree := PopIntegerTree() ;
         PushValue(high) ;
         hightree := PopIntegerTree() ;

         BuildIfVarInVar(location,
                         Mod2Gcc(SkipType(GetType(right))),
                         Mod2Gcc(right), Mod2Gcc(left),
                         GetMode(right)=LeftValue,
                         lowtree, hightree,
                         string(CreateLabelName(dest)))
      END
   END
END PerformCodeIfIn ;


(*
   PerformCodeIfNotIn - code the quadruple: if not (op1 in op2) then goto op3
*)

PROCEDURE PerformCodeIfNotIn (quad: CARDINAL) ;
VAR
   low,
   high    : CARDINAL ;
   lowtree,
   hightree,
   offset  : tree ;
   fieldno : INTEGER ;
   location                   : location_t ;
   left, right, dest, combined,
   leftpos, rightpos, destpos : CARDINAL ;
   constExpr, overflow        : BOOLEAN ;
   op                         : QuadOperator ;
BEGIN
   (* Ensure that any remaining undeclared constant literal is declared.  *)
   GetQuadOtok (quad, combined, op,
                left, right, dest,
                overflow, constExpr,
                leftpos, rightpos, destpos) ;
   location := TokenToLocation (combined) ;
   IF IsConst(left) AND IsConst(right)
   THEN
      InternalError ('should not get to here (if we do we should consider calling FoldIfIn)')
   ELSIF CheckElementSetTypes (quad)
   THEN
      IF IsConst(left)
      THEN
         fieldno := GetFieldNo(combined, left, SkipType(GetType(right)), offset) ;
         IF fieldno>=0
         THEN
            PushValue(left) ;
            PushIntegerTree(offset) ;
            ConvertToType(GetType(left)) ;
            Sub ;
            BuildIfNotConstInVar(location,
                                 Mod2Gcc(SkipType(GetType(right))),
                                 Mod2Gcc(right), PopIntegerTree(),
                                 GetMode(right)=LeftValue, fieldno,
                                 string(CreateLabelName(dest)))
         ELSE
            MetaErrorT1 (combined, 'bit exceeded the range of set {%1Eatd}', right)
         END
      ELSIF IsConst(right)
      THEN
         (* builds a cascaded list of if statements *)
         PushValue(right) ;
         BuildIfNotVarInConstValue(quad, GetValue(combined), left, dest)
      ELSE
         GetSetLimits(SkipType(GetType(right)), low, high) ;

         PushValue(low) ;
         lowtree := PopIntegerTree() ;
         PushValue(high) ;
         hightree := PopIntegerTree() ;

         BuildIfNotVarInVar(location,
                            Mod2Gcc(SkipType(GetType(right))),
                            Mod2Gcc(right), Mod2Gcc(left),
                            GetMode(right)=LeftValue,
                            lowtree, hightree,
                            string(CreateLabelName(dest)))
      END
   END
END PerformCodeIfNotIn ;


(*
   CodeIfIn - code the quadruple: if op1 in op2 then goto op3
*)

PROCEDURE CodeIfIn (quad: CARDINAL) ;
BEGIN
   IF IsValidExpressionRelOp (quad, TRUE)
   THEN
      PerformCodeIfIn (quad)
   END
END CodeIfIn ;


(*
   CodeIfNotIn - code the quadruple: if not (op1 in op2) then goto op3
*)

PROCEDURE CodeIfNotIn (quad: CARDINAL) ;
BEGIN
   IF IsValidExpressionRelOp (quad, TRUE)
   THEN
      PerformCodeIfNotIn (quad)
   END
END CodeIfNotIn ;


(*
------------------------------------------------------------------------------
   IndrX Operator           a = *b
------------------------------------------------------------------------------
   Sym1<X>   IndrX   Sym2<I>     Meaning     Mem[Sym1<I>] := Mem[constant]
   Sym1<X>   IndrX   Sym2<X>     Meaning     Mem[Sym1<I>] := Mem[Mem[Sym3<I>]]

   (op2 is the type of the data being indirectly copied)
*)

PROCEDURE CodeIndrX (quad: CARDINAL; op1, op2, op3: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation (CurrentQuadToken) ;

   (*
      Follow the Quadruple rules:
   *)
   DeclareConstant (CurrentQuadToken, op3) ;  (* checks to see whether it is a constant and declares it *)
   DeclareConstructor (CurrentQuadToken, quad, op3) ;
   IF IsConstString (op3)
   THEN
      InternalError ('not expecting to index through a constant string')
   ELSE
      (*
         Mem[op1] := Mem[Mem[op3]]
      *)
      BuildAssignmentStatement (location, Mod2Gcc (op1), BuildIndirect (location, Mod2Gcc (op3), Mod2Gcc (op2)))
   END
END CodeIndrX ;


(*
   CodeXIndr - operands for XIndrOp are: left type right.
                *left = right.  The second operand is the type of the data being
                indirectly copied.
*)

PROCEDURE CodeXIndr (quad: CARDINAL) ;
VAR
   constExpr,
   overflowChecking: BOOLEAN ;
   op              : QuadOperator ;
   tokenno,
   left,
   type,
   right,
   leftpos,
   rightpos,
   typepos,
   xindrpos        : CARDINAL ;
   length,
   newstr          : tree ;
   location        : location_t ;
BEGIN
   GetQuadOtok (quad, xindrpos, op, left, type, right,
                overflowChecking, constExpr,
                leftpos, typepos, rightpos) ;
   tokenno := MakeVirtualTok (xindrpos, leftpos, rightpos) ;
   location := TokenToLocation (tokenno) ;

   type := SkipType (type) ;
   DeclareConstant (rightpos, right) ;
   DeclareConstructor (rightpos, quad, right) ;
   IF IsProcType(SkipType(type))
   THEN
      BuildAssignmentStatement (location, BuildIndirect (location, Mod2Gcc (left), GetPointerType ()), Mod2Gcc (right))
   ELSIF IsConstString (right) AND (GetStringLength (rightpos, right) = 0) AND (GetMode (left) = LeftValue)
   THEN
      (*
         no need to check for type errors,
         but we handle nul string as a special case as back end
         complains if we pass through a "" and ask it to copy the
         contents.
      *)
      BuildAssignmentStatement (location,
                                BuildIndirect (location, LValueToGenericPtr (location, left), Mod2Gcc (Char)),
                                StringToChar (Mod2Gcc (right), Char, right))
   ELSIF IsConstString (right) AND (SkipTypeAndSubrange (GetType (left)) # Char)
   THEN
      IF NOT PrepareCopyString (tokenno, length, newstr, right, type)
      THEN
         MetaErrorT2 (MakeVirtualTok (xindrpos, leftpos, rightpos),
                      'string constant {%1Ea} is too large to be assigned to the array {%2ad}',
                      right, left)
      END ;
      AddStatement (location,
                    MaybeDebugBuiltinMemcpy (location,
                                             Mod2Gcc (left),
                                             BuildAddr (location, newstr, FALSE),
                                             length))
   ELSE
      BuildAssignmentStatement (location,
                                BuildIndirect (location, Mod2Gcc (left), Mod2Gcc (type)),
                                ConvertRHS (Mod2Gcc (right), type, right))
   END
END CodeXIndr ;


(*
   InitBuiltinSyms -
*)

PROCEDURE InitBuiltinSyms (tok: CARDINAL) ;
BEGIN
   IF Memset = NulSym
   THEN
      Memset := FromModuleGetSym (tok, MakeKey ('memset'), MakeDefinitionSource (tok, MakeKey ('Builtins')))
   END ;
   IF Memcpy = NulSym
   THEN
      Memcpy := FromModuleGetSym (tok, MakeKey ('memcpy'), MakeDefinitionSource (tok, MakeKey ('Builtins')))
   END ;
END InitBuiltinSyms ;


BEGIN
   Memset := NulSym ;
   Memcpy := NulSym ;
   UnboundedLabelNo := 0 ;
   CurrentQuadToken := 0 ;
   ScopeStack := InitStackWord ()
END M2GenGCC.
