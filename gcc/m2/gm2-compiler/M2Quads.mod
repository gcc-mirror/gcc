(* M2Quads.mod generates quadruples.

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

IMPLEMENTATION MODULE M2Quads ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Debug IMPORT Assert, WriteDebug ;
FROM NameKey IMPORT Name, NulName, MakeKey, GetKey, makekey, KeyToCharStar, WriteKey ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2, Sprintf3 ;
FROM M2DebugStack IMPORT DebugStack ;
FROM StrLib IMPORT StrLen ;
FROM M2Scaffold IMPORT DeclareScaffold, mainFunction, initFunction,
                       finiFunction, linkFunction, PopulateCtorArray,
                       ForeachModuleCallInit, ForeachModuleCallFinish ;

FROM M2MetaError IMPORT MetaError0, MetaError1, MetaError2, MetaError3,
                        MetaErrors1, MetaErrors2, MetaErrors3,
                        MetaErrorT0, MetaErrorT1, MetaErrorT2,
                        MetaErrorsT1, MetaErrorsT2, MetaErrorT3,
                        MetaErrorStringT0, MetaErrorStringT1,
                        MetaErrorString1, MetaErrorString2,
                        MetaErrorN1, MetaErrorN2,
                        MetaErrorNT0, MetaErrorNT1, MetaErrorNT2 ;

FROM DynamicStrings IMPORT String, string, InitString, KillString,
                           ConCat, InitStringCharStar, Dup, Mark,
                           PushAllocation, PopAllocationExemption,
                           InitStringDB, InitStringCharStarDB,
                           InitStringCharDB, MultDB, DupDB, SliceDB ;

FROM SymbolTable IMPORT ModeOfAddr, GetMode, PutMode, GetSymName, IsUnknown,
                        MakeTemporary,
                        MakeTemporaryFromExpression,
                        MakeTemporaryFromExpressions,
                        MakeConstLit,
                        MakeConstString, MakeConstant, MakeConstVar,
                        MakeConstStringM2nul, MakeConstStringCnul,
                        Make2Tuple,
                        RequestSym, MakePointer, PutPointer,
                        SkipType,
			GetDType, GetSType, GetLType,
                        GetScope, GetCurrentScope,
                        GetSubrange, SkipTypeAndSubrange,
                        GetModule, GetMainModule,
                        GetCurrentModule, GetFileModule, GetLocalSym,
                        GetStringLength, GetString,
                        GetArraySubscript, GetDimension,
                        GetParam,
                        GetNth, GetNthParam,
                        GetFirstUsed, GetDeclaredMod,
                        GetQuads, GetReadQuads, GetWriteQuads,
                        GetWriteLimitQuads, GetReadLimitQuads,
                        GetVarScope,
                        GetModuleQuads, GetProcedureQuads,
                        GetModuleCtors,
                        MakeProcedure,
                        PutConstStringKnown,
                        PutModuleStartQuad, PutModuleEndQuad,
                        PutModuleFinallyStartQuad, PutModuleFinallyEndQuad,
                        PutProcedureStartQuad, PutProcedureEndQuad,
                        PutProcedureScopeQuad,
                        PutVar, PutConstSet,
                        GetVarPointerCheck, PutVarPointerCheck,
                        PutVarWritten,
                        PutReadQuad, RemoveReadQuad,
                        PutWriteQuad, RemoveWriteQuad,
                        PutPriority, GetPriority,
                        PutProcedureBegin, PutProcedureEnd,
                        PutVarConst, IsVarConst,
                        PutConstLitInternal,
                        PutVarHeap,
                        IsVarParam, IsProcedure, IsPointer, IsParameter,
                        IsUnboundedParam, IsEnumeration, IsDefinitionForC,
                        IsVarAParam, IsVarient, IsLegal,
                        UsesVarArgs, UsesOptArg,
                        GetOptArgInit,
                        IsReturnOptional,
                        NoOfElements,
                        NoOfParam,
                        StartScope, EndScope,
                        IsGnuAsm, IsGnuAsmVolatile,
                        MakeRegInterface, PutRegInterface,
                        HasExceptionBlock, PutExceptionBlock,
                        HasExceptionFinally, PutExceptionFinally,
                        GetParent, GetRecord, IsRecordField, IsFieldVarient, IsRecord,
                        IsFieldEnumeration,
                        IsVar, IsProcType, IsType, IsSubrange, IsExported,
                        IsConst, IsConstString, IsModule, IsDefImp,
                        IsArray, IsUnbounded, IsProcedureNested,
                        IsParameterUnbounded,
                        IsPartialUnbounded, IsProcedureBuiltin,
                        IsSet, IsConstSet, IsConstructor, PutConst,
                        PutConstructor, PutConstructorFrom,
                        PutDeclared,
                        MakeComponentRecord, MakeComponentRef,
                        IsSubscript, IsComponent, IsConstStringKnown,
                        IsTemporary,
                        IsAModula2Type,
                        PutLeftValueFrontBackType,
                        PushSize, PushValue, PopValue,
                        GetVariableAtAddress, IsVariableAtAddress,
                        MakeError, UnknownReported,
                        IsProcedureBuiltinAvailable,
                        IsError,
                        IsInnerModule,
                        IsImportStatement, IsImport, GetImportModule, GetImportDeclared,
                        GetImportStatementList,
                        GetModuleDefImportStatementList, GetModuleModImportStatementList,
                        IsCtor, IsPublic, IsExtern, IsMonoName,

                        GetUnboundedRecordType,
                        GetUnboundedAddressOffset,
                        GetUnboundedHighOffset,
                        PutVarArrayRef,

                        ForeachFieldEnumerationDo, ForeachLocalSymDo,
                        GetExported, PutImported, GetSym, GetLibName,
                        GetTypeMode,
                        IsVarConditional, PutVarConditional,
                        IsUnused,
                        NulSym ;

FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2GCCDeclare IMPORT PutToBeSolvedByQuads ;

FROM FifoQueue IMPORT GetConstFromFifoQueue,
                      PutConstructorIntoFifoQueue, GetConstructorFromFifoQueue ;

FROM M2Comp IMPORT CompilingImplementationModule,
                   CompilingProgramModule ;

FROM M2LexBuf IMPORT currenttoken, UnknownTokenNo, BuiltinTokenNo,
                     GetToken, MakeVirtualTok, MakeVirtual2Tok,
                     GetFileName, TokenToLineNo, GetTokenName,
                     GetTokenNo, GetLineNo, GetPreviousTokenLineNo, PrintTokenNo ;

FROM M2Error IMPORT Error,
                    InternalError,
                    WriteFormat0, WriteFormat1, WriteFormat2, WriteFormat3,
                    NewError, NewWarning, ErrorFormat0, ErrorFormat1,
                    ErrorFormat2, ErrorFormat3, FlushErrors, ChainError,
                    ErrorString,
                    ErrorStringAt, ErrorStringAt2, ErrorStringsAt2,
                    WarnStringAt, WarnStringAt2, WarnStringsAt2 ;

FROM M2Printf IMPORT fprintf0, fprintf1, fprintf2, fprintf3, fprintf4,
                     printf0, printf1, printf2, printf3, printf4 ;

FROM M2Reserved IMPORT PlusTok, MinusTok, TimesTok, DivTok, ModTok,
                       DivideTok, RemTok,
                       OrTok, AndTok, AmbersandTok,
                       EqualTok, LessEqualTok, GreaterEqualTok,
                       LessTok, GreaterTok, HashTok, LessGreaterTok,
                       InTok,
                       UpArrowTok, RParaTok, LParaTok, CommaTok,
                       NulTok, ByTok,
                       SemiColonTok, toktype ;

FROM M2Base IMPORT True, False, Boolean, Cardinal, Integer, Char,
                   Real, LongReal, ShortReal, Nil,
                   ZType, RType, CType,
                   Re, Im, Cmplx,
                   NegateType, ComplexToScalar, GetCmplxReturnType,
                   IsAssignmentCompatible, IsExpressionCompatible,
                   AssignmentRequiresWarning,
                   CannotCheckTypeInPass3, ScalarToComplex, MixTypes,
                   CheckAssignmentCompatible, CheckExpressionCompatible,
                   High, LengthS, New, Dispose, Inc, Dec, Incl, Excl,
                   Cap, Abs, Odd,
                   IsOrd, Chr, Convert, Val, IsFloat, IsTrunc,
                   IsInt, Min, Max,
                   IsPseudoBaseProcedure, IsPseudoBaseFunction,
                   IsMathType, IsOrdinalType, IsRealType,
                   IsBaseType, GetBaseTypeMinMax, ActivationPointer ;

FROM M2System IMPORT IsPseudoSystemFunction, IsPseudoSystemProcedure,
                     IsSystemType, GetSystemTypeMinMax,
                     IsPseudoSystemFunctionConstExpression,
                     IsGenericSystemType,
                     Adr, TSize, TBitSize, AddAdr, SubAdr, DifAdr, Cast,
                     Shift, Rotate, MakeAdr, Address, Byte, Word, Loc, Throw ;

FROM M2Size IMPORT Size ;
FROM M2Bitset IMPORT Bitset ;

FROM M2ALU IMPORT PushInt, Gre, Less, PushNulSet, AddBitRange, AddBit,
                  IsGenericNulSet, IsValueAndTreeKnown, AddField,
                  AddElements, ChangeToConstructor ;

FROM Lists IMPORT List, InitList, GetItemFromList, NoOfItemsInList, PutItemIntoList,
                  IsItemInList, KillList, IncludeItemIntoList ;

FROM M2Options IMPORT NilChecking,
                      WholeDivChecking, WholeValueChecking,
                      IndexChecking, RangeChecking,
                      CaseElseChecking, ReturnChecking,
                      UnusedVariableChecking, UnusedParameterChecking,
                      Iso, Pim, Pim2, Pim3, Pim4, PositiveModFloorDiv,
                      Pedantic, CompilerDebugging, GenerateDebugging,
                      GenerateLineDebug, Exceptions,
                      Profiling, Coding, Optimizing,
                      UninitVariableChecking,
                      ScaffoldDynamic, ScaffoldStatic, cflag,
                      ScaffoldMain, SharedFlag, WholeProgram,
                      GetDumpDir, GetM2DumpFilter,
                      GetRuntimeModuleOverride, GetDebugTraceQuad,
                      GetDumpQuad ;

FROM M2LangDump IMPORT CreateDumpQuad, CloseDumpQuad, GetDumpFile ;
FROM M2Pass IMPORT IsPassCodeGeneration, IsNoPass ;

FROM M2StackAddress IMPORT StackOfAddress, InitStackAddress, KillStackAddress,
                           PushAddress, PopAddress, PeepAddress,
                           IsEmptyAddress, NoOfItemsInStackAddress ;

FROM M2StackWord IMPORT StackOfWord, InitStackWord, KillStackWord,
                        PushWord, PopWord, PeepWord, RemoveTop,
                        IsEmptyWord, NoOfItemsInStackWord ;

FROM Indexing IMPORT Index, InitIndex, GetIndice, PutIndice, InBounds, HighIndice,
                     IncludeIndiceIntoIndex, InitIndexTuned ;

FROM M2Range IMPORT InitAssignmentRangeCheck,
                    InitReturnRangeCheck,
                    InitSubrangeRangeCheck,
                    InitStaticArraySubscriptRangeCheck,
                    InitDynamicArraySubscriptRangeCheck,
                    InitIncRangeCheck,
                    InitDecRangeCheck,
                    InitInclCheck,
                    InitExclCheck,
                    InitRotateCheck,
                    InitShiftCheck,
                    InitTypesAssignmentCheck,
                    InitTypesExpressionCheck,
                    InitTypesParameterCheck,
                    InitForLoopBeginRangeCheck,
                    InitForLoopToRangeCheck,
                    InitForLoopEndRangeCheck,
                    InitPointerRangeCheck,
                    InitNoReturnRangeCheck,
                    InitNoElseRangeCheck,
                    InitCaseBounds,
                    InitWholeZeroDivisionCheck,
                    InitWholeZeroRemainderCheck,
                    InitParameterRangeCheck,
                    PutRangeForIncrement,
                    WriteRangeCheck ;

FROM M2CaseList IMPORT PushCase, PopCase, AddRange, BeginCaseList, EndCaseList, ElseCase ;
FROM PCSymBuild IMPORT SkipConst ;
FROM m2builtins IMPORT GetBuiltinTypeInfoType ;
FROM M2LangDump IMPORT IsDumpRequired ;

IMPORT M2Error, FIO, SFIO, DynamicStrings, StdIO ;


CONST
   DebugStackOn = TRUE ;
   DebugVarients = FALSE ;
   BreakAtQuad = 200 ;
   DebugTokPos = FALSE ;

TYPE
   ConstructorFrame = POINTER TO RECORD
                                    type : CARDINAL ;
                                    index: CARDINAL ;
                                 END ;

   BoolFrame = POINTER TO RECORD
                             TrueExit  : CARDINAL ;
                             FalseExit : CARDINAL ;
                             Unbounded : CARDINAL ;
                             BooleanOp : BOOLEAN ;
                             Dimension : CARDINAL ;
                             ReadWrite : CARDINAL ;
                             name      : CARDINAL ;
                             Annotation: String ;
                             tokenno   : CARDINAL ;
                          END ;

   QuadFrame = POINTER TO RECORD
                             Operator           : QuadOperator ;
                             Operand1           : CARDINAL ;
                             Operand2           : CARDINAL ;
                             Operand3           : CARDINAL ;
                             Trash              : CARDINAL ;
                             Next               : CARDINAL ;     (* Next quadruple.                 *)
                             LineNo             : CARDINAL ;     (* Line No of source text.         *)
                             TokenNo            : CARDINAL ;     (* Token No of source text.        *)
                             NoOfTimesReferenced: CARDINAL ;     (* No of times quad is referenced. *)
                             ConstExpr,                          (* Must backend resolve this at    *)
                                                                 (* compile time?  *)
                             CheckType,
                             CheckOverflow      : BOOLEAN ;      (* should backend check overflow   *)
                             op1pos,
                             op2pos,
                             op3pos             : CARDINAL ;     (* Token position of operands.     *)
                          END ;

   WithFrame = POINTER TO RECORD
                             RecordSym   : CARDINAL ;
                             RecordType  : CARDINAL ;
                             RecordRef   : CARDINAL ;
                             rw          : CARDINAL ;          (* The record variable.  *)
                             RecordTokPos: CARDINAL ;          (* Token of the record.  *)
                          END ;

   ForLoopInfo = POINTER TO RECORD
                               IncrementQuad,
                               StartOfForLoop,                 (* We keep a list of all for         *)
                               EndOfForLoop,                   (* loops so we can check index.      *)
                               ForLoopIndex,
                               IndexTok      : CARDINAL ;      (* Used to ensure iterators are not  *)
                                                               (* user modified.                    *)
                            END ;

   LineNote  = POINTER TO RECORD
                             Line: CARDINAL ;
                             File: Name ;
                             Next: LineNote ;
                          END ;
VAR
   ConstructorStack,
   LineStack,
   BoolStack,
   WithStack            : StackOfAddress ;
   TryStack,
   CatchStack,
   ExceptStack,
   ConstExprStack,
   ConstParamStack,
   AutoStack,
   RepeatStack,
   WhileStack,
   ForStack,
   ExitStack,
   ReturnStack          : StackOfWord ;   (* Return quadruple of the procedure.  *)
   PriorityStack        : StackOfWord ;   (* Temporary variable holding old      *)
                                          (* priority.                           *)
   SuppressWith         : BOOLEAN ;
   QuadArray            : Index ;
   NextQuad             : CARDINAL ;  (* Next quadruple number to be created.    *)
   FreeList             : CARDINAL ;  (* FreeList of quadruples.                 *)
   CurrentProc          : CARDINAL ;  (* Current procedure being compiled, used  *)
                                      (* to determine which procedure a RETURN.  *)
                                      (* ReturnValueOp must have as its 3rd op.  *)
   InitQuad             : CARDINAL ;  (* Initial Quad BackPatch that starts the  *)
                                      (* suit of Modules.                        *)
   LastQuadNo           : CARDINAL ;  (* Last Quadruple accessed by GetQuad.     *)
   ArithPlusTok,                      (* Internal + token for arithmetic only.   *)
   LogicalOrTok,                      (* Internal _LOR token.                    *)
   LogicalAndTok,                     (* Internal _LAND token.                   *)
   LogicalXorTok,                     (* Internal _LXOR token.                   *)
   LogicalDifferenceTok : Name ;      (* Internal _LDIFF token.                  *)
   InConstExpression,
   InConstParameters,
   IsAutoOn,                          (* Should parser automatically push        *)
                                      (* idents?                                 *)
   MustNotCheckBounds   : BOOLEAN ;
   ForInfo              : Index ;     (* Start and end of all FOR loops.         *)
   GrowInitialization   : CARDINAL ;  (* Upper limit of where the initialized    *)
                                      (* quadruples.                             *)
   BuildingHigh,
   BuildingSize,
   QuadrupleGeneration  : BOOLEAN ;      (* Should we be generating quadruples?  *)
   FreeLineList         : LineNote ;  (* Free list of line notes.                *)
   VarientFields        : List ;      (* The list of all varient fields created. *)
   VarientFieldNo       : CARDINAL ;  (* Used to retrieve the VarientFields      *)
                                      (* in order.                               *)
   NoOfQuads            : CARDINAL ;  (* Number of used quadruples.              *)
   Head                 : CARDINAL ;  (* Head of the list of quadruples.         *)


(*
   Rules for file and initialization quadruples:

   StartModFileOp  - indicates that this file (module) has produced the
                     following code
   StartDefFileOp  - indicates that this definition module has produced
                     this code.
   EndFileOp       - indicates that a module has finished
   InitStartOp     - the start of the initialization code of a module
   InitEndOp       - the end of the above
   FinallyStartOp  - the start of the finalization code of a module
   FinallyEndOp    - the end of the above
*)


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

(*
PROCEDURE doDSdbEnter ;
BEGIN
   PushAllocation
END doDSdbEnter ;
*)

(*
   doDSdbExit -
*)

(*
PROCEDURE doDSdbExit (s: String) ;
BEGIN
   s := PopAllocationExemption(TRUE, s)
END doDSdbExit ;
*)

(*
   DSdbEnter -
*)

PROCEDURE DSdbEnter ;
BEGIN
END DSdbEnter ;


(*
   DSdbExit -
*)

PROCEDURE DSdbExit ;
BEGIN
END DSdbExit ;


(*
#define DBsbEnter doDBsbEnter
#define DBsbExit  doDBsbExit
*)


(*
   SetOptionProfiling - builds a profile quadruple if the profiling
                        option was given to the compiler.
*)

PROCEDURE SetOptionProfiling (b: BOOLEAN) ;
BEGIN
   IF b#Profiling
   THEN
      IF b
      THEN
         BuildProfileOn
      ELSE
         BuildProfileOff
      END ;
      Profiling := b
   END
END SetOptionProfiling ;


(*
   SetOptionCoding - builds a code quadruple if the profiling
                     option was given to the compiler.
*)

PROCEDURE SetOptionCoding (b: BOOLEAN) ;
BEGIN
   IF b#Coding
   THEN
      IF b
      THEN
         BuildCodeOn
      ELSE
         BuildCodeOff
      END ;
      Coding := b
   END
END SetOptionCoding ;


(*
   SetOptionOptimizing - builds a quadruple to say that the optimization option
                         has been found in a comment.
*)

PROCEDURE SetOptionOptimizing (b: BOOLEAN) ;
BEGIN
   IF b
   THEN
      BuildOptimizeOn
   ELSE
      BuildOptimizeOff
   END
END SetOptionOptimizing ;


(*
   GetQF - returns the QuadFrame associated with, q.
*)

PROCEDURE GetQF (q: CARDINAL) : QuadFrame ;
BEGIN
   RETURN QuadFrame (GetIndice (QuadArray, q))
END GetQF ;


(*
   Opposite - returns the opposite comparison operator.
*)

PROCEDURE Opposite (Operator: QuadOperator) : QuadOperator ;
VAR
   Op: QuadOperator ;
BEGIN
   CASE Operator OF

   IfNotEquOp : Op := IfEquOp |
   IfEquOp    : Op := IfNotEquOp |
   IfLessEquOp: Op := IfGreOp |
   IfGreOp    : Op := IfLessEquOp |
   IfGreEquOp : Op := IfLessOp |
   IfLessOp   : Op := IfGreEquOp |
   IfInOp     : Op := IfNotInOp |
   IfNotInOp  : Op := IfInOp

   ELSE
      InternalError ('unexpected operator')
   END ;
   RETURN Op
END Opposite ;


(*
   IsReferenced - returns true if QuadNo is referenced by another quadruple.
*)

PROCEDURE IsReferenced (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      RETURN( (Operator=ProcedureScopeOp) OR (Operator=NewLocalVarOp) OR
              (NoOfTimesReferenced>0) )
   END
END IsReferenced ;


(*
   IsBackReference - returns TRUE if quadruple, q, is referenced from a quad further on.
*)

PROCEDURE IsBackReference (q: CARDINAL) : BOOLEAN ;
VAR
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   i := q ;
   WHILE i#0 DO
      GetQuad (i, op, op1, op2, op3) ;
      CASE op OF

      NewLocalVarOp,
      KillLocalVarOp,
      FinallyStartOp,
      FinallyEndOp,
      InitEndOp,
      InitStartOp,
      EndFileOp,
      StartDefFileOp,
      StartModFileOp:  RETURN( FALSE ) |       (* run into end of procedure or module *)

      GotoOp,
      IfEquOp,
      IfLessEquOp,
      IfGreEquOp,
      IfGreOp,
      IfLessOp,
      IfNotEquOp,
      IfInOp,
      IfNotInOp     :  IF op3=q
                       THEN
                          RETURN( TRUE )
                       END

      ELSE
      END ;
      i := GetNextQuad (i)
   END ;
   InternalError ('fix this for the sake of efficiency..')
END IsBackReference ;


(*
   IsUnConditional - returns true if QuadNo is an unconditional jump.
*)

PROCEDURE IsUnConditional (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      CASE Operator OF

      ThrowOp,
      RetryOp,
      CallOp,
      ReturnOp,
      GotoOp  : RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END
   END
END IsUnConditional ;


(*
   IsConditional - returns true if QuadNo is a conditional jump.
*)

PROCEDURE IsConditional (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      CASE Operator OF

      IfInOp,
      IfNotInOp,
      IfEquOp,
      IfNotEquOp,
      IfLessOp,
      IfLessEquOp,
      IfGreOp,
      IfGreEquOp : RETURN( TRUE )

      ELSE
         RETURN( FALSE )
      END ;
   END
END IsConditional ;


(*
   IsBackReferenceConditional - returns TRUE if quadruple, q, is referenced from
                                a conditional quad further on.
*)

PROCEDURE IsBackReferenceConditional (q: CARDINAL) : BOOLEAN ;
VAR
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   i := q ;
   WHILE i#0 DO
      GetQuad (i, op, op1, op2, op3) ;
      CASE op OF

      NewLocalVarOp,
      KillLocalVarOp,
      FinallyStartOp,
      FinallyEndOp,
      InitEndOp,
      InitStartOp,
      EndFileOp,
      StartDefFileOp,
      StartModFileOp:  RETURN( FALSE ) |       (* run into end of procedure or module *)

      TryOp,
      RetryOp,
      GotoOp,
      IfEquOp,
      IfLessEquOp,
      IfGreEquOp,
      IfGreOp,
      IfLessOp,
      IfNotEquOp,
      IfInOp,
      IfNotInOp     :  IF (op3=q) AND IsConditional(q)
                       THEN
                          RETURN( TRUE )
                       END

      ELSE
         RETURN FALSE
      END ;
      i := GetNextQuad (i)
   END ;
   InternalError ('fix this for the sake of efficiency..')
END IsBackReferenceConditional ;


(*
   IsQuadA - returns true if QuadNo is a op.
*)

PROCEDURE IsQuadA (QuadNo: CARDINAL; op: QuadOperator) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      RETURN( Operator=op )
   END
END IsQuadA ;


(*
   IsGoto - returns true if QuadNo is a goto operation.
*)

PROCEDURE IsGoto (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA (QuadNo, GotoOp) )
END IsGoto ;


(*
   IsCall - returns true if QuadNo is a call operation.
*)

PROCEDURE IsCall (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, CallOp) )
END IsCall ;


(*
   IsReturn - returns true if QuadNo is a return operation.
*)

PROCEDURE IsReturn (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, ReturnOp) )
END IsReturn ;


(*
   IsNewLocalVar - returns true if QuadNo is a NewLocalVar operation.
*)

PROCEDURE IsNewLocalVar (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, NewLocalVarOp) )
END IsNewLocalVar ;


(*
   IsKillLocalVar - returns true if QuadNo is a KillLocalVar operation.
*)

PROCEDURE IsKillLocalVar (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, KillLocalVarOp) )
END IsKillLocalVar ;


(*
   IsProcedureScope - returns true if QuadNo is a ProcedureScope operation.
*)

PROCEDURE IsProcedureScope (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, ProcedureScopeOp) )
END IsProcedureScope ;


(*
   IsCatchBegin - returns true if QuadNo is a catch begin quad.
*)

PROCEDURE IsCatchBegin (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, CatchBeginOp) )
END IsCatchBegin ;


(*
   IsCatchEnd - returns true if QuadNo is a catch end quad.
*)

PROCEDURE IsCatchEnd (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, CatchEndOp) )
END IsCatchEnd ;


(*
   IsInitStart - returns true if QuadNo is a init start quad.
*)

PROCEDURE IsInitStart (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, InitStartOp) )
END IsInitStart ;


(*
   IsInitEnd - returns true if QuadNo is a init end quad.
*)

PROCEDURE IsInitEnd (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, InitEndOp) )
END IsInitEnd ;


(*
   IsFinallyStart - returns true if QuadNo is a finally start quad.
*)

PROCEDURE IsFinallyStart (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, FinallyStartOp) )
END IsFinallyStart ;


(*
   IsFinallyEnd - returns true if QuadNo is a finally end quad.
*)

PROCEDURE IsFinallyEnd (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsQuadA(QuadNo, FinallyEndOp) )
END IsFinallyEnd ;


(*
   IsBecomes - return TRUE if QuadNo is a BecomesOp.
*)

PROCEDURE IsBecomes (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsQuadA (QuadNo, BecomesOp)
END IsBecomes ;


(*
   IsDummy - return TRUE if QuadNo is a DummyOp.
*)

PROCEDURE IsDummy (QuadNo: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsQuadA (QuadNo, DummyOp)
END IsDummy ;


(*
   IsQuadConstExpr - returns TRUE if QuadNo is part of a constant expression.
*)

PROCEDURE IsQuadConstExpr (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (QuadNo) ;
   RETURN f^.ConstExpr
END IsQuadConstExpr ;


(*
   SetQuadConstExpr - sets the constexpr field to value.
*)

PROCEDURE SetQuadConstExpr (QuadNo: CARDINAL; value: BOOLEAN) ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (QuadNo) ;
   f^.ConstExpr := value
END SetQuadConstExpr ;


(*
   GetQuadDest - returns the jump destination associated with quad.
*)

PROCEDURE GetQuadDest (QuadNo: CARDINAL) : CARDINAL ;
BEGIN
   RETURN GetQuadOp3 (QuadNo)
END GetQuadDest ;


(*
   GetQuadOp1 - returns the 1st operand associated with quad.
*)

PROCEDURE GetQuadOp1 (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (QuadNo) ;
   RETURN f^.Operand1
END GetQuadOp1 ;


(*
   GetQuadOp2 - returns the 2nd operand associated with quad.
*)

PROCEDURE GetQuadOp2 (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (QuadNo) ;
   RETURN f^.Operand2
END GetQuadOp2 ;


(*
   GetQuadOp3 - returns the 3rd operand associated with quad.
*)

PROCEDURE GetQuadOp3 (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (QuadNo) ;
   RETURN f^.Operand3
END GetQuadOp3 ;


(*
   IsInitialisingConst - returns TRUE if the quadruple is setting
                         a const (op1) with a value.
*)

PROCEDURE IsInitialisingConst (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   GetQuad (QuadNo, op, op1, op2, op3) ;
   RETURN OpUsesOp1 (op) AND IsConst (op1)
END IsInitialisingConst ;


(*
   IsConstQuad - return TRUE if the quadruple is marked as a constexpr.
*)

PROCEDURE IsConstQuad (quad: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (quad) ;
   RETURN f^.ConstExpr
END IsConstQuad ;


(*
   OpUsesOp1 - return TRUE if op allows op1.
*)

PROCEDURE OpUsesOp1 (op: QuadOperator) : BOOLEAN ;
BEGIN
   CASE op OF

   StringConvertCnulOp,
   StringConvertM2nulOp,
   StringLengthOp,
   InclOp,
   ExclOp,
   UnboundedOp,
   FunctValueOp,
   NegateOp,
   BecomesOp,
   HighOp,
   SizeOp,
   AddrOp,
   RecordFieldOp,
   ArrayOp,
   LogicalShiftOp,
   LogicalRotateOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   CoerceOp,
   ConvertOp,
   CastOp,
   AddOp,
   SubOp,
   MultOp,
   ModFloorOp,
   DivCeilOp,
   ModCeilOp,
   DivFloorOp,
   ModTruncOp,
   DivTruncOp,
   DivM2Op,
   ModM2Op,
   XIndrOp,
   IndrXOp,
   SaveExceptionOp,
   RestoreExceptionOp:  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END OpUsesOp1 ;


(*
   IsConditionalBooleanQuad - return TRUE if operand 1 is a boolean result.
*)

PROCEDURE IsConditionalBooleanQuad (quad: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (quad) ;
   RETURN OpUsesOp1 (f^.Operator) AND
          (IsVar (f^.Operand1) OR IsConst (f^.Operand1)) AND
          IsVarConditional (f^.Operand1)
END IsConditionalBooleanQuad ;


(*
   IsOptimizeOn - returns true if the Optimize flag was true at QuadNo.
*)

PROCEDURE IsOptimizeOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f : QuadFrame ;
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Optimizing ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      f := GetQF(q) ;
      WITH f^ DO
         IF Operator=OptimizeOnOp
         THEN
            On := TRUE
         ELSIF Operator=OptimizeOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsOptimizeOn ;


(*
   IsProfileOn - returns true if the Profile flag was true at QuadNo.
*)

PROCEDURE IsProfileOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f : QuadFrame ;
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Profiling ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      f := GetQF(q) ;
      WITH f^ DO
         IF Operator=ProfileOnOp
         THEN
            On := TRUE
         ELSIF Operator=ProfileOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsProfileOn ;


(*
   IsCodeOn - returns true if the Code flag was true at QuadNo.
*)

PROCEDURE IsCodeOn (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f : QuadFrame ;
   n,
   q : CARDINAL ;
   On: BOOLEAN ;
BEGIN
   On := Coding ;
   q := Head ;
   WHILE (q#0) AND (q#QuadNo) DO
      f := GetQF(q) ;
      WITH f^ DO
         IF Operator=CodeOnOp
         THEN
            On := TRUE
         ELSIF Operator=CodeOffOp
         THEN
            On := FALSE
         END ;
         n := Next
      END ;
      q := n
   END ;
   RETURN( On )
END IsCodeOn ;


(*
   IsDefOrModFile - returns TRUE if QuadNo is a start of Module or Def file
                    directive.
*)

PROCEDURE IsDefOrModFile (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      RETURN( (Operator=StartDefFileOp) OR (Operator=StartModFileOp) )
   END
END IsDefOrModFile ;


(*
   IsPseudoQuad - returns true if QuadNo is a compiler directive.
                  ie code, profile and optimize.
                     StartFile, EndFile,
*)

PROCEDURE IsPseudoQuad (QuadNo: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      RETURN( (Operator=CodeOnOp) OR (Operator=CodeOffOp) OR
              (Operator=ProfileOnOp) OR (Operator=ProfileOffOp) OR
              (Operator=OptimizeOnOp) OR (Operator=OptimizeOffOp) OR
              (Operator=EndFileOp) OR
              (Operator=StartDefFileOp) OR (Operator=StartModFileOp)
            )
   END
END IsPseudoQuad ;


(*
   GetLastFileQuad - returns the Quadruple number of the last StartDefFile or
                     StartModFile quadruple.
*)

PROCEDURE GetLastFileQuad (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f       : QuadFrame ;
   q, i,
   FileQuad: CARDINAL ;
BEGIN
   q := Head ;
   FileQuad := 0 ;
   REPEAT
      f := GetQF(q) ;
      WITH f^ DO
         IF (Operator=StartModFileOp) OR (Operator=StartDefFileOp)
         THEN
            FileQuad := q
         END ;
         i := Next
      END ;
      q := i
   UNTIL (i=QuadNo) OR (i=0) ;
   Assert(i#0) ;
   Assert(FileQuad#0) ;
   RETURN( FileQuad )
END GetLastFileQuad ;


(*
   GetLastQuadNo - returns the last quadruple number referenced
                   by a GetQuad.
*)

PROCEDURE GetLastQuadNo () : CARDINAL ;
BEGIN
   RETURN( LastQuadNo )
END GetLastQuadNo ;


(*
   QuadToLineNo - Converts a QuadNo into the approprate line number of the
                  source file, the line number is returned.

                  This may be used to yield an idea where abouts in the
                  source file the code generetion is
                  processing.
*)

PROCEDURE QuadToLineNo (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   IF ((LastQuadNo=0) AND (NOT IsNoPass()) AND (NOT IsPassCodeGeneration())) OR
      (NOT InBounds(QuadArray, QuadNo))
   THEN
      RETURN( 0 )
   ELSE
      f := GetQF(QuadNo) ;
      RETURN( f^.LineNo )
   END
END QuadToLineNo ;


(*
   QuadToTokenNo - Converts a QuadNo into the approprate token number of the
                   source file, the line number is returned.

                   This may be used to yield an idea where abouts in the
                   source file the code generetion is
                   processing.
*)

PROCEDURE QuadToTokenNo (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   IF ((LastQuadNo=0) AND (NOT IsNoPass()) AND (NOT IsPassCodeGeneration())) OR
      (NOT InBounds(QuadArray, QuadNo))
   THEN
      RETURN( 0 )
   ELSE
      f := GetQF(QuadNo) ;
      RETURN( f^.TokenNo )
   END
END QuadToTokenNo ;


(*
   GetQuad - returns the Quadruple QuadNo.
*)

PROCEDURE GetQuad (QuadNo: CARDINAL;
                   VAR Op: QuadOperator;
                   VAR Oper1, Oper2, Oper3: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   LastQuadNo := QuadNo ;
   WITH f^ DO
      Op := Operator ;
      Oper1 := Operand1 ;
      Oper2 := Operand2 ;
      Oper3 := Operand3
   END
END GetQuad ;


(*
   GetQuadtok - returns the Quadruple QuadNo.
*)

PROCEDURE GetQuadtok (QuadNo: CARDINAL;
                      VAR Op: QuadOperator;
                      VAR Oper1, Oper2, Oper3: CARDINAL;
                      VAR Op1Pos, Op2Pos, Op3Pos: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (QuadNo) ;
   LastQuadNo := QuadNo ;
   WITH f^ DO
      Op := Operator ;
      Oper1 := Operand1 ;
      Oper2 := Operand2 ;
      Oper3 := Operand3 ;
      Op1Pos := op1pos ;
      Op2Pos := op2pos ;
      Op3Pos := op3pos
   END
END GetQuadtok ;


(*
   GetQuadOtok - returns the Quadruple QuadNo.
*)

PROCEDURE GetQuadOtok (QuadNo: CARDINAL;
                       VAR tok: CARDINAL;
                       VAR Op: QuadOperator;
                       VAR Oper1, Oper2, Oper3: CARDINAL;
                       VAR overflowChecking, constExpr: BOOLEAN ;
                       VAR Op1Pos, Op2Pos, Op3Pos: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (QuadNo) ;
   LastQuadNo := QuadNo ;
   WITH f^ DO
      Op := Operator ;
      Oper1 := Operand1 ;
      Oper2 := Operand2 ;
      Oper3 := Operand3 ;
      Op1Pos := op1pos ;
      Op2Pos := op2pos ;
      Op3Pos := op3pos ;
      tok := TokenNo ;
      overflowChecking := CheckOverflow ;
      constExpr := ConstExpr
   END
END GetQuadOtok ;


(*
   PutQuadOtok - alters a quadruple QuadNo with Op, Oper1, Oper2, Oper3, and
                 sets a boolean to determinine whether overflow should be checked.
*)

PROCEDURE PutQuadOtok (QuadNo: CARDINAL;
                       tok: CARDINAL;
                       Op: QuadOperator;
                       Oper1, Oper2, Oper3: CARDINAL;
                       overflowChecking, constExpr: BOOLEAN ;
                       Op1Pos, Op2Pos, Op3Pos: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   IF QuadNo = BreakAtQuad
   THEN
      stop
   END ;
   IF QuadrupleGeneration
   THEN
      EraseQuad (QuadNo) ;
      AddQuadInformation (QuadNo, Op, Oper1, Oper2, Oper3) ;
      f := GetQF (QuadNo) ;
      WITH f^ DO
         Operator      := Op ;
         Operand1      := Oper1 ;
         Operand2      := Oper2 ;
         Operand3      := Oper3 ;
         CheckOverflow := overflowChecking ;
         op1pos        := Op1Pos ;
         op2pos        := Op2Pos ;
         op3pos        := Op3Pos ;
         TokenNo       := tok ;
         ConstExpr     := constExpr
      END
   END
END PutQuadOtok ;


(*
   AddQuadInformation - adds variable analysis and jump analysis to the new quadruple.
*)

PROCEDURE AddQuadInformation (QuadNo: CARDINAL;
                              Op: QuadOperator;
                              Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   CASE Op OF

   IfInOp,
   IfNotInOp,
   IfEquOp,
   IfNotEquOp,
   IfLessOp,
   IfLessEquOp,
   IfGreOp,
   IfGreEquOp : ManipulateReference(QuadNo, Oper3) ;
                CheckAddVariableRead(Oper1, FALSE, QuadNo) ;
                CheckAddVariableRead(Oper2, FALSE, QuadNo) |

   TryOp,
   RetryOp,
   GotoOp     : ManipulateReference(QuadNo, Oper3) |

   (* variable references *)

   InclOp,
   ExclOp            : CheckConst(Oper1) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) ;
                       CheckAddVariableWrite(Oper1, TRUE, QuadNo) |
   UnboundedOp,
   FunctValueOp,
   NegateOp,
   BecomesOp,
   HighOp,
   SizeOp            : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) |
   AddrOp            : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) ;
                       (* CheckAddVariableReadLeftValue(Oper3, QuadNo) *)
                       (* the next line is a kludge and assumes we _will_
                          write to the variable as we have taken its address *)
                       CheckRemoveVariableWrite(Oper1, TRUE, QuadNo) |
   ReturnValueOp     : CheckAddVariableRead(Oper1, FALSE, QuadNo) |
   ReturnOp,
   NewLocalVarOp,
   KillLocalVarOp    : |
   CallOp            : CheckAddVariableRead(Oper3, TRUE, QuadNo) |

   ParamOp           : CheckAddVariableRead(Oper2, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) ;
                       IF (Oper1>0) AND (Oper1<=NoOfParam(Oper2)) AND
                          IsVarParam(Oper2, Oper1)
                       THEN
                          (* _may_ also write to a var parameter, although we dont know *)
                          CheckAddVariableWrite(Oper3, TRUE, QuadNo)
                       END |
   RecordFieldOp,
   ArrayOp,
   LogicalShiftOp,
   LogicalRotateOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   CoerceOp,
   ConvertOp,
   CastOp,
   AddOp,
   SubOp,
   MultOp,
   DivM2Op,
   ModM2Op,
   ModFloorOp,
   DivCeilOp,
   ModCeilOp,
   DivFloorOp,
   ModTruncOp,
   DivTruncOp        : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper2, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) |

   XIndrOp           : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, TRUE, QuadNo) ;
                       CheckAddVariableRead(Oper3, FALSE, QuadNo) |

   IndrXOp           : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckAddVariableRead(Oper3, TRUE, QuadNo) |

(* RangeCheckOp      : CheckRangeAddVariableRead(Oper3, QuadNo) | *)
   SaveExceptionOp   : CheckConst(Oper1) ;
                       CheckAddVariableWrite(Oper1, FALSE, QuadNo) |
   RestoreExceptionOp: CheckAddVariableRead(Oper1, FALSE, QuadNo)

   ELSE
   END
END AddQuadInformation ;


PROCEDURE stop ; BEGIN END stop ;


(*
   PutQuadO - alters a quadruple QuadNo with Op, Oper1, Oper2, Oper3, and
              sets a boolean to determinine whether overflow should be checked.
*)

PROCEDURE PutQuadO (QuadNo: CARDINAL;
                    Op: QuadOperator;
                    Oper1, Oper2, Oper3: CARDINAL;
                    overflow: BOOLEAN) ;
BEGIN
   PutQuadOType (QuadNo, Op, Oper1, Oper2, Oper3, overflow, TRUE)
END PutQuadO ;


(*
   PutQuadOType -
*)

PROCEDURE PutQuadOType (QuadNo: CARDINAL;
                        Op: QuadOperator;
                        Oper1, Oper2, Oper3: CARDINAL;
                        overflow, checktype: BOOLEAN) ;
VAR
   f: QuadFrame ;
BEGIN
   IF QuadNo = BreakAtQuad
   THEN
      stop
   END ;
   IF QuadrupleGeneration
   THEN
      EraseQuad (QuadNo) ;
      AddQuadInformation (QuadNo, Op, Oper1, Oper2, Oper3) ;
      f := GetQF (QuadNo) ;
      WITH f^ DO
         Operator      := Op ;
         Operand1      := Oper1 ;
         Operand2      := Oper2 ;
         Operand3      := Oper3 ;
         CheckOverflow := overflow ;
         CheckType     := checktype ;
         ConstExpr     := FALSE ;  (* IsInConstExpression () *)
      END
   END
END PutQuadOType ;


(*
   PutQuad - overwrites a quadruple QuadNo with Op, Oper1, Oper2, Oper3
*)

PROCEDURE PutQuad (QuadNo: CARDINAL;
                   Op: QuadOperator;
                   Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   PutQuadO (QuadNo, Op, Oper1, Oper2, Oper3, TRUE)
END PutQuad ;


(*
   GetQuadOTypetok - returns the fields associated with quadruple QuadNo.
*)

PROCEDURE GetQuadOTypetok (QuadNo: CARDINAL;
                           VAR tok: CARDINAL;
                           VAR Op: QuadOperator;
                           VAR Oper1, Oper2, Oper3: CARDINAL;
                           VAR overflowChecking, typeChecking, constExpr: BOOLEAN ;
                           VAR Op1Pos, Op2Pos, Op3Pos: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (QuadNo) ;
   LastQuadNo := QuadNo ;
   WITH f^ DO
      Op := Operator ;
      Oper1 := Operand1 ;
      Oper2 := Operand2 ;
      Oper3 := Operand3 ;
      Op1Pos := op1pos ;
      Op2Pos := op2pos ;
      Op3Pos := op3pos ;
      tok := TokenNo ;
      overflowChecking := CheckOverflow ;
      typeChecking := CheckType ;
      constExpr := ConstExpr
   END
END GetQuadOTypetok ;


(*
   UndoReadWriteInfo -
*)

PROCEDURE UndoReadWriteInfo (QuadNo: CARDINAL;
                             Op: QuadOperator;
                             Oper1, Oper2, Oper3: CARDINAL) ;
BEGIN
   CASE Op OF

   (* jumps, calls and branches *)
   IfInOp,
   IfNotInOp,
   IfEquOp,
   IfNotEquOp,
   IfLessOp,
   IfLessEquOp,
   IfGreOp,
   IfGreEquOp        : RemoveReference(QuadNo) ;
                       CheckRemoveVariableRead(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper2, FALSE, QuadNo) |

   TryOp,
   RetryOp,
   GotoOp            : RemoveReference(QuadNo) |

   (* variable references *)

   InclOp,
   ExclOp            : CheckRemoveVariableRead(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableWrite(Oper1, TRUE, QuadNo) |

   UnboundedOp,
   FunctValueOp,
   NegateOp,
   BecomesOp,
   HighOp,
   SizeOp            : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, FALSE, QuadNo) |
   AddrOp            : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) ;
                       (* CheckRemoveVariableReadLeftValue(Oper3, QuadNo) ; *)
                       (* the next line is a kludge and assumes we _will_
                          write to the variable as we have taken its address *)
                       CheckRemoveVariableWrite(Oper1, TRUE, QuadNo) |
   ReturnValueOp     : CheckRemoveVariableRead(Oper1, FALSE, QuadNo) |
   ReturnOp,
   CallOp,
   NewLocalVarOp,
   KillLocalVarOp    : |
   ParamOp           : CheckRemoveVariableRead(Oper2, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, FALSE, QuadNo) ;
                       IF (Oper1>0) AND (Oper1<=NoOfParam(Oper2)) AND
                          IsVarParam(Oper2, Oper1)
                       THEN
                          (* _may_ also write to a var parameter, although we dont know *)
                          CheckRemoveVariableWrite(Oper3, TRUE, QuadNo)
                       END |
   RecordFieldOp,
   ArrayOp,
   LogicalShiftOp,
   LogicalRotateOp,
   LogicalOrOp,
   LogicalAndOp,
   LogicalXorOp,
   CoerceOp,
   ConvertOp,
   CastOp,
   AddOp,
   SubOp,
   MultOp,
   DivM2Op,
   ModM2Op,
   ModFloorOp,
   DivCeilOp,
   ModCeilOp,
   DivFloorOp,
   ModTruncOp,
   DivTruncOp        : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper2, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, FALSE, QuadNo) |

   XIndrOp           : CheckRemoveVariableWrite(Oper1, TRUE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, FALSE, QuadNo) |

   IndrXOp           : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) ;
                       CheckRemoveVariableRead(Oper3, TRUE, QuadNo) |

(* RangeCheckOp      : CheckRangeRemoveVariableRead(Oper3, QuadNo) | *)
   SaveExceptionOp   : CheckRemoveVariableWrite(Oper1, FALSE, QuadNo) |
   RestoreExceptionOp: CheckRemoveVariableRead(Oper1, FALSE, QuadNo)

   ELSE
   END
END UndoReadWriteInfo ;


(*
   EraseQuad - erases a quadruple QuadNo, the quadruple is still in the list
               but wiped clean.
*)

PROCEDURE EraseQuad (QuadNo: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   WITH f^ DO
      UndoReadWriteInfo(QuadNo, Operator, Operand1, Operand2, Operand3) ;
      Operator := DummyOp ;   (* finally blank it out *)
      Operand1 := 0 ;
      Operand2 := 0 ;
      Operand3 := 0 ;
      Trash := 0 ;
      op1pos   := UnknownTokenNo ;
      op2pos   := UnknownTokenNo ;
      op3pos   := UnknownTokenNo ;
      ConstExpr := FALSE
   END
END EraseQuad ;


(*
   CheckAddVariableReadLeftValue -
*)

(*
PROCEDURE CheckAddVariableReadLeftValue (sym: CARDINAL; q: CARDINAL) ;
BEGIN
   IF IsVar(sym)
   THEN
      PutReadQuad(sym, LeftValue, q)
   END
END CheckAddVariableReadLeftValue ;
*)


(*
   CheckRemoveVariableReadLeftValue -
*)

(*
PROCEDURE CheckRemoveVariableReadLeftValue (sym: CARDINAL; q: CARDINAL) ;
BEGIN
   IF IsVar(sym)
   THEN
      RemoveReadQuad(sym, LeftValue, q)
   END
END CheckRemoveVariableReadLeftValue ;
*)


(*
   CheckAddVariableRead - checks to see whether symbol, Sym, is a variable or
                          a parameter and if so it then adds this quadruple
                          to the variable list.
*)

PROCEDURE CheckAddVariableRead (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      PutReadQuad(Sym, GetMode(Sym), Quad) ;
      IF (GetMode(Sym)=LeftValue) AND canDereference
      THEN
         PutReadQuad(Sym, RightValue, Quad)
      END
   END
END CheckAddVariableRead ;


(*
   CheckRemoveVariableRead - checks to see whether, Sym, is a variable or
                             a parameter and if so then it removes the
                             quadruple from the variable list.
*)

PROCEDURE CheckRemoveVariableRead (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      RemoveReadQuad(Sym, GetMode(Sym), Quad) ;
      IF (GetMode(Sym)=LeftValue) AND canDereference
      THEN
         RemoveReadQuad(Sym, RightValue, Quad)
      END
   END
END CheckRemoveVariableRead ;


(*
   CheckAddVariableWrite - checks to see whether symbol, Sym, is a variable and
                           if so it then adds this quadruple to the variable list.
*)

PROCEDURE CheckAddVariableWrite (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      IF (GetMode(Sym)=LeftValue) AND canDereference
      THEN
         PutReadQuad(Sym, LeftValue, Quad) ;
         PutWriteQuad(Sym, RightValue, Quad)
      ELSE
         PutWriteQuad(Sym, GetMode(Sym), Quad)
      END
   END
END CheckAddVariableWrite ;


(*
   CheckRemoveVariableWrite - checks to see whether, Sym, is a variable and
                              if so then it removes the quadruple from the
                              variable list.
*)

PROCEDURE CheckRemoveVariableWrite (Sym: CARDINAL; canDereference: BOOLEAN; Quad: CARDINAL) ;
BEGIN
   IF IsVar(Sym)
   THEN
      IF (GetMode(Sym)=LeftValue) AND canDereference
      THEN
         RemoveReadQuad(Sym, LeftValue, Quad) ;
         RemoveWriteQuad(Sym, RightValue, Quad)
      ELSE
         RemoveWriteQuad(Sym, GetMode(Sym), Quad)
      END
   END
END CheckRemoveVariableWrite ;


(*
   CheckConst -
*)

PROCEDURE CheckConst (sym: CARDINAL) ;
BEGIN
   IF IsConst(sym)
   THEN
      PutToBeSolvedByQuads(sym)
   END
END CheckConst ;


(*
   GetFirstQuad - returns the first quadruple.
*)

PROCEDURE GetFirstQuad () : CARDINAL ;
BEGIN
   RETURN( Head )
END GetFirstQuad ;


(*
   GetNextQuad - returns the Quadruple number following QuadNo.
*)

PROCEDURE GetNextQuad (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(QuadNo) ;
   RETURN( f^.Next )
END GetNextQuad ;


(*
   SubQuad - subtracts a quadruple QuadNo from a list Head.
*)

PROCEDURE SubQuad (QuadNo: CARDINAL) ;
VAR
   i   : CARDINAL ;
   f, g: QuadFrame ;
BEGIN
   IF QuadNo = BreakAtQuad
   THEN
      stop
   END ;
   f := GetQF(QuadNo) ;
   WITH f^ DO
      AlterReference(Head, QuadNo, f^.Next) ;
      UndoReadWriteInfo(QuadNo, Operator, Operand1, Operand2, Operand3)
   END ;
   IF Head=QuadNo
   THEN
      Head := f^.Next
   ELSE
      i := Head ;
      g := GetQF(i) ;
      WHILE g^.Next#QuadNo DO
         i := g^.Next ;
         g := GetQF(i)
      END ;
      g^.Next := f^.Next
   END ;
   f^.Operator := DummyOp ;
   DEC(NoOfQuads)
END SubQuad ;


(*
   GetRealQuad - returns the Quadruple number of the real quadruple
                 at QuadNo or beyond.
*)

PROCEDURE GetRealQuad (QuadNo: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   WHILE QuadNo#0 DO
      IF InBounds(QuadArray, QuadNo)
      THEN
         f := GetQF(QuadNo) ;
         WITH f^ DO
            IF (NOT IsPseudoQuad(QuadNo)) AND
               (Operator#DummyOp) AND (Operator#LineNumberOp) AND (Operator#StatementNoteOp)
            THEN
               RETURN( QuadNo )
            END
         END ;
         INC(QuadNo)
      ELSE
         RETURN( 0 )
      END
   END ;
   RETURN( 0 )
END GetRealQuad ;


(*
   AlterReference - alters all references from OldQuad, to NewQuad in a
                    quadruple list Head.
*)

PROCEDURE AlterReference (Head, OldQuad, NewQuad: CARDINAL) ;
VAR
   f, g: QuadFrame ;
   i   : CARDINAL ;
BEGIN
   f := GetQF(OldQuad) ;
   WHILE (f^.NoOfTimesReferenced>0) AND (Head#0) DO
      g := GetQF(Head) ;
      WITH g^ DO
         CASE Operator OF

         IfInOp,
         IfNotInOp,
         IfEquOp,
         IfNotEquOp,
         IfLessOp,
         IfLessEquOp,
         IfGreOp,
         IfGreEquOp,
         TryOp,
         RetryOp,
         GotoOp     : IF Operand3=OldQuad
                      THEN
                         ManipulateReference(Head, NewQuad)
                      END

         ELSE
         END ;
         i := Next
      END ;
      Head := i
   END
END AlterReference ;


(*
   GrowQuads - grows the list of quadruples to the quadruple, to.
*)

PROCEDURE GrowQuads (to: CARDINAL) ;
VAR
   i: CARDINAL ;
   f: QuadFrame ;
BEGIN
   IF (to#0) AND (to>GrowInitialization)
   THEN
      i := GrowInitialization+1 ;
      WHILE i<=to DO
         IF InBounds(QuadArray, i)
         THEN
            Assert(GetIndice(QuadArray, i)#NIL)
         ELSE
            NEW(f) ;
            IF f=NIL
            THEN
               InternalError ('out of memory error when trying to allocate a quadruple')
            END ;
            PutIndice(QuadArray, i, f) ;
            f^.NoOfTimesReferenced := 0
         END ;
         INC(i)
      END ;
      GrowInitialization := to
   END
END GrowQuads ;


(*
   ManipulateReference - manipulates the quadruple, q, so that it now points to quad, to.
*)

PROCEDURE ManipulateReference (q: CARDINAL; to: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   Assert((GrowInitialization>=q) OR (to=0)) ;
   GrowQuads(to) ;
   RemoveReference(q) ;
   f := GetQF(q) ;
   f^.Operand3 := to ;
   IF to#0
   THEN
      f := GetQF(to) ;
      INC(f^.NoOfTimesReferenced)
   END
END ManipulateReference ;


(*
   RemoveReference - remove the reference by quadruple q to wherever
                     it was pointing.
*)

PROCEDURE RemoveReference (q: CARDINAL) ;
VAR
   f, g: QuadFrame ;
BEGIN
   f := GetQF(q) ;
   IF (f^.Operand3#0) AND (f^.Operand3<NextQuad)
   THEN
      IF f^.Operand3 = BreakAtQuad
      THEN
         stop
      END ;
      g := GetQF(f^.Operand3) ;
      Assert(g^.NoOfTimesReferenced#0) ;
      DEC(g^.NoOfTimesReferenced)
   END
END RemoveReference ;


(*
   CountQuads - returns the number of quadruples.
*)

PROCEDURE CountQuads () : CARDINAL ;
BEGIN
   RETURN( NoOfQuads )
END CountQuads ;


(*
   NewQuad - sets QuadNo to a new quadruple.
*)

PROCEDURE NewQuad (VAR QuadNo: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   QuadNo := FreeList ;
   IF InBounds (QuadArray, QuadNo) AND (GetIndice (QuadArray, QuadNo) # NIL)
   THEN
      f := GetIndice (QuadArray, QuadNo)
   ELSE
      NEW (f) ;
      IF f=NIL
      THEN
         InternalError ('out of memory error trying to allocate a quadruple')
      ELSE
         INC (NoOfQuads) ;
         PutIndice (QuadArray, QuadNo, f) ;
         f^.NoOfTimesReferenced := 0
      END
   END ;
   WITH f^ DO
      Operator := DummyOp ;
      Operand3 := 0 ;
      Next := 0
   END ;
   INC (FreeList) ;
   IF GrowInitialization < FreeList
   THEN
      GrowInitialization := FreeList
   END
END NewQuad ;


(*
   CheckVariableAt - checks to see whether, sym, was declared at a particular address.
*)

PROCEDURE CheckVariableAt (sym: CARDINAL) ;
BEGIN
   IF IsVar (sym) AND IsVariableAtAddress (sym)
   THEN
      IF GetMode (sym) = LeftValue
      THEN
         GenQuad (InitAddressOp, sym, NulSym, GetVariableAtAddress (sym))
      ELSE
         InternalError ('expecting lvalue for this variable which is declared at an explicit address')
      END
   END
END CheckVariableAt ;


(*
   CheckVariablesAt - checks to see whether we need to initialize any pointers
                      which point to variable declared at addresses.
*)

PROCEDURE CheckVariablesAt (scope: CARDINAL) ;
BEGIN
   ForeachLocalSymDo (scope, CheckVariableAt)
END CheckVariablesAt ;


(*
   GetTurnInterrupts - returns the TurnInterrupts procedure function.
*)

PROCEDURE GetTurnInterrupts (tok: CARDINAL) : CARDINAL ;
BEGIN
   IF Iso
   THEN
      RETURN GetQualidentImport (tok,
                                 MakeKey ('TurnInterrupts'), MakeKey ('COROUTINES'))
   ELSE
      RETURN GetQualidentImport (tok,
                                 MakeKey ('TurnInterrupts'), MakeKey ('SYSTEM'))
   END
END GetTurnInterrupts ;


(*
   GetProtection - returns the PROTECTION data type.
*)

PROCEDURE GetProtection (tok: CARDINAL) : CARDINAL ;
BEGIN
   IF Iso
   THEN
      RETURN GetQualidentImport (tok,
                                 MakeKey ('PROTECTION'), MakeKey ('COROUTINES'))
   ELSE
      RETURN GetQualidentImport (tok,
                                 MakeKey ('PROTECTION'), MakeKey ('SYSTEM'))
   END
END GetProtection ;


(*
   CheckNeedPriorityBegin - checks to see whether we need to save the old
                            module priority and change to another module
                            priority.
                            The current module initialization or procedure
                            being built is defined by, scope. The module whose
                            priority will be used is defined by, module.
*)

PROCEDURE CheckNeedPriorityBegin (tok: CARDINAL; scope, module: CARDINAL) ;
VAR
   ProcSym, old: CARDINAL ;
BEGIN
   IF GetPriority (module) # NulSym
   THEN
      (* module has been given a priority *)
      ProcSym := GetTurnInterrupts (tok) ;
      IF ProcSym # NulSym
      THEN
         old := MakeTemporary (tok, RightValue) ;
         PutVar (old, GetProtection (tok)) ;

         GenQuadO (tok, SavePriorityOp, old, scope, ProcSym, FALSE) ;
         PushWord (PriorityStack, old)
      END
   END
END CheckNeedPriorityBegin ;


(*
   CheckNeedPriorityEnd - checks to see whether we need to restore the old
                          module priority.
                          The current module initialization or procedure
                          being built is defined by, scope.
*)

PROCEDURE CheckNeedPriorityEnd (tok: CARDINAL;
                                scope, module: CARDINAL) ;
VAR
   ProcSym, old: CARDINAL ;
BEGIN
   IF GetPriority (module) # NulSym
   THEN
      (* module has been given a priority *)
      ProcSym := GetTurnInterrupts (tok) ;
      IF ProcSym # NulSym
      THEN
         old := PopWord (PriorityStack) ;
         GenQuad (RestorePriorityOp, old, scope, ProcSym)
      END
   END
END CheckNeedPriorityEnd ;


(*
   StartBuildDefFile - generates a StartFileDefOp quadruple indicating the file
                       that has produced the subsequent quadruples.
                       The code generator uses the StartDefFileOp quadruples
                       to relate any error to the appropriate file.


                       Entry                   Exit
                       =====                   ====


                Ptr ->                                        <- Ptr
                       +------------+          +------------+
                       | ModuleName |          | ModuleName |
                       |------------|          |------------|


                       Quadruples Produced

                       q     StartDefFileOp  _  _  ModuleSym
*)

PROCEDURE StartBuildDefFile (tok: CARDINAL) ;
VAR
   ModuleName: Name ;
BEGIN
   PopT (ModuleName) ;
   PushT (ModuleName) ;
   GenQuadO (tok, StartDefFileOp, tok, NulSym, GetModule (ModuleName), FALSE)
END StartBuildDefFile ;


(*
   StartBuildModFile - generates a StartModFileOp quadruple indicating the file
                       that has produced the subsequent quadruples.
                       The code generator uses the StartModFileOp quadruples
                       to relate any error to the appropriate file.


                       Entry                   Exit
                       =====                   ====


                Ptr ->                                        <- Ptr
                       +------------+          +------------+
                       | ModuleName |          | ModuleName |
                       |------------|          |------------|


                       Quadruples Produced

                       q     StartModFileOp  lineno  filename  ModuleSym
*)

PROCEDURE StartBuildModFile (tok: CARDINAL) ;
BEGIN
   GenQuadO (tok, StartModFileOp, tok,
             WORD (makekey (string (GetFileName ()))),
             GetFileModule (), FALSE)
END StartBuildModFile ;


(*
   EndBuildFile - generates an EndFileOp quadruple indicating the file
                  that has produced the previous quadruples has ended.

                  Entry                   Exit
                  =====                   ====


           Ptr ->                                        <- Ptr
                  +------------+          +------------+
                  | ModuleName |          | ModuleName |
                  |------------|          |------------|


                  Quadruples Produced

                  q     EndFileOp  _  _  ModuleSym
*)

PROCEDURE EndBuildFile (tok: CARDINAL) ;
VAR
   ModuleName: Name ;
BEGIN
   ModuleName := OperandT (1) ;
   GenQuadO (tok, EndFileOp, NulSym, NulSym, GetModule (ModuleName), FALSE)
END EndBuildFile ;


(*
   StartBuildInit - Sets the start of initialization code of the
                    current module to the next quadruple.
*)

PROCEDURE StartBuildInit (tok: CARDINAL) ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := GetCurrentModule() ;
   Assert(IsModule(ModuleSym) OR IsDefImp(ModuleSym)) ;
   Assert(GetSymName(ModuleSym)=name) ;
   PutModuleStartQuad(ModuleSym, NextQuad) ;
   GenQuad(InitStartOp, tok, GetFileModule(), ModuleSym) ;
   PushWord(ReturnStack, 0) ;
   PushT(name) ;
   CheckVariablesAt(ModuleSym) ;
   CheckNeedPriorityBegin(tok, ModuleSym, ModuleSym) ;
   PushWord(TryStack, NextQuad) ;
   PushWord(CatchStack, 0) ;
   IF HasExceptionBlock(ModuleSym)
   THEN
      GenQuad(TryOp, NulSym, NulSym, 0)
   END
END StartBuildInit ;


(*
   EndBuildInit - Sets the end initialization code of a module.
*)

PROCEDURE EndBuildInit (tok: CARDINAL) ;
BEGIN
   IF HasExceptionBlock(GetCurrentModule())
   THEN
      BuildRTExceptLeave (tok, TRUE) ;
      GenQuadO (tok, CatchEndOp, NulSym, NulSym, NulSym, FALSE)
   END ;
   BackPatch (PopWord (ReturnStack), NextQuad) ;
   CheckNeedPriorityEnd (tok, GetCurrentModule(), GetCurrentModule()) ;
   PutModuleEndQuad (GetCurrentModule(), NextQuad) ;
   CheckVariablesInBlock (GetCurrentModule()) ;
   GenQuadO (tok, InitEndOp, tok, GetFileModule(), GetCurrentModule(), FALSE)
END EndBuildInit ;


(*
   StartBuildFinally - Sets the start of finalization code of the
                       current module to the next quadruple.
*)

PROCEDURE StartBuildFinally (tok: CARDINAL) ;
VAR
   name     : Name ;
   ModuleSym: CARDINAL ;
BEGIN
   PopT(name) ;
   ModuleSym := GetCurrentModule() ;
   Assert(IsModule(ModuleSym) OR IsDefImp(ModuleSym)) ;
   Assert(GetSymName(ModuleSym)=name) ;
   PutModuleFinallyStartQuad(ModuleSym, NextQuad) ;
   GenQuadO (tok, FinallyStartOp, tok, GetFileModule(), ModuleSym, FALSE) ;
   PushWord (ReturnStack, 0) ;
   PushT (name) ;
   (* CheckVariablesAt(ModuleSym) ; *)
   CheckNeedPriorityBegin (tok, ModuleSym, ModuleSym) ;
   PushWord (TryStack, NextQuad) ;
   PushWord (CatchStack, 0) ;
   IF HasExceptionFinally (ModuleSym)
   THEN
      GenQuadO (tok, TryOp, NulSym, NulSym, 0, FALSE)
   END
END StartBuildFinally ;


(*
   EndBuildFinally - Sets the end finalization code of a module.
*)

PROCEDURE EndBuildFinally (tok: CARDINAL) ;
BEGIN
   IF HasExceptionFinally(GetCurrentModule())
   THEN
      BuildRTExceptLeave (tok, TRUE) ;
      GenQuadO (tok, CatchEndOp, NulSym, NulSym, NulSym, FALSE)
   END ;
   BackPatch (PopWord (ReturnStack), NextQuad) ;
   CheckNeedPriorityEnd (tok, GetCurrentModule (), GetCurrentModule ()) ;
   PutModuleFinallyEndQuad(GetCurrentModule (), NextQuad) ;
   CheckVariablesInBlock (GetCurrentModule ()) ;
   GenQuadO (tok, FinallyEndOp, tok, GetFileModule (),
             GetCurrentModule(), FALSE)
END EndBuildFinally ;


(*
   BuildRTExceptEnter - informs RTExceptions that we are about to enter the except state.
*)

PROCEDURE BuildRTExceptEnter (tok: CARDINAL) ;
VAR
   old,
   ProcSym: CARDINAL ;
BEGIN
   IF Exceptions
   THEN
      (* now inform the Modula-2 runtime we are in the exception state *)
      ProcSym := GetQualidentImport (tok,
                                     MakeKey('SetExceptionState'), MakeKey('RTExceptions')) ;
      IF ProcSym=NulSym
      THEN
         MetaErrorT0 (tok,
                      '{%W}no procedure SetExceptionState found in RTExceptions which is needed to implement exception handling')
      ELSE
         old := MakeTemporary (tok, RightValue) ;
         PutVar (old, Boolean) ;
         GenQuadO (tok, SaveExceptionOp, old, NulSym, ProcSym, FALSE) ;
         PushWord (ExceptStack, old)
      END
   ELSE
      MetaErrorT0 (tok,
                   '{%E}cannot use {%kEXCEPT} blocks with the -fno-exceptions flag')
   END
END BuildRTExceptEnter ;


(*
   BuildRTExceptLeave - informs RTExceptions that we are about to leave the except state.
                        If, destroy, is TRUE then pop the ExceptStack.
*)

PROCEDURE BuildRTExceptLeave (tok: CARDINAL; destroy: BOOLEAN) ;
VAR
   old,
   ProcSym: CARDINAL ;
BEGIN
   IF Exceptions
   THEN
      (* now inform the Modula-2 runtime we are in the exception state *)
      ProcSym := GetQualidentImport (tok,
                                     MakeKey('SetExceptionState'), MakeKey('RTExceptions')) ;
      IF ProcSym#NulSym
      THEN
         IF destroy
         THEN
            old := PopWord (ExceptStack)
         ELSE
            old := PeepWord (ExceptStack, 1)
         END ;
         GenQuadO (tok, RestoreExceptionOp, old, NulSym, ProcSym, FALSE)
      END
   ELSE
      (* no need for an error message here as it will be generated in the Enter procedure above *)
   END
END BuildRTExceptLeave ;


(*
   BuildExceptInitial - adds an CatchBeginOp, CatchEndOp quadruple
                        in the current block.
*)

PROCEDURE BuildExceptInitial (tok: CARDINAL) ;
VAR
   previous: CARDINAL ;
BEGIN
   (* we have finished the 'try' block, so now goto the return
      section which will tidy up (any) priorities before returning.
   *)
   GenQuadO (tok, GotoOp, NulSym, NulSym, PopWord(ReturnStack), FALSE) ;
   PushWord (ReturnStack, NextQuad-1) ;
   (*
      this is the 'catch' block.
   *)
   BackPatch (PeepWord (TryStack, 1), NextQuad) ;
   GenQuadO (tok, CatchBeginOp, NulSym, NulSym, NulSym, FALSE) ;
   previous := PopWord (CatchStack) ;
   IF previous # 0
   THEN
      MetaErrorT0 (tok,
                   '{%E}only allowed one EXCEPT statement in a procedure or module')
   END ;
   PushWord (CatchStack, NextQuad-1) ;
   BuildRTExceptEnter (tok)
END BuildExceptInitial ;


(*
   BuildExceptFinally - adds an ExceptOp quadruple in a modules
                        finally block.
*)

PROCEDURE BuildExceptFinally (tok: CARDINAL) ;
BEGIN
   BuildExceptInitial (tok)
END BuildExceptFinally ;


(*
   BuildExceptProcedure - adds an ExceptOp quadruple in a procedure
                          block.
*)

PROCEDURE BuildExceptProcedure (tok: CARDINAL) ;
BEGIN
   BuildExceptInitial (tok)
END BuildExceptProcedure ;


(*
   BuildRetry - adds an RetryOp quadruple.
*)

PROCEDURE BuildRetry (tok: CARDINAL);
BEGIN
   IF PeepWord (CatchStack, 1) = 0
   THEN
      MetaErrorT0 (tok,
                   '{%E}the {%kRETRY} statement must occur after an {%kEXCEPT} statement in the same module or procedure block')
   ELSE
      BuildRTExceptLeave (tok, FALSE) ;
      GenQuadO (tok, RetryOp, NulSym, NulSym, PeepWord (TryStack, 1), FALSE)
   END
END BuildRetry ;


(*
   SafeRequestSym - only used during scaffold to get argc, argv, envp.
                    It attempts to get symbol name from the current scope(s) and if
                    it fails then it falls back onto default constants.
*)

PROCEDURE SafeRequestSym (tok: CARDINAL; name: Name) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   sym := GetSym (name) ;
   IF sym = NulSym
   THEN
      IF name = MakeKey ('argc')
      THEN
         RETURN MakeConstLit (tok, MakeKey ('0'), ZType)
      ELSIF (name = MakeKey ('argv')) OR (name = MakeKey ('envp'))
      THEN
         RETURN Nil
      ELSE
         InternalError ('not expecting this parameter name') ;
         RETURN Nil
      END
   END ;
   RETURN sym
END SafeRequestSym ;


(*
   callRequestDependant - create a call:
                          RequestDependant (GetSymName (modulesym), GetLibName (modulesym),
                                            GetSymName (depModuleSym), GetLibName (depModuleSym));
*)

PROCEDURE callRequestDependant (tokno: CARDINAL;
                                moduleSym, depModuleSym: CARDINAL;
                                requestDep: CARDINAL) ;
BEGIN
   Assert (requestDep # NulSym) ;
   PushTtok (requestDep, tokno) ;
   PushTFtok (Adr, Address, tokno) ;
   PushTtok (MakeConstString (tokno, GetSymName (moduleSym)), tokno) ;
   PushT (1) ;
   BuildAdrFunction ;

   PushTFtok (Adr, Address, tokno) ;
   PushTtok (MakeConstString (tokno, GetLibName (moduleSym)), tokno) ;
   PushT (1) ;
   BuildAdrFunction ;

   IF depModuleSym = NulSym
   THEN
      PushTF (Nil, Address) ;
      PushTF (Nil, Address)
   ELSE
      PushTFtok (Adr, Address, tokno) ;
      PushTtok (MakeConstString (tokno, GetSymName (depModuleSym)), tokno) ;
      PushT (1) ;
      BuildAdrFunction ;

      PushTFtok (Adr, Address, tokno) ;
      PushTtok (MakeConstString (tokno, GetLibName (depModuleSym)), tokno) ;
      PushT (1) ;
      BuildAdrFunction
   END ;

   PushT (4) ;
   BuildProcedureCall (tokno)
END callRequestDependant ;


(*
   ForeachImportInDepDo -
*)

PROCEDURE ForeachImportInDepDo (importStatements: List; moduleSym, requestDep: CARDINAL) ;
VAR
   i, j,
   m, n    : CARDINAL ;
   imported,
   stmt     : CARDINAL ;
   l       : List ;
BEGIN
   IF importStatements # NIL
   THEN
      i := 1 ;
      n := NoOfItemsInList (importStatements) ;
      WHILE i <= n DO
         stmt := GetItemFromList (importStatements, i) ;
         Assert (IsImportStatement (stmt)) ;
         l := GetImportStatementList (stmt) ;
         j := 1 ;
         m := NoOfItemsInList (l) ;
         WHILE j <= m DO
            imported := GetItemFromList (l, j) ;
            Assert (IsImport (imported)) ;
            callRequestDependant (GetImportDeclared (imported),
                                  moduleSym, GetImportModule (imported),
                                  requestDep) ;
            INC (j) ;
         END ;
         INC (i)
      END
   END
END ForeachImportInDepDo ;


(*
   ForeachImportedModuleDo -
*)

PROCEDURE ForeachImportedModuleDo (moduleSym, requestDep: CARDINAL) ;
VAR
   importStatements: List ;
BEGIN
   importStatements := GetModuleModImportStatementList (moduleSym) ;
   ForeachImportInDepDo (importStatements, moduleSym, requestDep) ;
   importStatements := GetModuleDefImportStatementList (moduleSym) ;
   ForeachImportInDepDo (importStatements, moduleSym, requestDep)
END ForeachImportedModuleDo ;


(*
   BuildM2DepFunction - creates the dependency graph procedure using IR:
                        static void
                        dependencies (void)
                        {
                           M2RTS_RequestDependant (module_name, libname, "b", "b libname");
                           M2RTS_RequestDependant (module_name, libname, NULL, NULL);
                        }
*)

PROCEDURE BuildM2DepFunction (tokno: CARDINAL; moduleSym: CARDINAL) ;
VAR
   requestDep,
   ctor, init, fini, dep: CARDINAL ;
BEGIN
   IF ScaffoldDynamic
   THEN
      (* Scaffold required and dynamic dependency graph should be produced.  *)
      GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
      PushT (dep) ;
      BuildProcedureStart ;
      BuildProcedureBegin ;
      StartScope (dep) ;
      requestDep := GetQualidentImport (tokno,
                                        MakeKey ("RequestDependant"),
                                        MakeKey ("M2RTS")) ;
      IF requestDep # NulSym
      THEN
         ForeachImportedModuleDo (moduleSym, requestDep) ;
         callRequestDependant (tokno, moduleSym, NulSym, requestDep)
      END ;
      EndScope ;
      BuildProcedureEnd ;
      PopN (1)
   END
END BuildM2DepFunction ;


(*
   BuildM2LinkFunction - creates the _M2_link procedure which will
                         cause the linker to pull in all the module ctors.
*)

PROCEDURE BuildM2LinkFunction (tokno: CARDINAL) ;
BEGIN
   IF ScaffoldDynamic
   THEN
      IF linkFunction # NulSym
      THEN
         (* void
           _M2_link (void)
           {
              for each module in uselist do
                 PROC foo_%d = _M2_module_ctor
              done
           }.  *)
         PushT (linkFunction) ;
         BuildProcedureStart ;
         BuildProcedureBegin ;
         StartScope (linkFunction) ;
         PopulateCtorArray (tokno) ;
         EndScope ;
         BuildProcedureEnd ;
         PopN (1)
      END
   END
END BuildM2LinkFunction ;


(*
   BuildTry - build the try statement for main.
*)

PROCEDURE BuildTry (tokno: CARDINAL) ;
BEGIN
   IF Exceptions
   THEN
      PushWord (TryStack, NextQuad) ;
      PushWord (CatchStack, 0) ;
      GenQuadO (tokno, TryOp, NulSym, NulSym, 0, FALSE)
   END
END BuildTry ;


(*
   BuildExcept - build the except block for main.
*)

PROCEDURE BuildExcept (tokno: CARDINAL) ;
VAR
   catchProcedure: CARDINAL ;
BEGIN
   IF Exceptions
   THEN
      BuildExceptInitial (tokno) ;
      catchProcedure := GetQualidentImport (tokno,
                                            MakeKey ('DefaultErrorCatch'),
                                            MakeKey ('RTExceptions')) ;
      IF catchProcedure # NulSym
      THEN
         PushTtok (catchProcedure, tokno) ;
         PushT (0) ;
         BuildProcedureCall (tokno)
      END ;
      BuildRTExceptLeave (tokno, TRUE) ;
      GenQuadO (tokno, CatchEndOp, NulSym, NulSym, NulSym, FALSE)
   END
END BuildExcept ;


(*
   BuildM2MainFunction - creates the main function with appropriate calls to the scaffold.
*)

PROCEDURE BuildM2MainFunction (tokno: CARDINAL) ;
BEGIN
   IF (ScaffoldDynamic OR ScaffoldStatic) AND (NOT SharedFlag)
   THEN
      (* Scaffold required and main should be produced.  *)
      (*
         int
         main (int argc, char *argv[], char *envp[])
         {
            try {
               _M2_init (argc, argv, envp);
               _M2_fini (argc, argv, envp);
               return 0;
            }
            catch (...) {
               RTExceptions_DefaultErrorCatch ();
               return 0;
            }
         }
      *)
      PushT (mainFunction) ;
      BuildProcedureStart ;
      BuildProcedureBegin ;
      StartScope (mainFunction) ;
      BuildTry (tokno) ;
      (* _M2_init (argc, argv, envp);  *)
      PushTtok (initFunction, tokno) ;
      PushTtok (RequestSym (tokno, MakeKey ("argc")), tokno) ;
      PushTtok (RequestSym (tokno, MakeKey ("argv")), tokno) ;
      PushTtok (RequestSym (tokno, MakeKey ("envp")), tokno) ;
      PushT (3) ;
      BuildProcedureCall (tokno) ;

      (* _M2_fini (argc, argv, envp);  *)
      PushTtok (finiFunction, tokno) ;
      PushTtok (RequestSym (tokno, MakeKey ("argc")), tokno) ;
      PushTtok (RequestSym (tokno, MakeKey ("argv")), tokno) ;
      PushTtok (RequestSym (tokno, MakeKey ("envp")), tokno) ;
      PushT (3) ;
      BuildProcedureCall (tokno) ;
      PushZero (tokno, Integer) ;
      BuildReturn (tokno) ;
      BuildExcept (tokno) ;
      PushZero (tokno, Integer) ;
      BuildReturn (tokno) ;
      EndScope ;
      BuildProcedureEnd ;
      PopN (1)
   END
END BuildM2MainFunction ;


(*
   DeferMakeConstStringCnul - return a C const string which will be nul terminated.
*)

PROCEDURE DeferMakeConstStringCnul (tok: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   const: CARDINAL ;
BEGIN
   const := MakeConstStringCnul (tok, NulName, FALSE) ;
   GenQuadO (tok, StringConvertCnulOp, const, 0, sym, FALSE) ;
   RETURN const
END DeferMakeConstStringCnul ;


(*
   DeferMakeConstStringM2nul - return a const string which will be nul terminated.
*)

PROCEDURE DeferMakeConstStringM2nul (tok: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   const: CARDINAL ;
BEGIN
   const := MakeConstStringM2nul (tok, NulName, FALSE) ;
   GenQuadO (tok, StringConvertM2nulOp, const, 0, sym, FALSE) ;
   RETURN const
END DeferMakeConstStringM2nul ;


(*
   BuildStringAdrParam - push the address of a nul terminated string onto the quad stack.
*)

PROCEDURE BuildStringAdrParam (tok: CARDINAL; name: Name);
VAR
   str, m2strnul: CARDINAL ;
BEGIN
   PushTFtok (Adr, Address, tok) ;
   str := MakeConstString (tok, name) ;
   PutConstStringKnown (tok, str, name, FALSE, TRUE) ;
   m2strnul := DeferMakeConstStringM2nul (tok, str) ;
   PushTtok (m2strnul, tok) ;
   PushT (1) ;
   BuildAdrFunction
END BuildStringAdrParam ;


(*
   BuildM2InitFunction -
*)

PROCEDURE BuildM2InitFunction (tok: CARDINAL; moduleSym: CARDINAL) ;
VAR
   constructModules: CARDINAL ;
BEGIN
   IF ScaffoldDynamic OR ScaffoldStatic
   THEN
      (* Scaffold required and main should be produced.  *)
      (* int
         _M2_init (int argc, char *argv[], char *envp[])
         {
            M2RTS_ConstructModules (module_name, libname,
                                    overrideliborder, argc, argv, envp);
         }  *)
      PushT (initFunction) ;
      BuildProcedureStart ;
      BuildProcedureBegin ;
      StartScope (initFunction) ;
      IF ScaffoldDynamic
      THEN
         IF linkFunction # NulSym
         THEN
            (* _M2_link ();  *)
            PushTtok (linkFunction, tok) ;
            PushT (0) ;
            BuildProcedureCall (tok)
         END ;

         (* Lookup ConstructModules and call it.  *)
         constructModules := GetQualidentImport (tok,
                                                 MakeKey ("ConstructModules"),
                                                 MakeKey ("M2RTS")) ;
         IF constructModules # NulSym
         THEN
            (* ConstructModules (module_name, argc, argv, envp);  *)
            PushTtok (constructModules, tok) ;

	    BuildStringAdrParam (tok, GetSymName (moduleSym)) ;
	    BuildStringAdrParam (tok, GetLibName (moduleSym)) ;
	    BuildStringAdrParam (tok, makekey (GetRuntimeModuleOverride ())) ;

            PushTtok (SafeRequestSym (tok, MakeKey ("argc")), tok) ;
            PushTtok (SafeRequestSym (tok, MakeKey ("argv")), tok) ;
            PushTtok (SafeRequestSym (tok, MakeKey ("envp")), tok) ;
            PushT (6) ;
            BuildProcedureCall (tok) ;
         END
      ELSIF ScaffoldStatic
      THEN
         ForeachModuleCallInit (tok,
                                SafeRequestSym (tok, MakeKey ("argc")),
                                SafeRequestSym (tok, MakeKey ("argv")),
                                SafeRequestSym (tok, MakeKey ("envp")))
      END ;
      EndScope ;
      BuildProcedureEnd ;
      PopN (1)
   END
END BuildM2InitFunction ;


(*
   BuildM2FiniFunction -
*)

PROCEDURE BuildM2FiniFunction (tok: CARDINAL; moduleSym: CARDINAL) ;
VAR
   deconstructModules: CARDINAL ;
BEGIN
   IF ScaffoldDynamic OR ScaffoldStatic
   THEN
      (* Scaffold required and main should be produced.  *)
      PushT (finiFunction) ;
      BuildProcedureStart ;
      BuildProcedureBegin ;
      StartScope (finiFunction) ;
      IF ScaffoldDynamic
      THEN
         (* static void
            _M2_finish (int argc, char *argv[], char *envp[])
            {
              M2RTS_DeconstructModules (module_name, argc, argv, envp);
            }  *)
         deconstructModules := GetQualidentImport (tok,
                                                   MakeKey ("DeconstructModules"),
                                                   MakeKey ("M2RTS")) ;
         IF deconstructModules # NulSym
         THEN
            (* DeconstructModules (module_name, argc, argv, envp);  *)
            PushTtok (deconstructModules, tok) ;

            PushTFtok (Adr, Address, tok) ;
            PushTtok (MakeConstString (tok, GetSymName (moduleSym)), tok) ;
            PushT(1) ;
            BuildAdrFunction ;

            PushTFtok (Adr, Address, tok) ;
            PushTtok (MakeConstString (tok, GetLibName (moduleSym)), tok) ;
            PushT(1) ;
            BuildAdrFunction ;

            PushTtok (SafeRequestSym (tok, MakeKey ("argc")), tok) ;
            PushTtok (SafeRequestSym (tok, MakeKey ("argv")), tok) ;
            PushTtok (SafeRequestSym (tok, MakeKey ("envp")), tok) ;
            PushT (5) ;
            BuildProcedureCall (tok)
         END
      ELSIF ScaffoldStatic
      THEN
         ForeachModuleCallFinish (tok,
                                  SafeRequestSym (tok, MakeKey ("argc")),
                                  SafeRequestSym (tok, MakeKey ("argv")),
                                  SafeRequestSym (tok, MakeKey ("envp")))
      END ;
      EndScope ;
      BuildProcedureEnd ;
      PopN (1)
   END
END BuildM2FiniFunction ;


(*
   BuildM2CtorFunction - create a constructor function associated with moduleSym.

                         void
                         ctorFunction ()
                         {
                           M2RTS_RegisterModule (GetSymName (moduleSym), GetLibName (moduleSym),
                                                 init, fini, dependencies);
                         }
*)

PROCEDURE BuildM2CtorFunction (tok: CARDINAL; moduleSym: CARDINAL) ;
VAR
   RegisterModule       : CARDINAL ;
   ctor, init, fini, dep: CARDINAL ;
BEGIN
   IF ScaffoldDynamic
   THEN
      GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
      IF ctor # NulSym
      THEN
         Assert (IsProcedure (ctor)) ;
         PushT (ctor) ;
         BuildProcedureStart ;
         BuildProcedureBegin ;
         StartScope (ctor) ;
         RegisterModule := GetQualidentImport (tok,
                                               MakeKey ("RegisterModule"),
                                               MakeKey ("M2RTS")) ;
         IF RegisterModule # NulSym
         THEN
            (* RegisterModule (module_name, init, fini, dependencies);  *)
            PushTtok (RegisterModule, tok) ;

            PushTFtok (Adr, Address, tok) ;
            PushTtok (MakeConstString (tok, GetSymName (moduleSym)), tok) ;
            PushT (1) ;
            BuildAdrFunction ;

            PushTFtok (Adr, Address, tok) ;
            PushTtok (MakeConstString (tok, GetLibName (moduleSym)), tok) ;
            PushT (1) ;
            BuildAdrFunction ;

            PushTtok (init, tok) ;
            PushTtok (fini, tok) ;
            PushTtok (dep, tok) ;
            PushT (5) ;
            BuildProcedureCall (tok)
         END ;
         EndScope ;
         BuildProcedureEnd ;
         PopN (1)
      END
   END
END BuildM2CtorFunction ;


(*
   BuildScaffold - generate the main, init, finish functions if
                   no -c and this is the application module.
*)

PROCEDURE BuildScaffold (tok: CARDINAL; moduleSym: CARDINAL) ;
BEGIN
   IF GetMainModule () = moduleSym
   THEN
      DeclareScaffold (tok) ;
      IF (ScaffoldMain OR (NOT cflag))
      THEN
         (* There are module init/fini functions and
            application init/fini functions.
            Here we create the application pair.  *)
         BuildM2LinkFunction (tok) ;
         BuildM2MainFunction (tok) ;
         BuildM2InitFunction (tok, moduleSym) ;  (* Application init.  *)
         BuildM2FiniFunction (tok, moduleSym) ;  (* Application fini.  *)
      END ;
      BuildM2DepFunction (tok, moduleSym) ;  (* Per module dependency.  *)
      (* Each module needs a ctor to register the module
         init/finish/dep with M2RTS.  *)
      BuildM2CtorFunction (tok, moduleSym)
   ELSIF WholeProgram
   THEN
      DeclareScaffold (tok) ;
      BuildM2DepFunction (tok, moduleSym) ;  (* Per module dependency.  *)
      (* Each module needs a ctor to register the module
         init/finish/dep with M2RTS.  *)
      BuildM2CtorFunction (tok, moduleSym)
   END
END BuildScaffold ;


(*
   BuildModuleStart - starts current module scope.
*)

PROCEDURE BuildModuleStart (tok: CARDINAL) ;
BEGIN
   GenQuadO (tok,
             ModuleScopeOp, tok,
             WORD (makekey (string (GetFileName ()))), GetCurrentModule (), FALSE)
END BuildModuleStart ;


(*
   StartBuildInnerInit - Sets the start of initialization code of the
                         inner module to the next quadruple.
*)

PROCEDURE StartBuildInnerInit (tok: CARDINAL) ;
BEGIN
   PutModuleStartQuad (GetCurrentModule(), NextQuad) ;
   GenQuadO (tok, InitStartOp, tok, NulSym, GetCurrentModule(), FALSE) ;
   PushWord (ReturnStack, 0) ;
   CheckNeedPriorityBegin (tok, GetCurrentModule(), GetCurrentModule()) ;
   PushWord (TryStack, NextQuad) ;
   PushWord (CatchStack, 0) ;
   IF HasExceptionFinally (GetCurrentModule())
   THEN
      GenQuadO (tok, TryOp, NulSym, NulSym, 0, FALSE)
   END
END StartBuildInnerInit ;


(*
   EndBuildInnerInit - Sets the end initialization code of a module.
*)

PROCEDURE EndBuildInnerInit (tok: CARDINAL) ;
BEGIN
   IF HasExceptionBlock (GetCurrentModule())
   THEN
      BuildRTExceptLeave (tok, TRUE) ;
      GenQuadO (tok, CatchEndOp, NulSym, NulSym, NulSym, FALSE)
   END ;
   PutModuleEndQuad (GetCurrentModule(), NextQuad) ;
   CheckVariablesInBlock (GetCurrentModule ()) ;
   BackPatch (PopWord (ReturnStack), NextQuad) ;
   CheckNeedPriorityEnd (tok, GetCurrentModule (), GetCurrentModule ()) ;
   GenQuadO (tok, InitEndOp, tok, NulSym, GetCurrentModule (), FALSE)
END EndBuildInnerInit ;


(*
   BuildModulePriority - assigns the current module with a priority
                         from the top of stack.

                         Entry                   Exit
                         =====                   ====


                  Ptr ->                         Empty
                         +------------+
                         | Priority   |
                         |------------|
*)

PROCEDURE BuildModulePriority ;
VAR
   Priority: CARDINAL ;
BEGIN
   PopT (Priority) ;
   PutPriority (GetCurrentModule (), Priority)
END BuildModulePriority ;


(*
   ForLoopAnalysis - checks all the FOR loops for index variable manipulation
                     and dangerous usage outside the loop.
*)

PROCEDURE ForLoopAnalysis ;
VAR
   i, n   : CARDINAL ;
   forDesc: ForLoopInfo ;
BEGIN
   IF Pedantic
   THEN
      n := HighIndice (ForInfo) ;
      i := 1 ;
      WHILE i <= n DO
         forDesc := GetIndice (ForInfo, i) ;
         CheckForIndex (forDesc) ;
         INC (i)
      END
   END
END ForLoopAnalysis ;


(*
   AddForInfo - adds the description of the FOR loop into the record list.
                This is used if -pedantic is turned on to check index variable
                usage.
*)

PROCEDURE AddForInfo (Start, End, IncQuad: CARDINAL; Sym: CARDINAL; idtok: CARDINAL) ;
VAR
   forDesc: ForLoopInfo ;
BEGIN
   IF Pedantic
   THEN
      NEW (forDesc) ;
      WITH forDesc^ DO
         IncrementQuad := IncQuad ;
         StartOfForLoop := Start ;
         EndOfForLoop := End ;
         ForLoopIndex := Sym ;
         IndexTok := idtok
      END ;
      IncludeIndiceIntoIndex (ForInfo, forDesc)
   END
END AddForInfo ;


(*
   CheckForIndex - checks the quadruples: Start..End to see whether a
                   for loop index is manipulated by the programmer.
                   It generates a warning if this is the case.
                   It also checks to see whether the IndexSym is read
                   immediately outside the loop in which case a warning
                   is issued.
*)

PROCEDURE CheckForIndex (forDesc: ForLoopInfo) ;
VAR
   ReadStart, ReadEnd,
   WriteStart, WriteEnd: CARDINAL ;
BEGIN
   GetWriteLimitQuads (forDesc^.ForLoopIndex, RightValue, forDesc^.StartOfForLoop, forDesc^.EndOfForLoop, WriteStart, WriteEnd) ;
   IF (WriteStart < forDesc^.IncrementQuad) AND (WriteStart > forDesc^.StartOfForLoop)
   THEN
      MetaErrorT1 (forDesc^.IndexTok,
                   '{%kFOR} loop index variable {%1Wad} is being manipulated inside the loop',
                   forDesc^.ForLoopIndex) ;
      MetaErrorT1 (QuadToTokenNo (WriteStart),
                   '{%kFOR} loop index variable {%1Wad} is being manipulated, this is considered bad practice and may cause unknown program behaviour',
                   forDesc^.ForLoopIndex)
   END ;
   GetWriteLimitQuads (forDesc^.ForLoopIndex, RightValue, forDesc^.EndOfForLoop, 0, WriteStart, WriteEnd) ;
   GetReadLimitQuads (forDesc^.ForLoopIndex, RightValue, forDesc^.EndOfForLoop, 0, ReadStart, ReadEnd) ;
   IF (ReadStart#0) AND ((ReadStart < WriteStart) OR (WriteStart = 0))
   THEN
      MetaErrorT1 (forDesc^.IndexTok,
                   '{%kFOR} loop index variable {%1Wad} is being read outside the FOR loop (without being reset)',
                   forDesc^.ForLoopIndex) ;
      MetaErrorT1 (QuadToTokenNo (ReadStart),
                   '{%kFOR} loop index variable {%1Wad} is being read outside the FOR loop (without being reset), this is considered extremely bad practice and may cause unknown program behaviour',
                   forDesc^.ForLoopIndex)
   END
END CheckForIndex ;


(*
   GetCurrentFunctionName - returns the name for the current __FUNCTION__
*)

(*
PROCEDURE GetCurrentFunctionName () : Name ;
VAR
   s: String ;
   n: Name ;
BEGIN
   IF CurrentProc=NulSym
   THEN
      s := InitStringCharStar(KeyToCharStar(GetSymName(GetCurrentModule()))) ;
      s := Sprintf1(Mark(InitString('module %s initialization')), s) ;
      n := makekey(string(s)) ;
      s := KillString(s) ;
      RETURN( n )
   ELSE
      RETURN( GetSymName(CurrentProc) )
   END
END GetCurrentFunctionName ;
*)


(*
   BuildRange - generates a RangeCheckOp quad with, r, as its operand.
*)

PROCEDURE BuildRange (r: CARDINAL) ;
BEGIN
   GenQuad (RangeCheckOp, WORD (GetLineNo ()), NulSym, r)
END BuildRange ;


(*
   BuildError - generates a ErrorOp quad, indicating that if this
                quadruple is reachable, then a runtime error would
                occur.
*)

PROCEDURE BuildError (r: CARDINAL) ;
BEGIN
   GenQuad (ErrorOp, WORD (GetLineNo ()), NulSym, r)
END BuildError ;


(*
   CheckPointerThroughNil - builds a range quadruple, providing, sym, is
                            a candidate for checking against NIL.
                            This range quadruple is only expanded into
                            code during the code generation phase
                            thus allowing limited compile time checking.
*)

PROCEDURE CheckPointerThroughNil (tokpos: CARDINAL; sym: CARDINAL) ;
BEGIN
   IF IsVar (sym) AND GetVarPointerCheck (sym)
   THEN
      (* PutVarPointerCheck(sym, FALSE) ;  (* so we do not detect this again *) *)
      BuildRange (InitPointerRangeCheck (tokpos, sym, GetMode (sym) = LeftValue))
   END
END CheckPointerThroughNil ;


(*
   CollectLow - returns the low of the subrange value.
*)

PROCEDURE CollectLow (sym: CARDINAL) : CARDINAL ;
VAR
   low, high: CARDINAL ;
BEGIN
   IF IsSubrange (sym)
   THEN
      GetSubrange (sym, high, low) ;
      RETURN low
   ELSE
      InternalError  ('expecting Subrange symbol')
   END
END CollectLow ;


(*
   CollectHigh - returns the high of the subrange value, sym.
*)

PROCEDURE CollectHigh (sym: CARDINAL) : CARDINAL ;
VAR
   low, high: CARDINAL ;
BEGIN
   IF IsSubrange (sym)
   THEN
      GetSubrange (sym, high, low) ;
      RETURN high
   ELSE
      InternalError  ('expecting Subrange symbol')
   END
END CollectHigh ;


(*
   BackPatchSubrangesAndOptParam - runs through all the quadruples and finds SubrangeLow or SubrangeHigh
                                   quadruples and replaces it by an assignment to the Low or High component
                                   of the subrange type.

                                   Input:
                                   SubrangeLow    op1     op3         (* op3 is a subrange *)

                                   Output:
                                   Becomes        op1     low

                                   Input:
                                   SubrangeHigh   op1     op3         (* op3 is a subrange *)

                                   Output:
                                   Becomes        op1     high

                                   Input:
                                   OptParam       op1     op2    op3

                                   Output:
                                   Param          op1     op2    GetOptArgInit(op3)
*)

PROCEDURE BackPatchSubrangesAndOptParam ;
VAR
   f: QuadFrame ;
   q: CARDINAL ;
BEGIN
   q := GetFirstQuad () ;
   IF q # 0
   THEN
      REPEAT
         f := GetQF (q) ;
         WITH f^ DO
            CASE Operator OF

            SubrangeLowOp :  Operand3 := CollectLow (Operand3) ;
                             Operator := BecomesOp ;
                             ConstExpr := FALSE |
            SubrangeHighOp:  Operand3 := CollectHigh (Operand3) ;
                             Operator := BecomesOp ;
                             ConstExpr := FALSE |
            OptParamOp    :  Operand3 := GetOptArgInit (Operand3) ;
                             Operator := ParamOp

            ELSE
            END ;
            q := Next
         END
      UNTIL q = 0
   END
END BackPatchSubrangesAndOptParam ;


(*
   CheckCompatibleWithBecomes - checks to see that symbol, sym, is
                                compatible with the := operator.
*)

PROCEDURE CheckCompatibleWithBecomes (des, expr,
                                      destok, exprtok: CARDINAL) ;
BEGIN
   IF IsType (des)
   THEN
      MetaErrorT1 (destok,
                   'an assignment cannot assign a value to a type {%1a}', des)
   ELSIF IsProcedure (des)
   THEN
      MetaErrorT1 (destok,
                   'an assignment cannot assign a value to a procedure {%1a}', des)
   ELSIF IsFieldEnumeration (des)
   THEN
      MetaErrorT1 (destok,
                   'an assignment cannot assign a value to an enumeration field {%1a}', des)
   END ;
   IF IsPseudoBaseProcedure (expr) OR IsPseudoBaseFunction (expr)
   THEN
      MetaErrorT1 (exprtok,
                  'an assignment cannot assign a {%1d} {%1a}', expr)
   END
END CheckCompatibleWithBecomes ;


(*
   BuildAssignmentWithoutBounds - calls BuildAssignment but makes sure we do not
                                  check bounds.
*)

PROCEDURE BuildAssignmentWithoutBounds (tok: CARDINAL; checkTypes, checkOverflow: BOOLEAN) ;
VAR
   old: BOOLEAN ;
BEGIN
   old := MustNotCheckBounds ;
   MustNotCheckBounds := TRUE ;
   doBuildAssignment (tok, checkTypes, checkOverflow) ;
   MustNotCheckBounds := old
END BuildAssignmentWithoutBounds ;


(*
   MarkArrayWritten - marks, Array, as being written.
*)

PROCEDURE MarkArrayWritten (Array: CARDINAL) ;
BEGIN
   IF (Array#NulSym) AND IsVarAParam(Array)
   THEN
      PutVarWritten (Array, TRUE)
   END
END MarkArrayWritten ;


(*
   MarkAsReadWrite - marks the variable or parameter as being
                     read/write.
*)

PROCEDURE MarkAsReadWrite (sym: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      PutReadQuad (sym, RightValue, NextQuad) ;
      PutWriteQuad (sym, RightValue, NextQuad)
   END
END MarkAsReadWrite ;


(*
   MarkAsRead - marks the variable or parameter as being read.
*)

PROCEDURE MarkAsRead (sym: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      PutReadQuad (sym, RightValue, NextQuad)
   END
END MarkAsRead ;


(*
   MarkAsWrite - marks the variable or parameter as being written.
*)

PROCEDURE MarkAsWrite (sym: CARDINAL) ;
BEGIN
   IF (sym # NulSym) AND IsVar (sym)
   THEN
      PutWriteQuad (sym, RightValue, NextQuad)
   END
END MarkAsWrite ;


(*
   doVal - return an expression which is VAL(type, expr).  If
           expr is a constant then return expr.
*)

PROCEDURE doVal (type, expr: CARDINAL) : CARDINAL ;
BEGIN
   IF (NOT IsConst (expr)) AND (SkipType (type) # GetDType (expr))
   THEN
      PushTF (Convert, NulSym) ;
      PushT (SkipType(type)) ;
      PushT (expr) ;
      PushT (2) ;          (* Two parameters *)
      BuildConvertFunction (Convert, FALSE) ;
      PopT (expr)
   END ;
   RETURN( expr )
END doVal ;


(*
   MoveWithMode -
*)

PROCEDURE MoveWithMode (tokno: CARDINAL;
                        Des, Exp, Array: CARDINAL;
                        destok, exptok: CARDINAL;
                        checkOverflow: BOOLEAN) ;
VAR
   t: CARDINAL ;
BEGIN
   IF IsConstString(Exp) AND IsConst(Des)
   THEN
      GenQuadOtok (tokno, BecomesOp, Des, NulSym, Exp, TRUE,
                   destok, UnknownTokenNo, exptok) ;
   ELSE
      IF GetMode(Des)=RightValue
      THEN
         IF GetMode(Exp)=LeftValue
         THEN
            CheckPointerThroughNil (tokno, Exp) ;  (*    Des = *Exp    *)
            doIndrX (tokno, Des, Exp)
         ELSE
            GenQuadOtok (tokno, BecomesOp, Des, NulSym, Exp, TRUE,
                         destok, UnknownTokenNo, exptok)
         END
      ELSIF GetMode(Des)=LeftValue
      THEN
         MarkArrayWritten (Array) ;
         IF GetMode(Exp) = LeftValue
         THEN
            t := MakeTemporary (tokno, RightValue) ;
            PutVar(t, GetSType(Exp)) ;
            CheckPointerThroughNil (tokno, Exp) ;
            doIndrX (tokno, t, Exp) ;
            CheckPointerThroughNil (tokno, Des) ;  (*    *Des = Exp    *)
            GenQuadO (tokno, XIndrOp, Des, GetSType (Des), doVal (GetSType (Des), t),
                      checkOverflow)
         ELSE
            CheckPointerThroughNil (tokno, Des) ;  (*    *Des = Exp    *)
            GenQuadO (tokno, XIndrOp, Des, GetSType (Des), doVal (GetSType (Des), Exp),
                      checkOverflow)
         END
      ELSE
         (* This might be inside a const expression.  *)
         GenQuadOTypetok (tokno, BecomesOp,
                          Des, NulSym, Exp,
                          TRUE, TRUE,
                          destok, UnknownTokenNo, exptok)
      END
   END
END MoveWithMode ;


(*
   BuildBuiltinConst - makes reference to a builtin constant within gm2.

                              Entry                 Exit

                       Ptr ->
                              +------------+        +------------+
                              | Ident      |        | Sym        |
                              |------------|        |------------|

                       Quadruple produced:

                       q    Sym  BuiltinConstOp  Ident
*)

PROCEDURE BuildBuiltinConst ;
VAR
   idtok: CARDINAL ;
   Id   : CARDINAL ;
   Sym  : CARDINAL ;
BEGIN
   PopTtok (Id, idtok) ;
   Sym := MakeTemporary (idtok, ImmediateValue) ;
   PutVar (Sym, Integer) ;
(*
   CASE GetBuiltinConstType(KeyToCharStar(Name(Id))) OF

   0:  ErrorFormat1(NewError(GetTokenNo()),
                    '%a unrecognised builtin constant', Id) |
   1:  PutVar(Sym, Integer) |
   2:  PutVar(Sym, Real)

   ELSE
      InternalError ('unrecognised value')
   END ;
*)
   GenQuadO (idtok, BuiltinConstOp, Sym, NulSym, Id, FALSE) ;
   PushTtok (Sym, idtok)
END BuildBuiltinConst ;


(*
   BuildBuiltinTypeInfo - make reference to a builtin typeinfo function
                          within gm2.

                                 Entry                 Exit

                          Ptr ->
                                 +-------------+
                                 | Type        |
                                 |-------------|       +------------+
                                 | Ident       |       | Sym        |
                                 |-------------|       |------------|

                          Quadruple produced:

                          q    Sym  BuiltinTypeInfoOp  Type Ident
*)

PROCEDURE BuildBuiltinTypeInfo ;
VAR
   idtok: CARDINAL ;
   Ident,
   Type,
   Sym  : CARDINAL ;
BEGIN
   PopTtok (Ident, idtok) ;
   PopT (Type) ;
   Sym := MakeTemporary (BuiltinTokenNo, ImmediateValue) ;
   CASE GetBuiltinTypeInfoType (KeyToCharStar (Name (Ident))) OF

   0:  ErrorFormat1 (NewError(idtok),
                     '%a unrecognised builtin constant', Ident) |
   1:  PutVar (Sym, Boolean) |
   2:  PutVar (Sym, ZType) |
   3:  PutVar (Sym, RType)

   ELSE
      InternalError ('unrecognised value')
   END ;
   GenQuadO (idtok, BuiltinTypeInfoOp, Sym, Type, Ident, FALSE) ;
   PushTtok (Sym, idtok)
END BuildBuiltinTypeInfo ;


(*
   CheckBecomesMeta - checks to make sure that we are not
                      assigning a variable to a constant.
                      Also check we are not assigning to an
                      unbounded array.
*)

PROCEDURE CheckBecomesMeta (Des, Exp: CARDINAL; combinedtok, destok, exprtok: CARDINAL) ;
BEGIN
   IF IsConst (Des) AND IsVar (Exp)
   THEN
      MetaErrorsT2 (combinedtok,
                    'in assignment, cannot assign a variable {%2a} to a constant {%1a}',
                    'designator {%1Da} is declared as a {%kCONST}', Des, Exp)
   END ;
   IF (GetDType(Des) # NulSym) AND IsVar (Des) AND IsUnbounded (GetDType (Des))
   THEN
      MetaErrorT1 (destok,
                   'in assignment, cannot assign to an unbounded array {%1ad}', Des)
   END ;
   IF (GetDType(Exp) # NulSym) AND IsVar (Exp) AND IsUnbounded (GetDType (Exp))
   THEN
      MetaErrorT1 (exprtok,
                  'in assignment, cannot assign from an unbounded array {%1ad}', Exp)
   END
END CheckBecomesMeta ;


(*
   BuildAssignment - Builds an assignment from the values given on the
                     quad stack. Either an assignment to an
                     arithmetic expression or an assignment to a
                     boolean expression.  This procedure should not
                     be called in CONST declarations.
                     The Stack is expected to contain:


       Either

                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | Expression |
                     |------------|
                     | Designator |
                     |------------|          +------------+
                     |            |          |            |  <- Ptr
                     |------------|          |------------|


                     Quadruples Produced

                     q     BecomesOp  Designator  _  Expression

       OR

                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | True |False|
                     |------------|
                     | Designator |
                     |------------|          +------------+
                     |            |          |            |  <- Ptr
                     |------------|          |------------|


                     Quadruples Produced

                     q     BecomesOp  Designator  _  TRUE
                     q+1   GotoOp                    q+3
                     q+2   BecomesOp  Designator  _  FALSE

*)

PROCEDURE BuildAssignment (becomesTokNo: CARDINAL) ;
VAR
   des, exp   : CARDINAL ;
   destok,
   exptok,
   combinedtok: CARDINAL ;
BEGIN
   des := OperandT (2) ;
   IF IsReadOnly (des)
   THEN
      destok := OperandTok (2) ;
      exptok := OperandTok (1) ;
      exp := OperandT (1) ;
      IF DebugTokPos
      THEN
         MetaErrorT1 (destok, 'destok {%1Ead}', des) ;
         MetaErrorT1 (exptok, 'exptok {%1Ead}', exp)
      END ;
      combinedtok := MakeVirtualTok (becomesTokNo, destok, exptok) ;
      IF DebugTokPos
      THEN
         MetaErrorT1 (combinedtok, 'combined {%1Ead}', des)
      END ;
      IF IsBoolean (1)
      THEN
         MetaErrorT1 (combinedtok,
                      'cannot assign expression to a constant designator {%1Ead}', des)
      ELSE
         exp := OperandT (1) ;
         MetaErrorT2 (combinedtok,
                      'cannot assign a constant designator {%1Ead} with an expression {%2Ead}',
                      des, exp)
      END ;
      PopN (2)  (* Remove both parameters.  *)
   ELSIF IsError (des)
   THEN
      PopN (2)  (* Remove both parameters.  *)
   ELSE
      doBuildAssignment (becomesTokNo, TRUE, TRUE)
   END
END BuildAssignment ;


(*
   BuildAssignConstant - used to create constant in the CONST declaration.
                         The stack is expected to contain:

       Either

                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | Expression |
                     |------------|
                     | Designator |
                     |------------|          +------------+
                     |            |          |            |  <- Ptr
                     |------------|          |------------|


                     Quadruples Produced

                     q     BecomesOp  Designator  _  Expression

       OR

                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | True |False|
                     |------------|
                     | Designator |
                     |------------|          +------------+
                     |            |          |            |  <- Ptr
                     |------------|          |------------|


                     Quadruples Produced

                     q     BecomesOp  Designator  _  TRUE
                     q+1   GotoOp                    q+3
                     q+2   BecomesOp  Designator  _  FALSE
*)

PROCEDURE BuildAssignConstant (equalsTokNo: CARDINAL) ;
BEGIN
   doBuildAssignment (equalsTokNo, TRUE, TRUE)
END BuildAssignConstant ;


(*
   doBuildAssignment - subsiduary procedure of BuildAssignment.
                       It builds the assignment and optionally
                       checks the types are compatible.
*)

PROCEDURE doBuildAssignment (becomesTokNo: CARDINAL; checkTypes, checkOverflow: BOOLEAN) ;
VAR
   r, w,
   t, f,
   Array,
   Des, Exp      : CARDINAL ;
   combinedtok,
   destok, exptok: CARDINAL ;
BEGIN
   DisplayStack ;
   IF IsBoolean (1)
   THEN
      PopBool (t, f) ;
      PopTtok (Des, destok) ;
      PutVarConditional (Des, TRUE) ;  (* Des will contain the result of a boolean relop.  *)
      (* Conditional Boolean Assignment.  *)
      BackPatch (t, NextQuad) ;
      IF GetMode (Des) = LeftValue
      THEN
         CheckPointerThroughNil (destok, Des) ;
         GenQuadO (destok, XIndrOp, Des, Boolean, True, checkOverflow) ;
         GenQuadO (destok, GotoOp, NulSym, NulSym, NextQuad+2, FALSE) ;
      ELSE
         (* This might be inside a const expression.  *)
         GenQuadO (becomesTokNo, BecomesOp, Des, NulSym, True, checkOverflow) ;
         GenQuadO (destok, GotoOp, NulSym, NulSym, NextQuad+2, FALSE)
      END ;
      BackPatch (f, NextQuad) ;
      IF GetMode (Des) = LeftValue
      THEN
         CheckPointerThroughNil (destok, Des) ;
         GenQuadO (destok, XIndrOp, Des, Boolean, False, checkOverflow)
      ELSE
         GenQuadO (becomesTokNo, BecomesOp, Des, NulSym, False, checkOverflow)
      END
   ELSE
      PopTrwtok (Exp, r, exptok) ;
      MarkAsRead (r) ;
      IF Exp = NulSym
      THEN
         MetaError0 ('{%E}unknown expression found during assignment') ;
         FlushErrors
      END ;
      Array := OperandA (1) ;
      PopTrwtok (Des, w, destok) ;
      MarkAsWrite (w) ;
      CheckCompatibleWithBecomes (Des, Exp, destok, exptok) ;
      IF DebugTokPos
      THEN
         MetaErrorT1 (becomesTokNo, 'becomestok {%1Oad}', Des) ;
         MetaErrorT1 (destok, 'destok {%1Oad}', Des) ;
         MetaErrorT1 (exptok, 'exptok {%1Oad}', Exp)
      END ;
      combinedtok := MakeVirtualTok (becomesTokNo, destok, exptok) ;
      IF DebugTokPos
      THEN
         MetaErrorT1 (combinedtok, 'combined {%1Oad}', Des)
      END ;
      IF (GetSType (Des) # NulSym) AND (NOT IsSet (GetDType (Des)))
      THEN
         (* Tell code generator to test runtime values of assignment so ensure we
            catch overflow and underflow.  *)
         BuildRange (InitAssignmentRangeCheck (combinedtok, Des, Exp, destok, exptok))
      END ;
      IF checkTypes
      THEN
         CheckBecomesMeta (Des, Exp, combinedtok, destok, exptok)
      END ;
      (* Simple assignment.  *)
      MoveWithMode (combinedtok, Des, Exp, Array, destok, exptok, checkOverflow) ;
      IF checkTypes
      THEN
         CheckAssignCompatible (Des, Exp, combinedtok, destok, exptok)
      END
   END ;
   DisplayStack
END doBuildAssignment ;


(*
   CheckAssignCompatible - checks to see that an assignment is compatible.
                           It performs limited checking - thorough checking
                           is done in pass 3.  But we do what we can here
                           given knowledge so far.
*)

PROCEDURE CheckAssignCompatible (Des, Exp: CARDINAL; combinedtok, destok, exprtok: CARDINAL) ;
VAR
   DesT, ExpT, DesL: CARDINAL ;
BEGIN
   DesT := GetSType(Des) ;
   ExpT := GetSType(Exp) ;
   DesL := GetLType(Des) ;
   IF IsProcedure(Exp) AND
      ((DesT#NulSym) AND (NOT IsProcType(DesT))) AND
      ((DesL#NulSym) AND (NOT IsProcType(DesL)))
   THEN
      MetaErrorT1 (destok,
                  'incorrectly assigning a procedure to a designator {%1Ead} (designator is not a procedure type, {%1ast})', Des)
   ELSIF IsProcedure (Exp) AND IsProcedureNested (Exp)
   THEN
      MetaErrorT1 (exprtok,
                   'cannot call nested procedure {%1Ead} indirectly as the outer scope will not be known', Exp)
   ELSIF IsConstString(Exp)
   THEN
   ELSIF (DesT#NulSym) AND (IsUnbounded(DesT))
   THEN
   ELSIF (ExpT#NulSym) AND (IsUnbounded(ExpT))
   THEN
   ELSIF (DesL#NulSym) AND IsArray(DesL)
   THEN
   ELSIF IsConstructor(Exp)
   THEN
      IF ExpT=NulSym
      THEN
         (* ignore type checking *)
      ELSIF (DesT=NulSym) AND IsConst(Des) AND (IsConstructor(Des) OR IsConstSet(Des))
      THEN
         PutConst(Des, ExpT)
      ELSIF NOT IsAssignmentCompatible(DesT, ExpT)
      THEN
         MetaErrorT1 (combinedtok,
                      'constructor expression is not compatible during assignment to {%1Ead}', Des)
      END
   ELSIF (DesT#NulSym) AND IsSet(DesT) AND IsConst(Exp)
   THEN
      (* We ignore checking of these types in pass 3 - but we do check them thoroughly post pass 3 *)
   ELSIF IsConst(Exp) AND (ExpT#Address) AND (NOT IsConst(Des)) AND
         (DesL#NulSym) AND ((DesL=Cardinal) OR (NOT IsSubrange(DesL))) AND
         (NOT IsEnumeration(DesL))
   THEN
      IF (IsBaseType(DesL) OR IsSystemType(DesL))
      THEN
         CheckAssignmentCompatible (combinedtok, ExpT, DesT)
      ELSE
         MetaErrorT2 (combinedtok,
                      'assignment of a constant {%1Ead} can only be made to a variable whose type is equivalent to a Modula-2 base type {%2tsa}', Exp, Des)
      END
   ELSE
      IF (DesT#NulSym) AND IsProcType(DesT) AND IsProcedure(Exp)
      THEN
         DesT := GetSType(DesT) ; (* we can at least check RETURN values of procedure variables *)
         (* remember that thorough assignment checking is done post pass 3 *)
         CheckAssignmentCompatible (combinedtok, ExpT, DesT)
      END
   END
END CheckAssignCompatible ;


(*
   CheckBooleanId - Checks to see if the top operand is a boolean.
                    If the operand is not a boolean then it is tested
                    with true and a boolean is generated.
                    The Stack:


                    Entry                     Exit
             Ptr ->                                          <- Ptr
                    +------------+            +------------+
                    | Sym        |            | t   | f    |
                    |------------|            |------------|

                     Quadruples

                     q   If=      Sym   True   _
                     q+1 GotoOp   _     _      _
*)

PROCEDURE CheckBooleanId ;
VAR
   tok: CARDINAL ;
BEGIN
   IF NOT IsBoolean (1)
   THEN
      tok := OperandTok (1) ;
      IF IsVar (OperandT (1))
      THEN
         IF GetSType (OperandT (1)) # Boolean
         THEN
            MetaError1 ('{%1Ua:is not a boolean expression}' +
                        '{!%1Ua:boolean expression expected}', OperandT (1))
         END
      END ;
      PushT (EqualTok) ;
      PushT (True) ;
      BuildRelOp (tok)
   END
END CheckBooleanId ;


(*
   BuildAlignment - builds an assignment to an alignment constant.

                    The Stack is expected to contain:


                            Entry                   Exit
                            =====                   ====

                    Ptr ->
                            +---------------+
                            | Expression    |
                            |---------------|
                            | bytealignment |
                            |---------------|       empty
*)

PROCEDURE BuildAlignment (tokno: CARDINAL) ;
VAR
   name : Name ;
   expr,
   align: CARDINAL ;
BEGIN
   PopT (expr) ;
   PopT (name) ;
   IF name # MakeKey ('bytealignment')
   THEN
      MetaError1 ('expecting bytealignment identifier, rather than {%1Ea}',
                  MakeError (tokno, name))
   END ;
   GetConstFromFifoQueue (align) ;
   PushT (align) ;
   PushT (expr) ;
   BuildAssignConstant (tokno)
END BuildAlignment ;


(*
   BuildBitLength - builds an assignment to a bit length constant.

                    The Stack is expected to contain:


                           Entry                   Exit
                           =====                   ====

                    Ptr ->
                           +------------+
                           | Expression |
                           |------------|          empty
*)

PROCEDURE BuildBitLength (tokno: CARDINAL) ;
VAR
   expr,
   length: CARDINAL ;
BEGIN
   PopT (expr) ;
   GetConstFromFifoQueue (length) ;
   PushT (length) ;
   PushT (expr) ;
   BuildAssignConstant (tokno)
END BuildBitLength ;


(*
   BuildDefaultFieldAlignment - builds an assignment to an alignment constant.

                                The Stack is expected to contain:


                                       Entry                   Exit
                                       =====                   ====

                                Ptr ->
                                       +------------+
                                       | Expression |
                                       |------------|          empty
*)

PROCEDURE BuildDefaultFieldAlignment ;
VAR
   expr,
   align: CARDINAL ;
   name : Name ;
BEGIN
   PopT (expr) ;
   PopT (name) ;
   IF name # MakeKey ('bytealignment')
   THEN
      MetaError0 ('{%E}only allowed to use the attribute {%kbytealignment} in the default record field alignment pragma')
   END ;
   GetConstFromFifoQueue (align) ;
   PushT (align) ;
   PushT (expr) ;
   BuildAssignConstant (GetTokenNo ())
END BuildDefaultFieldAlignment ;


(*
   BuildPragmaField - builds an assignment to an alignment constant.

                      The Stack is expected to contain:


                      Entry                   Exit
                      =====                   ====

               Ptr ->
                      +------------+
                      | Expression |
                      |------------|          empty
*)

PROCEDURE BuildPragmaField ;
VAR
   expr,
   const: CARDINAL ;
   name : Name ;
BEGIN
   PopT (expr) ;
   PopT (name) ;
   IF (name # MakeKey ('unused')) AND (name # MakeKey ('bytealignment'))
   THEN
      MetaError0 ('only allowed to use the attribute {%Ekbytealignment} in the default record field alignment pragma')
   END ;
   IF expr # NulSym
   THEN
      GetConstFromFifoQueue (const) ;
      PushT (const) ;
      PushT (expr) ;
      BuildAssignConstant (GetTokenNo ())
   END
END BuildPragmaField ;


(*
   BuildRepeat - Builds the repeat statement from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====


                 Empty
                                                        <- Ptr
                                         +------------+
                                         | RepeatQuad |
                                         |------------|

*)

PROCEDURE BuildRepeat ;
BEGIN
   PushT(NextQuad)
END BuildRepeat ;


(*
   BuildUntil - Builds the until part of the repeat statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

        Ptr ->
                +------------+
                | t   | f    |
                |------------|
                | RepeatQuad |          Empty
                |------------|
*)

PROCEDURE BuildUntil ;
VAR
   t, f,
   Repeat: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   PopT(Repeat) ;
   BackPatch(f, Repeat) ;          (* If False then keep on repeating *)
   BackPatch(t, NextQuad) ;        (* If True then exit repeat        *)
END BuildUntil ;


(*
   BuildWhile - Builds the While part of the While statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

                                                       <- Ptr
                                        |------------|
                Empty                   | WhileQuad  |
                                        |------------|
*)

PROCEDURE BuildWhile ;
BEGIN
   PushT(NextQuad)
END BuildWhile ;


(*
   BuildDoWhile - Builds the Do part of the while statement
                  from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

          Ptr ->
                  +------------+          +------------+
                  | t   | f    |          | 0    | f   |
                  |------------|          |------------|
                  | WhileQuad  |          | WhileQuad  |
                  |------------|          |------------|

                  Quadruples

                  BackPatch t exit to the NextQuad
*)

PROCEDURE BuildDoWhile ;
VAR
   t, f: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   PushBool(0, f)
END BuildDoWhile ;


(*
   BuildEndWhile - Builds the end part of the while statement
                   from the quad stack.
                   The Stack is expected to contain:


                   Entry                   Exit
                   =====                   ====

           Ptr ->
                   +------------+
                   | t   | f    |
                   |------------|
                   | WhileQuad  |          Empty
                   |------------|

                   Quadruples

                   q    GotoOp  WhileQuad
                   False exit is backpatched with q+1
*)

PROCEDURE BuildEndWhile ;
VAR
   While,
   t, f : CARDINAL ;
BEGIN
   PopBool(t, f) ;
   Assert(t=0) ;
   PopT(While) ;
   GenQuad(GotoOp, NulSym, NulSym, While) ;
   BackPatch(f, NextQuad)
END BuildEndWhile ;


(*
   BuildLoop - Builds the Loop part of the Loop statement
               from the quad stack.
               The Stack is expected to contain:


               Entry                   Exit
               =====                   ====

                                                      <- Ptr
               Empty                   +------------+
                                       | LoopQuad   |
                                       |------------|
*)

PROCEDURE BuildLoop ;
BEGIN
   PushT(NextQuad) ;
   PushExit(0)       (* Seperate Exit Stack for loop end *)
END BuildLoop ;


(*
   BuildExit - Builds the Exit part of the Loop statement.
*)

PROCEDURE BuildExit ;
BEGIN
   IF IsEmptyWord(ExitStack)
   THEN
      MetaError0 ('{%EkEXIT} is only allowed in a {%kLOOP} statement')
   ELSE
      GenQuad(GotoOp, NulSym, NulSym, 0) ;
      PushExit(Merge(PopExit(), NextQuad-1))
   END
END BuildExit ;


(*
   BuildEndLoop - Builds the End part of the Loop statement
                  from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

          Ptr ->
                  +------------+
                  | LoopQuad   |          Empty
                  |------------|

                  Quadruples

                  Goto  _  _  LoopQuad
*)

PROCEDURE BuildEndLoop ;
VAR
   Loop: CARDINAL ;
BEGIN
   PopT(Loop) ;
   GenQuad(GotoOp, NulSym, NulSym, Loop) ;
   BackPatch(PopExit(), NextQuad)
END BuildEndLoop ;


(*
   BuildThenIf - Builds the Then part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->                                          <- Ptr
                 +------------+          +------------+
                 | t   | f    |          | 0    | f   |
                 |------------|          |------------|

                 Quadruples

                 The true exit is BackPatched to point to
                 the NextQuad.
*)

PROCEDURE BuildThenIf ;
VAR
   t, f: CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   PushBool(0, f)
END BuildThenIf ;


(*
   BuildElse - Builds the Else part of the If statement
               from the quad stack.
               The Stack is expected to contain:


               Entry                   Exit
               =====                   ====

       Ptr ->
               +------------+          +------------+
               | t   | f    |          | t+q  | 0   |
               |------------|          |------------|

               Quadruples

               q    GotoOp  _  _  0
               q+1  <- BackPatched from f
*)

PROCEDURE BuildElse ;
VAR
   t, f: CARDINAL ;
BEGIN
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   PushBool(Merge(t, NextQuad-1), 0)   (* NextQuad-1 = Goto Quad *)
END BuildElse ;


(*
   BuildEndIf - Builds the End part of the If statement
                from the quad stack.
                The Stack is expected to contain:


                Entry                   Exit
                =====                   ====

        Ptr ->
                +------------+
                | t   | f    |          Empty
                |------------|

                Quadruples

                Both t and f are backpatched to point to the NextQuad
*)

PROCEDURE BuildEndIf ;
VAR
   t, f: CARDINAL ;
BEGIN
   PopBool(t, f) ;
   BackPatch(t, NextQuad) ;
   BackPatch(f, NextQuad)
END BuildEndIf ;


(*
   BuildElsif1 - Builds the Elsif part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +------------+          +------------+
                 | t   | f    |          | t+q  | 0   |
                 |------------|          |------------|

                 Quadruples

                 q    GotoOp  _  _  0
                 q+1  <- BackPatched from f
*)

PROCEDURE BuildElsif1 ;
VAR
   t, f: CARDINAL ;
BEGIN
   GenQuad(GotoOp, NulSym, NulSym, 0) ;
   PopBool(t, f) ;
   BackPatch(f, NextQuad) ;
   PushBool(Merge(t, NextQuad-1), 0)   (* NextQuad-1 = Goto Quad *)
END BuildElsif1 ;


(*
   BuildElsif2 - Builds the Elsif until part of the If statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

          Ptr ->
                 +--------------+
                 | 0    | f1    |                            <- Ptr
                 |--------------|          +---------------+
                 | t2   | f2    |          | t2    | f1+f2 |
                 |--------------|          |---------------|
*)

PROCEDURE BuildElsif2 ;
VAR
   t1, f1,
   t2, f2: CARDINAL ;
BEGIN
   PopBool(t1, f1) ;
   Assert(t1=0) ;
   PopBool(t2, f2) ;
   PushBool(t2, Merge(f1, f2))
END BuildElsif2 ;


(*
   PushOne - pushes the value one to the stack.
             The Stack is changed:


                    Entry                   Exit
                    =====                   ====

                                                            <- Ptr
                                            +------------+
             Ptr ->                         | 1 | type   |
                                            |------------|
*)

PROCEDURE PushOne (tok: CARDINAL; type: CARDINAL;
                   message: ARRAY OF CHAR) ;
VAR
   const: CARDINAL ;
BEGIN
   IF type = NulSym
   THEN
      const := MakeConstLit (tok, MakeKey('1'), NulSym) ;
      PutConstLitInternal (const, TRUE) ;
      PushTFtok (const, NulSym, tok)
   ELSIF IsEnumeration (type)
   THEN
      IF NoOfElements (type) = 0
      THEN
         MetaErrorString1 (ConCat (InitString ('enumeration type only has one element {%1Dad} and therefore '),
                                   Mark (InitString (message))),
                           type) ;
         PushZero (tok, type)
      ELSE
         PushTFtok (Convert, NulSym, tok) ;
         PushT (type) ;
         PushTFtok (MakeConstLit (tok, MakeKey ('1'), ZType), ZType, tok) ;
         PushT (2) ;          (* Two parameters *)
         BuildConvertFunction (Convert, TRUE)
      END
   ELSE
      const := MakeConstLit (tok, MakeKey ('1'), type) ;
      PutConstLitInternal (const, TRUE) ;
      PushTFtok (const, type, tok)
   END
END PushOne ;


(*
   PushZero - pushes the value zero to the stack.
              The Stack is changed:


                    Entry                   Exit
                    =====                   ====

                                                            <- Ptr
                                            +------------+
             Ptr ->                         | 0 | type   |
                                            |------------|
*)

PROCEDURE PushZero (tok: CARDINAL; type: CARDINAL) ;
BEGIN
   IF type = NulSym
   THEN
      PushTFtok (MakeConstLit (tok, MakeKey ('0'), NulSym), NulSym, tok)
   ELSIF IsEnumeration (type)
   THEN
      PushTFtok (Convert, NulSym, tok) ;
      PushTtok (type, tok) ;
      PushTtok (MakeConstLit (tok, MakeKey ('0'), ZType), tok) ;
      PushT (2) ;          (* Two parameters *)
      BuildConvertFunction (Convert, TRUE)
   ELSE
      PushTFtok (MakeConstLit (tok, MakeKey ('0'), type), type, tok)
   END
END PushZero ;


(*
   BuildPseudoBy - Builds the Non existant part of the By
                   clause of the For statement
                   from the quad stack.
                   The Stack is expected to contain:


                   Entry                   Exit
                   =====                   ====

                                                           <- Ptr
                                           +------------+
            Ptr ->                         | BySym | t  |
                   +------------+          |------------|
                   | e    | t   |          | e     | t  |
                   |------------|          |------------|
*)

PROCEDURE BuildPseudoBy ;
VAR
   expr, type, dotok: CARDINAL ;
BEGIN
   (* As there is no BY token this position is the DO at the end of the last expression.  *)
   PopTFtok (expr, type, dotok) ;
   PushTFtok (expr, type, dotok) ;
   IF type = NulSym
   THEN
      (* type := ZType *)
   ELSIF IsEnumeration (SkipType (type)) OR (SkipType (type) = Char)
   THEN
      (* Use type.  *)
   ELSIF IsOrdinalType (SkipType (type))
   THEN
      type := ZType
   END ;
   PushOne (dotok, type,
            'the implied {%kFOR} loop increment will cause an overflow {%1ad}')
END BuildPseudoBy ;


(*
   BuildForLoopToRangeCheck - builds the range check to ensure that the id
                              does not exceed the limits of its type.
*)

PROCEDURE BuildForLoopToRangeCheck ;
VAR
   d, dt,
   e, et: CARDINAL ;
BEGIN
   PopTF (e, et) ;
   PopTF (d, dt) ;
   BuildRange (InitForLoopToRangeCheck (d, e)) ;
   PushTF (d, dt) ;
   PushTF (e, et)
END BuildForLoopToRangeCheck ;


(*
   ForLoopLastIteratorVariable - assigns the last value of the index variable to
                                 symbol LastIterator.
                                 The For Loop is regarded:

                                 For ident := e1 To e2 By BySym Do

                                 End
*)

PROCEDURE ForLoopLastIteratorVariable (LastIterator, e1, e2, BySym, ByType: CARDINAL ;
                                       e1tok, e2tok, bytok: CARDINAL) ;
VAR
   PBType,
   PositiveBy,
   ElseQuad,
   t, f      : CARDINAL ;
BEGIN
   Assert (IsVar (LastIterator)) ;
   (* If By > 0 then.  *)
   (* q+1 if >=      by        0  q+3.  *)
   (* q+2 GotoOp                  q+else.   *)
   PushTFtok (BySym, ByType, bytok) ;  (* BuildRelOp  1st parameter *)
   PushT (GreaterEqualTok) ;           (*             2nd parameter *)
                                       (* 3rd parameter *)
   PushZero (bytok, ByType) ;
   BuildRelOp (e2tok) ;       (* Choose final expression position.  *)
   PopBool (t, f) ;
   BackPatch (t, NextQuad) ;

   (* LastIterator := ((e2-e1) DIV By) * By + e1.  *)
   PushTF (LastIterator, GetSType (LastIterator)) ;
   PushTFtok (e2, GetSType (e2), e2tok) ;
   PushT (MinusTok) ;
   PushTFtok (e1, GetSType (e1), e1tok) ;
   doBuildBinaryOp (TRUE, FALSE) ;
   PushT (DivideTok) ;
   PushTFtok (BySym, ByType, bytok) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   PushT (TimesTok) ;
   PushTFtok (BySym, ByType, bytok) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   PushT (ArithPlusTok) ;
   PushTFtok (e1, GetSType (e1), e1tok) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   BuildForLoopToRangeCheck ;
   BuildAssignmentWithoutBounds (e1tok, FALSE, FALSE) ;
   GenQuad (GotoOp, NulSym, NulSym, 0) ;
   ElseQuad := NextQuad-1 ;

   (* Else.  *)

   BackPatch (f, NextQuad) ;

   PushTtok (MinusTok, bytok) ;
   PushTFtok (BySym, ByType, bytok) ;
   BuildUnaryOp ;
   PopTF (PositiveBy, PBType) ;  (* PositiveBy := - BySym.  *)

   (* LastIterator := e1 - ((e1-e2) DIV PositiveBy) * PositiveBy.  *)
   PushTF (LastIterator, GetSType (LastIterator)) ;
   PushTFtok (e1, GetSType (e1), e1tok) ;
   PushT (MinusTok) ;
   PushTFtok (e1, GetSType (e1), e1tok) ;
   PushT (MinusTok) ;
   PushTFtok (e2, GetSType (e2), e2tok) ;
   doBuildBinaryOp (TRUE, FALSE) ;
   PushT (DivideTok) ;
   PushTFtok (PositiveBy, ByType, bytok) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   PushT (TimesTok) ;
   PushTFtok (PositiveBy, ByType, bytok) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   BuildForLoopToRangeCheck ;
   BuildAssignmentWithoutBounds (e1tok, FALSE, FALSE) ;
   BackPatch (ElseQuad, NextQuad) ;

   (* End.  *)
END ForLoopLastIteratorVariable ;


(*
   ForLoopLastIteratorConstant - assigns the last value of the index variable to
                                 symbol LastIterator.
                                 The For Loop is regarded:

                                 For ident := e1 To e2 By BySym Do

                                 End
*)

PROCEDURE ForLoopLastIteratorConstant (LastIterator, e1, e2, BySym, ByType: CARDINAL;
                                       e1tok, e2tok, bytok: CARDINAL) ;
BEGIN
   Assert (IsConst (LastIterator)) ;
   (* LastIterator := VAL (GetType (LastIterator), ((e2-e1) DIV By) * By + e1)  *)
   PushTF (LastIterator, GetSType (LastIterator)) ;
   PushTFtok (e2, GetSType (e2), e2tok) ;
   PushT (MinusTok) ;
   PushTFtok (e1, GetSType (e1), e1tok) ;
   doBuildBinaryOp (TRUE, FALSE) ;
   PushT (DivideTok) ;
   PushTFtok (BySym, ByType, bytok) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   PushT (TimesTok) ;
   PushTFtok (BySym, ByType, bytok) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   PushT (ArithPlusTok) ;
   PushTFtok (e1, GetSType (e1), e1tok) ;
   doBuildBinaryOp (FALSE, FALSE) ;
   BuildForLoopToRangeCheck ;
   BuildAssignmentWithoutBounds (e1tok, FALSE, FALSE)
END ForLoopLastIteratorConstant ;


(*
   ForLoopLastIterator - calculate the last iterator value but avoid setting
                         LastIterator twice if it is a constant (in the quads).
                         In the ForLoopLastIteratorVariable case only one
                         path will be chosen but at the time of quadruple
                         generation we do not know the value of BySym.
*)

PROCEDURE ForLoopLastIterator (LastIterator, e1, e2, BySym, ByType: CARDINAL ;
                               e1tok, e2tok, bytok: CARDINAL) ;
BEGIN
   IF IsVar (LastIterator)
   THEN
      ForLoopLastIteratorVariable (LastIterator, e1, e2, BySym, ByType,
                                   e1tok, e2tok, bytok)
   ELSE
      ForLoopLastIteratorConstant (LastIterator, e1, e2, BySym, ByType,
                                   e1tok, e2tok, bytok)
   END
END ForLoopLastIterator ;


(*
   BuildForToByDo - Builds the For To By Do part of the For statement
                    from the quad stack.
                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====

                                                               <- Ptr
                                            +----------------+
             Ptr ->                         | RangeId        |
                    +----------------+      |----------------|
                    | BySym | ByType |      | ForQuad        |
                    |----------------|      |----------------|
                    | e2             |      | LastValue      |
                    |----------------|      |----------------|
                    | e1             |      | BySym | ByType |
                    |----------------|      |----------------|
                    | Ident          |      | IdentSym       |
                    |----------------|      |----------------|


                    x := e1 ;
                    LASTVALUE := ((e2-e1) DIV BySym) * BySym + e1
                    IF BySym<0
                    THEN
                       IF e1<e2
                       THEN
                          goto exit
                       END
                    ELSE
                       IF e1>e2
                       THEN
                          goto exit
                       END
                    END ;
                    LOOP
                       body
                       IF x=LASTVALUE
                       THEN
                          goto exit
                       END ;
                       INC(x, BySym)
                    END

                    Quadruples:

                    q     BecomesOp  IdentSym  _  e1
                    q+    LastValue  := ((e1-e2) DIV by) * by + e1
                    q+1   if >=      by        0  q+..2
                    q+2   GotoOp                  q+3
                    q+3   If >=      e1  e2       q+5
                    q+4   GotoOp                  exit
                    q+5   ..
                    q+..1 Goto                    q+..5
                    q+..2 If >=      e2  e1       q+..4
                    q+..3 GotoOp                  exit
                    q+..4 ..

                    The For Loop is regarded:

                    For ident := e1 To e2 By by Do

                    End
*)

PROCEDURE BuildForToByDo ;
VAR
   l1, l2    : LineNote ;
   e1, e2,
   Id        : Name ;
   e1tok,
   e2tok,
   idtok,
   bytok     : CARDINAL ;
   LastIterator,
   exit1,
   IdSym,
   BySym,
   ByType,
   ForLoop,
   RangeId,
   t, f      : CARDINAL ;
   etype,
   t1        : CARDINAL ;
BEGIN
   l2 := PopLineNo() ;
   l1 := PopLineNo() ;
   UseLineNote(l1) ;
   PushFor (0) ;
   PopTFtok (BySym, ByType, bytok) ;
   PopTtok (e2, e2tok) ;
   PopTtok (e1, e1tok) ;
   PopTtok (Id, idtok) ;
   IdSym := RequestSym (idtok, Id) ;
   RangeId := InitForLoopBeginRangeCheck (IdSym, idtok, e1, e1tok, e2, e2tok, BySym, bytok) ;
   BuildRange (RangeId) ;
   PushTtok (IdSym, idtok) ;
   PushTtok (e1, e1tok) ;
   BuildAssignmentWithoutBounds (idtok, TRUE, TRUE) ;

   UseLineNote (l2) ;
   LastIterator := MakeTemporary (e2tok,
                                  AreConstant (IsConst (e1) AND IsConst (e2) AND
                                               IsConst (BySym))) ;
   PutVar (LastIterator, GetSType (IdSym)) ;
   etype := MixTypes (GetSType (e1), GetSType (e2), e2tok) ;
   e1 := doConvert (etype, e1) ;
   e2 := doConvert (etype, e2) ;

   ForLoopLastIterator (LastIterator, e1, e2, BySym, ByType, e1tok, e2tok, bytok) ;

   (* q+1 if >=      by        0  q+..2 *)
   (* q+2 GotoOp                  q+3   *)
   PushTFtok (BySym, ByType, bytok) ;  (* BuildRelOp  1st parameter.  *)
   PushT (GreaterEqualTok) ;           (*             2nd parameter.  *)
                                       (* 3rd parameter.  *)
   PushZero (bytok, ByType) ;

   BuildRelOp (e2tok) ;           (* Choose final expression position.  *)
   PopBool (t, f) ;
   BackPatch (f, NextQuad) ;
   (* q+3 If >=       e1  e2      q+5  *)
   (* q+4 GotoOp                  Exit *)
   PushTFtok (e1, GetSType (e1), e1tok) ;  (* BuildRelOp  1st parameter *)
   PushT (GreaterEqualTok) ;               (*             2nd parameter *)
   PushTFtok (e2, GetSType (e2), e2tok) ;  (*             3rd parameter *)
   BuildRelOp (e2tok) ;           (* Choose final expression position.  *)
   PopBool (t1, exit1) ;
   BackPatch (t1, NextQuad) ;
   PushFor (Merge (PopFor (), exit1)) ;    (* Merge exit1.  *)

   GenQuad (GotoOp, NulSym, NulSym, 0) ;
   ForLoop := NextQuad-1 ;

   (* ELSE.  *)

   BackPatch (t, NextQuad) ;
   PushTFtok (e2, GetSType(e2), e2tok) ; (* BuildRelOp  1st parameter *)
   PushT (GreaterEqualTok) ;             (*             2nd parameter *)
   PushTFtok (e1, GetSType(e1), e1tok) ; (*             3rd parameter *)
   BuildRelOp (e2tok) ;
   PopBool (t1, exit1) ;
   BackPatch (t1, NextQuad) ;
   PushFor (Merge (PopFor (), exit1)) ;       (* Merge exit1.  *)

   BackPatch(ForLoop, NextQuad) ; (* Fixes the start of the for loop.  *)
   ForLoop := NextQuad ;

   (* And set up the stack.  *)

   PushTFtok (IdSym, GetSym (IdSym), idtok) ;
   PushTFtok (BySym, ByType, bytok) ;
   PushTFtok (LastIterator, GetSType (LastIterator), e2tok) ;
   PushT (ForLoop) ;
   PushT (RangeId)
END BuildForToByDo ;


(*
   BuildEndFor - Builds the End part of the For statement
                 from the quad stack.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +----------------+
                 | RangeId        |
                 |----------------|
                 | ForQuad        |
                 |----------------|
                 | LastValue      |
                 |----------------|
                 | BySym | ByType |
                 |----------------|
                 | IdSym          |      Empty
                 |----------------|
*)

PROCEDURE BuildEndFor (endpostok: CARDINAL) ;
VAR
   t, f,
   tsym,
   RangeId,
   IncQuad,
   ForQuad: CARDINAL ;
   LastSym,
   ByType,
   BySym,
   bytok,
   IdSym,
   idtok  : CARDINAL ;
BEGIN
   PopT (RangeId) ;
   PopT (ForQuad) ;
   PopT (LastSym) ;
   PopTFtok (BySym, ByType, bytok) ;
   PopTtok (IdSym, idtok) ;

   (* IF IdSym=LastSym THEN exit END *)
   PushTF(IdSym, GetSType (IdSym)) ;
   PushT (EqualTok) ;
   PushTF (LastSym, GetSType (LastSym)) ;
   BuildRelOp (endpostok) ;
   PopBool (t, f) ;

   BackPatch (t, NextQuad) ;
   GenQuad (GotoOp, NulSym, NulSym, 0) ;
   PushFor (Merge (PopFor (), NextQuad-1)) ;
   BackPatch (f, NextQuad) ;
   IF GetMode (IdSym) = LeftValue
   THEN
      (* index variable is a LeftValue, therefore we must dereference it *)
      tsym := MakeTemporary (idtok, RightValue) ;
      PutVar (tsym, GetSType (IdSym)) ;
      CheckPointerThroughNil (idtok, IdSym) ;
      doIndrX (endpostok, tsym, IdSym) ;
      BuildRange (InitForLoopEndRangeCheck (tsym, BySym)) ;  (* --fixme-- pass endpostok.  *)
      IncQuad := NextQuad ;
      (* we have explicitly checked using the above and also
         this addition can legitimately overflow if a cardinal type
         is counting down.  The above test will generate a more
         precise error message, so we suppress overflow detection
         here.  *)
      GenQuadOTypetok (bytok, AddOp, tsym, tsym, BySym, FALSE, FALSE,
                       idtok, idtok, bytok) ;
      CheckPointerThroughNil (idtok, IdSym) ;
      GenQuadOtok (idtok, XIndrOp, IdSym, GetSType (IdSym),
                   tsym, FALSE,
                   idtok, idtok, idtok)
   ELSE
      BuildRange (InitForLoopEndRangeCheck (IdSym, BySym)) ;
      IncQuad := NextQuad ;
      (* we have explicitly checked using the above and also
         this addition can legitimately overflow if a cardinal type
         is counting down.  The above test will generate a more
         precise error message, so we suppress overflow detection
         here.

         This quadruple suppresses the generic binary op type
         check (performed in M2GenGCC.mod) as there
         will be a more informative/exhaustive check performed by the
         InitForLoopBeginRangeCheck setup in BuildForToByDo and
         performed by M2Range.mod.  *)
      GenQuadOTypetok (idtok, AddOp, IdSym, IdSym, BySym, FALSE, FALSE,
                       idtok, idtok, bytok)
   END ;
   GenQuadO (endpostok, GotoOp, NulSym, NulSym, ForQuad, FALSE) ;
   BackPatch (PopFor (), NextQuad) ;
   AddForInfo (ForQuad, NextQuad-1, IncQuad, IdSym, idtok) ;
   PutRangeForIncrement (RangeId, IncQuad)
END BuildEndFor ;


(*
   BuildCaseStart - starts the case statement.
                    It initializes a backpatch list on the compile
                    time stack, the list is used to contain all
                    case break points. The list is later backpatched
                    and contains all positions of the case statement
                    which jump to the end of the case statement.
                    The stack also contains room for a boolean
                    expression, this is needed to allow , operator
                    in the CaseField alternatives.

                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====

                                                           <- Ptr
                                            +------------+
                                            | 0    | 0   |
                                            |------------|
                                            | 0    | 0   |
                    +-------------+         |------------|
                    | Expr |      |         | Expr |     |
                    |-------------|         |------------|
*)

PROCEDURE BuildCaseStart ;
BEGIN
   BuildRange (InitCaseBounds (PushCase (NulSym, NulSym, OperandT (1)))) ;
   PushBool (0, 0) ;  (* BackPatch list initialized *)
   PushBool (0, 0)    (* Room for a boolean expression *)
END BuildCaseStart ;


(*
   BuildCaseStartStatementSequence - starts the statement sequence
                                     inside a case clause.
                                     BackPatches the true exit to the
                                     NextQuad.
                                     The Stack:

                                     Entry             Exit

                              Ptr ->                                  <- Ptr
                                     +-----------+     +------------+
                                     | t   | f   |     | 0   | f    |
                                     |-----------|     |------------|
*)

PROCEDURE BuildCaseStartStatementSequence ;
VAR
   t, f: CARDINAL ;
BEGIN
   PopBool (t, f) ;
   BackPatch (t, NextQuad) ;
   PushBool (0, f)
END BuildCaseStartStatementSequence ;


(*
   BuildCaseEndStatementSequence - ends the statement sequence
                                   inside a case clause.
                                   BackPatches the false exit f1 to the
                                   NextQuad.
                                   Asserts that t1 and f2 is 0
                                   Pushes t2+q and 0

                                   Quadruples:

                                   q  GotoOp  _  _  0

                                   The Stack:

                                   Entry             Exit

                            Ptr ->                                  <- Ptr
                                   +-----------+     +------------+
                                   | t1  | f1  |     | 0    | 0   |
                                   |-----------|     |------------|
                                   | t2  | f2  |     | t2+q | 0   |
                                   |-----------|     |------------|
*)

PROCEDURE BuildCaseEndStatementSequence ;
VAR
   t1, f1,
   t2, f2: CARDINAL ;
BEGIN
   GenQuad (GotoOp, NulSym, NulSym, 0) ;
   PopBool (t1, f1) ;
   PopBool (t2, f2) ;          (* t2 contains the break list for the case *)
   BackPatch (f1, NextQuad) ;  (* f1 no longer needed *)
   Assert (t1=0) ;
   Assert (f2=0) ;
   PushBool (Merge (t2, NextQuad-1), 0) ;  (* NextQuad-1 = Goto Quad *)
   PushBool (0, 0)             (* Room for boolean expression *)
END BuildCaseEndStatementSequence ;


(*
   BuildCaseRange - builds the range testing quaruples for
                    a case clause.

                    IF (e1>=ce1) AND (e1<=ce2)
                    THEN

                    ELS..

                    The Stack:

                    Entry             Exit

             Ptr ->
                    +-----------+
                    | ce2       |                   <- Ptr
                    |-----------|     +-----------+
                    | ce1       |     | t   | f   |
                    |-----------|     |-----------|
                    | t1  | f1  |     | t1  | f1  |
                    |-----------|     |-----------|
                    | t2  | f2  |     | t2  | f2  |
                    |-----------|     |-----------|
                    | e1        |     | e1        |
                    |-----------|     |-----------|
*)

PROCEDURE BuildCaseRange ;
VAR
   ce1, ce2,
   combinedtok,
   ce1tok,
   ce2tok,
   e1tok,
   e1,
   t2, f2,
   t1, f1  : CARDINAL ;
BEGIN
   PopTtok (ce2, ce2tok) ;
   PopTtok (ce1, ce1tok) ;
   combinedtok := MakeVirtualTok (ce2tok, ce2tok, ce1tok) ;
   AddRange (ce1, ce2, combinedtok) ;
   PopBool (t1, f1) ;
   PopBool (t2, f2) ;
   PopTtok (e1, e1tok) ;
   PushTtok (e1, e1tok) ;  (* leave e1 on bottom of stack when exit procedure *)
   PushBool (t2, f2) ;
   PushBool (t1, f1) ;  (* also leave t1 and f1 on the bottom of the stack *)
   PushTtok (e1, e1tok) ;
   PushT (GreaterEqualTok) ;
   PushTtok (ce1, ce1tok) ;
   BuildRelOp (combinedtok) ;
   PushT (AndTok) ;
   RecordOp ;
   PushTtok (e1, e1tok) ;
   PushT (LessEqualTok) ;
   PushTtok (ce2, ce2tok) ;
   BuildRelOp (combinedtok) ;
   BuildBinaryOp
END BuildCaseRange ;


(*
   BuildCaseEquality - builds the range testing quadruples for
                       a case clause.

                       IF e1=ce1
                       THEN

                       ELS..

                       The Stack:

                       Entry             Exit

                Ptr ->
                       +-----------+     +-----------+
                       | ce1       |     | t   | f   |
                       |-----------|     |-----------|
                       | t1  | f1  |     | t1  | f1  |
                       |-----------|     |-----------|
                       | t2  | f2  |     | t2  | f2  |
                       |-----------|     |-----------|
                       | e1        |     | e1        |
                       |-----------|     |-----------|
*)

PROCEDURE BuildCaseEquality ;
VAR
   ce1tok,
   e1tok,
   ce1, e1,
   t2, f2,
   t1, f1 : CARDINAL ;
BEGIN
   PopTtok (ce1, ce1tok) ;
   AddRange (ce1, NulSym, ce1tok) ;
   PopBool (t1, f1) ;
   PopBool (t2, f2) ;
   PopTtok (e1, e1tok) ;
   PushTtok (e1, e1tok) ;   (* leave e1 on bottom of stack when exit procedure *)
   PushBool (t2, f2) ;      (* also leave t2 and f2 on the bottom of the stack *)
   PushBool (t1, f1) ;
   PushTtok (e1, e1tok) ;
   PushT (EqualTok) ;
   PushTtok (ce1, ce1tok) ;
   BuildRelOp (ce1tok)
END BuildCaseEquality ;


(*
   BuildCaseList - merges two case tests into one

                   The Stack:

                   Entry             Exit

            Ptr ->
                   +-----------+
                   | t2  | f2  |
                   |-----------|     +-------------+
                   | t1  | f1  |     | t1+t2| f1+f2|
                   |-----------|     |-------------|
*)

PROCEDURE BuildCaseList ;
VAR
   t2, f2,
   t1, f1: CARDINAL ;
BEGIN
   PopBool (t2, f2) ;
   PopBool (t1, f1) ;
   PushBool (Merge (t1, t2), Merge (f1, f2))
END BuildCaseList ;


(*
   BuildCaseOr - builds the , in the case clause.

                 The Stack:

                 Entry             Exit

          Ptr ->                                  <- Ptr
                 +-----------+     +------------+
                 | t   | f   |     | t    | 0   |
                 |-----------|     |------------|
*)

PROCEDURE BuildCaseOr ;
VAR
   t, f: CARDINAL ;
BEGIN
   PopBool (t, f) ;
   BackPatch (f, NextQuad) ;
   PushBool (t, 0)
END BuildCaseOr ;


(*
   BuildCaseElse - builds the else of case clause.

                  The Stack:

                  Entry             Exit

           Ptr ->                                  <- Ptr
                  +-----------+     +------------+
                  | t   | f   |     | t    | 0   |
                  |-----------|     |------------|
*)

PROCEDURE BuildCaseElse ;
VAR
   t, f: CARDINAL ;
BEGIN
   PopBool (t, f) ;
   BackPatch (f, NextQuad) ;
   PushBool (t, 0)
END BuildCaseElse ;


(*
   BuildCaseEnd - builds the end of case clause.

                  The Stack:

                  Entry             Exit

           Ptr ->
                  +-----------+
                  | t1  | f1  |
                  |-----------|
                  | t2  | f2  |
                  |-----------|
                  | e1        |
                  |-----------|     Empty
*)

PROCEDURE BuildCaseEnd ;
VAR
   e1,
   t, f: CARDINAL ;
BEGIN
   PopBool (t, f) ;
   BackPatch (f, NextQuad) ;
   BackPatch (t, NextQuad) ;
   PopBool (t, f) ;
   BackPatch (f, NextQuad) ;
   BackPatch (t, NextQuad) ;
   PopT (e1) ;
   PopCase
END BuildCaseEnd ;


(*
   BuildCaseCheck - builds the case checking code to ensure that
                    the program does not need an else clause at runtime.
                    The stack is unaltered.
*)

PROCEDURE BuildCaseCheck ;
BEGIN
   BuildError (InitNoElseRangeCheck ())
END BuildCaseCheck ;


(*
   BuildNulParam - Builds a nul parameter on the stack.
                   The Stack:

                   Entry             Exit

                                                    <- Ptr
                   Empty             +------------+
                                     | 0          |
                                     |------------|
*)

PROCEDURE BuildNulParam ;
BEGIN
   PushT (0)
END BuildNulParam ;


(*
   BuildSizeCheckStart - switches off all quadruple generation if the function SIZE or HIGH
                         is being "called".  This should be done as SIZE only requires the
                         actual type of the expression, not its value.  Consider the problem of
                         SIZE(UninitializedPointer^) which is quite legal and it must
                         also be safe!
                         ISO Modula-2 also allows HIGH(a[0]) for a two dimensional array
                         and there is no need to compute a[0], we just need to follow the
                         type and count dimensions.  However if SIZE(a) or HIGH(a) occurs
                         and, a, is an unbounded array then we turn on quadruple generation.

                         The Stack is expected to contain:


                         Entry                       Exit
                         =====                       ====

                 Ptr ->                                                       <- Ptr
                        +----------------------+     +----------------------+
                        | ProcSym | Type | tok |     | ProcSym | Type | tok |
                        |----------------------|     |----------------------|
*)

PROCEDURE BuildSizeCheckStart ;
VAR
   ProcSym, Type, tok: CARDINAL ;
BEGIN
   PopTFtok (ProcSym, Type, tok) ;
   IF (ProcSym=Size) OR (ProcSym=TSize) OR (ProcSym=TBitSize)
   THEN
      QuadrupleGeneration := FALSE ;
      BuildingSize := TRUE
   ELSIF ProcSym=High
   THEN
      QuadrupleGeneration := FALSE ;
      BuildingHigh := TRUE
   END ;
   PushTFtok (ProcSym, Type, tok)
END BuildSizeCheckStart ;


(*
   BuildSizeCheckEnd - checks to see whether the function "called" was in fact SIZE.
                       If so then we restore quadruple generation.
*)

PROCEDURE BuildSizeCheckEnd (ProcSym: CARDINAL) ;
BEGIN
   IF (ProcSym=Size) OR (ProcSym=TSize) OR (ProcSym=TBitSize)
   THEN
      QuadrupleGeneration := TRUE ;
      BuildingSize := FALSE
   ELSIF ProcSym=High
   THEN
      QuadrupleGeneration := TRUE ;
      BuildingHigh := FALSE
   END ;
END BuildSizeCheckEnd ;


(*
   BuildProcedureCall - builds a procedure call.
                        Although this procedure does not directly
                        destroy the procedure parameters, it calls
                        routine which will manipulate the stack and
                        so the entry and exit states of the stack are shown.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildProcedureCall (tokno: CARDINAL) ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT (NoOfParam+1) ;
   PushT (NoOfParam) ;  (* Compile time stack restored to entry state *)
   IF IsPseudoBaseProcedure (ProcSym) OR IsPseudoSystemProcedure (ProcSym)
   THEN
      DisplayStack ;
      ManipulatePseudoCallParameters ;
      DisplayStack ;
      BuildPseudoProcedureCall (tokno) ;
      DisplayStack
   ELSIF IsUnknown (ProcSym)
   THEN
      MetaError1 ('{%1Ua} is not recognised as a procedure, check declaration or import', ProcSym) ;
      PopN (NoOfParam + 2)
   ELSE
      DisplayStack ;
      BuildRealProcedureCall (tokno) ;
      DisplayStack ;
   END
END BuildProcedureCall ;


(*
   BuildRealProcedureCall - builds a real procedure call.
                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |
                            |----------------|
                            | ProcSym | Type |         Empty
                            |----------------|
*)

PROCEDURE BuildRealProcedureCall (tokno: CARDINAL) ;
VAR
   NoOfParam: CARDINAL ;
   ProcSym  : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   PushT (NoOfParam) ;
   ProcSym := OperandT (NoOfParam+2) ;
   ProcSym := SkipConst (ProcSym) ;
   (* tokno := OperandTtok (NoOfParam+2) ; *)   (* --checkme-- *)
   IF IsVar (ProcSym)
   THEN
      (* Procedure Variable ? *)
      ProcSym := SkipType (OperandF (NoOfParam+2))
   END ;
   IF IsDefImp (GetScope (ProcSym)) AND IsDefinitionForC (GetScope (ProcSym))
   THEN
      BuildRealFuncProcCall (tokno, FALSE, TRUE, FALSE)
   ELSE
      BuildRealFuncProcCall (tokno, FALSE, FALSE, FALSE)
   END
END BuildRealProcedureCall ;


(*
   BuildRealFuncProcCall - builds a real procedure or function call.
                           The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |
                            |----------------|
                            | ProcSym | Type |         Empty
                            |----------------|
*)

PROCEDURE BuildRealFuncProcCall (tokno: CARDINAL; IsFunc, IsForC, ConstExpr: BOOLEAN) ;
VAR
   AllocateProc,
   DeallocateProc,
   ForcedFunc,
   ParamConstant : BOOLEAN ;
   trash,
   resulttok,
   paramtok,
   proctok,
   NoOfParameters,
   i, pi,
   ParamType,
   Param1,     (* Used to remember first param for allocate/deallocate.  *)
   ReturnVar,
   ProcSym,
   Proc          : CARDINAL ;
BEGIN
   Param1 := NulSym ;
   ParamType := NulSym ;
   CheckProcedureParameters (IsForC) ;
   PopT (NoOfParameters) ;
   PushT (NoOfParameters) ;  (* Restore stack to original state.  *)
   ProcSym := OperandT (NoOfParameters+2) ;
   proctok := tokno ;  (* OperandTtok (NoOfParameters+2) ;  *)
   IF proctok = UnknownTokenNo
   THEN
      proctok := GetTokenNo ()
   END ;
   paramtok := proctok ;
   ProcSym := SkipConst (ProcSym) ;
   ForcedFunc := FALSE ;
   AllocateProc := FALSE ;
   DeallocateProc := FALSE ;
   IF IsVar (ProcSym)
   THEN
      (* Procedure Variable ? *)
      Proc := SkipType (OperandF (NoOfParameters+2)) ;
      ParamConstant := FALSE
   ELSE
      Proc := ProcSym ;
      ParamConstant := TRUE ;
      AllocateProc := GetSymName (Proc) = MakeKey('ALLOCATE') ;
      DeallocateProc := GetSymName (Proc) = MakeKey('DEALLOCATE')
   END ;
   IF IsFunc
   THEN
      IF GetSType (Proc) = NulSym
      THEN
         MetaErrors1 ('procedure {%1a} cannot be used as a function',
                      'procedure {%1Da} does not have a return type',
                      Proc)
      END
   ELSE
      (* is being called as a procedure *)
      IF GetSType (Proc) # NulSym
      THEN
         (* however it was declared as a procedure function *)
         IF NOT IsReturnOptional (Proc)
         THEN
            MetaErrors1 ('function {%1a} is being called but its return value is ignored',
                         'function {%1Da} return a type {%1ta:of {%1ta}}',
                         Proc)
         END ;
         IsFunc := TRUE ;
         ForcedFunc := TRUE
      END
   END ;
   IF AllocateProc OR DeallocateProc
   THEN
      Param1 := OperandT (NoOfParameters+1)   (* Remember this before manipulating.  *)
   END ;
   ManipulateParameters (IsForC) ;
   CheckParameterOrdinals ;
   PopT(NoOfParameters) ;
   IF IsFunc
   THEN
      GenQuad (ParamOp, 0, Proc, ProcSym)  (* Space for return value *)
   END ;
   IF (NoOfParameters+1=NoOfParam(Proc)) AND UsesOptArg(Proc)
   THEN
      GenQuad (OptParamOp, NoOfParam(Proc), Proc, Proc)
   END ;
   i := NoOfParameters ;
   pi := 1 ;     (* stack index referencing stacked parameter, i *)
   WHILE i>0 DO
      paramtok := OperandTtok (pi) ;
      IF (AllocateProc OR DeallocateProc) AND (i = 1) AND (Param1 # NulSym)
      THEN
         ParamType := GetItemPointedTo (Param1) ;
         IF ParamType = NulSym
         THEN
            GenQuadO (paramtok, ParamOp, i, Proc, OperandT (pi), TRUE)
         ELSE
            IF AllocateProc
            THEN
               trash := MakeTemporary (paramtok, RightValue) ;
               PutVar (trash, ParamType) ;
               PutVarHeap (trash, TRUE)
            ELSE
               Assert (DeallocateProc) ;
               trash := Nil
            END ;
            GenQuadOTrash (paramtok, ParamOp, i, Proc, OperandT (pi), TRUE, trash)
         END
      ELSE
         GenQuadO (paramtok, ParamOp, i, Proc, OperandT (pi), TRUE)
      END ;
      IF NOT IsConst (OperandT (pi))
      THEN
         ParamConstant := FALSE
      END ;
      DEC (i) ;
      INC (pi)
   END ;
   GenQuadO (proctok, CallOp, NulSym, NulSym, ProcSym, TRUE) ;
   PopN (NoOfParameters+1) ;  (* Destroy arguments and procedure call *)
   IF IsFunc
   THEN
      (* ReturnVar has the type of the procedure.  *)
      resulttok := MakeVirtualTok (proctok, proctok, paramtok) ;
      IF ConstExpr AND (NOT IsProcedureBuiltinAvailable (Proc))
      THEN
         MetaError1('{%1d} {%1ad} cannot be used in a constant expression', Proc) ;
         ParamConstant := FALSE
      END ;
      ReturnVar := MakeTemporary (resulttok, AreConstant (ParamConstant AND ConstExpr)) ;
      PutVar (ReturnVar, GetSType (Proc)) ;
      GenQuadO (resulttok, FunctValueOp, ReturnVar, NulSym, Proc, TRUE) ;
      IF NOT ForcedFunc
      THEN
         PushTFtok (ReturnVar, GetSType (Proc), resulttok)
      END
   END
END BuildRealFuncProcCall ;


(*
   CheckProcedureParameters - Checks the parameters which are being passed to
                              procedure ProcSym.

                              The Stack:


                              Entry                      Exit

                       Ptr ->                                               <- Ptr
                              +----------------+         +----------------+
                              | NoOfParam      |         | NoOfParam      |
                              |----------------|         |----------------|
                              | Param 1        |         | Param 1        |
                              |----------------|         |----------------|
                              | Param 2        |         | Param 2        |
                              |----------------|         |----------------|
                              .                .         .                .
                              .                .         .                .
                              .                .         .                .
                              |----------------|         |----------------|
                              | Param #        |         | Param #        |
                              |----------------|         |----------------|
                              | ProcSym | Type |         | ProcSym | Type |
                              |----------------|         |----------------|

*)

PROCEDURE CheckProcedureParameters (IsForC: BOOLEAN) ;
VAR
   proctok,
   paramtok    : CARDINAL ;
   n1, n2      : Name ;
   Dim,
   Actual,
   FormalI,
   ParamTotal,
   pi,
   Proc,
   ProcSym,
   i           : CARDINAL ;
   s           : String ;
BEGIN
   PopT(ParamTotal) ;
   PushT(ParamTotal) ;  (* Restore stack to origional state *)
   ProcSym := OperandT(ParamTotal+1+1) ;
   proctok := OperandTtok(ParamTotal+1+1) ;
   IF IsVar(ProcSym) AND IsProcType(GetDType(ProcSym))
   THEN
      (* Procedure Variable ? *)
      Proc := SkipType(OperandF(ParamTotal+1+1))
   ELSE
      Proc := SkipConst(ProcSym)
   END ;
   IF NOT (IsProcedure(Proc) OR IsProcType(Proc))
   THEN
      IF IsUnknown(Proc)
      THEN
         MetaError1('{%1Ua} is not recognised as a procedure, check declaration or import', Proc)
      ELSE
         MetaErrors1('{%1a} is not recognised as a procedure, check declaration or import',
                     '{%1Ua} is not recognised as a procedure, check declaration or import',
                     Proc)
      END
   END ;
   IF CompilerDebugging
   THEN
      n1 := GetSymName(Proc) ;
      printf1('  %a ( ', n1)
   END ;
   IF DebugTokPos
   THEN
      s := InitString ('procedure') ;
      WarnStringAt (s, proctok)
   END ;

   i := 1 ;
   pi := ParamTotal+1 ;   (* stack index referencing stacked parameter, i *)
   WHILE i<=ParamTotal DO
      IF i<=NoOfParam(Proc)
      THEN
         FormalI := GetParam(Proc, i) ;
         IF CompilerDebugging
         THEN
            n1 := GetSymName(FormalI) ;
            n2 := GetSymName(GetSType(FormalI)) ;
            printf2('%a: %a', n1, n2)
         END ;
         Actual := OperandT(pi) ;
         Dim := OperandD(pi) ;
         paramtok := OperandTtok(pi) ;
         IF DebugTokPos
         THEN
            s := InitString ('actual') ;
            WarnStringAt (s, paramtok)
         END ;

         BuildRange (InitTypesParameterCheck (paramtok, Proc, i, FormalI, Actual)) ;
         IF IsConst(Actual)
         THEN
            IF IsVarParam(Proc, i)
            THEN
               FailParameter (paramtok,
                              'trying to pass a constant to a VAR parameter',
                              Actual, FormalI, Proc, i)
            ELSIF IsConstString (Actual)
            THEN
               IF (NOT IsConstStringKnown (Actual))
               THEN
                  (* We dont check this yet, it is checked in M2GenGCC.mod:CodeParam
                     after the string has been created.  *)
               ELSIF IsArray(GetDType(FormalI)) AND (GetSType(GetDType(FormalI))=Char)
               THEN
                  (* Allow string literals to be passed to ARRAY [0..n] OF CHAR.  *)
               ELSIF (GetStringLength(paramtok, Actual) = 1)   (* If = 1 then it maybe treated as a char.  *)
               THEN
                  CheckParameter (paramtok, Actual, Dim, FormalI, Proc, i, NIL)
               ELSIF NOT IsUnboundedParam(Proc, i)
               THEN
                  IF IsForC AND (GetSType(FormalI)=Address)
                  THEN
                     FailParameter (paramtok,
                                    'a string constant can either be passed to an ADDRESS parameter or an ARRAY OF CHAR',
                                    Actual, FormalI, Proc, i)
                  ELSE
                     FailParameter (paramtok,
                                    'cannot pass a string constant to a non unbounded array parameter',
                                    Actual, FormalI, Proc, i)
                  END
               END
            END
         ELSE
            CheckParameter (paramtok, Actual, Dim, FormalI, Proc, i, NIL)
         END
      ELSE
         IF IsForC AND UsesVarArgs(Proc)
         THEN
            (* these are varargs, therefore we don't check them *)
            i := ParamTotal
         ELSE
            MetaErrorT2 (proctok, 'too many parameters, {%2n} passed to {%1a} ', Proc, i)
         END
      END ;
      INC(i) ;
      DEC(pi) ;
      IF CompilerDebugging
      THEN
         IF i<=ParamTotal
         THEN
            printf0 ('; ')
         ELSE
            printf0 (' ) ; \n')
         END
      END
   END
END CheckProcedureParameters ;


(*
   CheckProcTypeAndProcedure - checks the ProcType with the call.
*)

PROCEDURE CheckProcTypeAndProcedure (tokno: CARDINAL; ProcType: CARDINAL; call: CARDINAL) ;
VAR
   n1, n2          : Name ;
   i, n, t         : CARDINAL ;
   CheckedProcedure: CARDINAL ;
   e               : Error ;
BEGIN
   n := NoOfParam(ProcType) ;
   IF IsVar(call) OR IsTemporary(call) OR IsParameter(call)
   THEN
      CheckedProcedure := GetDType(call)
   ELSE
      CheckedProcedure := call
   END ;
   IF n#NoOfParam(CheckedProcedure)
   THEN
      e := NewError(GetDeclaredMod(ProcType)) ;
      n1 := GetSymName(call) ;
      n2 := GetSymName(ProcType) ;
      ErrorFormat2(e, 'procedure (%a) is a parameter being passed as variable (%a) but they are declared with different number of parameters',
                   n1, n2) ;
      e := ChainError(GetDeclaredMod(call), e) ;
      t := NoOfParam(CheckedProcedure) ;
      IF n<2
      THEN
         ErrorFormat3(e, 'procedure (%a) is being called incorrectly with (%d) parameter, declared with (%d)',
                      n1, n, t)
      ELSE
         ErrorFormat3(e, 'procedure (%a) is being called incorrectly with (%d) parameters, declared with (%d)',
                      n1, n, t)
      END
   ELSE
      i := 1 ;
      WHILE i<=n DO
         IF IsVarParam (ProcType, i) # IsVarParam (CheckedProcedure, i)
         THEN
            MetaError3 ('parameter {%3n} in {%1dD} causes a mismatch it was declared as a {%2d}', ProcType, GetNth (ProcType, i), i) ;
            MetaError3 ('parameter {%3n} in {%1dD} causes a mismatch it was declared as a {%2d}', call, GetNth (call, i), i)
         END ;
         BuildRange (InitTypesParameterCheck (tokno, CheckedProcedure, i,
                                              GetParam (CheckedProcedure, i),
                                              GetParam (ProcType, i))) ;
         (* CheckParameter(tokpos, GetParam(CheckedProcedure, i), 0, GetParam(ProcType, i), call, i, TypeList) ; *)
         INC(i)
      END
   END
END CheckProcTypeAndProcedure ;


(*
   IsReallyPointer - returns TRUE is sym is a pointer, address or a type declared
                     as a pointer or address.
*)

PROCEDURE IsReallyPointer (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsVar(Sym)
   THEN
      Sym := GetSType(Sym)
   END ;
   Sym := SkipType(Sym) ;
   RETURN( IsPointer(Sym) OR (Sym=Address) )
END IsReallyPointer ;


(*
   LegalUnboundedParam - returns TRUE if the parameter, Actual, can legitimately be
                         passed to ProcSym, i, the, Formal, parameter.
*)

PROCEDURE LegalUnboundedParam (tokpos: CARDINAL; ProcSym, i, ActualType, Actual, Dimension, Formal: CARDINAL) : BOOLEAN ;
VAR
   FormalType: CARDINAL ;
   n, m      : CARDINAL ;
BEGIN
   ActualType := SkipType(ActualType) ;
   FormalType := GetDType(Formal) ;
   FormalType := GetSType(FormalType) ;   (* type of the unbounded ARRAY *)
   IF IsArray(ActualType)
   THEN
      m := GetDimension(Formal) ;
      n := 0 ;
      WHILE IsArray(ActualType) DO
         INC(n) ;
         ActualType := GetDType(ActualType) ;
         IF (m=n) AND (ActualType=FormalType)
         THEN
            RETURN( TRUE )
         END
      END ;
      IF n=m
      THEN
         (* now we fall though and test ActualType against FormalType *)
      ELSE
         IF IsGenericSystemType(FormalType)
         THEN
            RETURN( TRUE )
         ELSE
            FailParameter(tokpos,
                          'attempting to pass an array with the incorrect number dimenisons to an unbounded formal parameter of different dimensions',
                          Actual, Formal, ProcSym, i) ;
            RETURN( FALSE )
         END
      END
   ELSIF IsUnbounded(ActualType)
   THEN
      IF (Dimension=0) AND (GetDimension(Formal)=GetDimension(Actual))
      THEN
         (* now we fall though and test ActualType against FormalType *)
         ActualType := GetSType(ActualType)
      ELSE
         IF IsGenericSystemType(FormalType)
         THEN
            RETURN( TRUE )
         ELSE
            IF GetDimension(Actual)-Dimension = GetDimension(Formal)
            THEN
               ActualType := GetSType(ActualType)
            ELSE
               FailParameter(tokpos,
                             'attempting to pass an unbounded array with the incorrect number dimenisons to an unbounded formal parameter of different dimensions',
                             Actual, Formal, ProcSym, i) ;
               RETURN( FALSE )
            END
         END
      END
   END ;
   IF IsGenericSystemType (FormalType) OR
      IsGenericSystemType (ActualType) OR
      IsAssignmentCompatible (FormalType, ActualType)
   THEN
      (* we think it is legal, but we ask post pass 3 to check as
         not all types are known at this point *)
      RETURN( TRUE )
   ELSE
      FailParameter(tokpos,
                    'identifier with an incompatible type is being passed to this procedure',
                    Actual, Formal, ProcSym, i) ;
      RETURN( FALSE )
   END
END LegalUnboundedParam ;


(*
   CheckParameter - checks that types ActualType and FormalType are compatible for parameter
                    passing. ProcSym is the procedure and i is the parameter number.

                    We obey the following rules:

                    (1)  we allow WORD, BYTE, LOC to be compitable with any like sized
                         type.
                    (2)  we allow ADDRESS to be compatible with any pointer type.
                    (3)  we relax INTEGER and CARDINAL checking for Temporary variables.

                    Note that type sizes are checked during the code generation pass.
*)

PROCEDURE CheckParameter (tokpos: CARDINAL;
                          Actual, Dimension, Formal, ProcSym: CARDINAL;
                          i: CARDINAL; TypeList: List) ;
VAR
   NewList            : BOOLEAN ;
   ActualType, FormalType: CARDINAL ;
BEGIN
   IF IsConstString(Actual) AND (NOT IsConstStringKnown (Actual))
   THEN
      (* Cannot check if the string content is not yet known.  *)
      RETURN
   END ;
   FormalType := GetDType(Formal) ;
   IF IsConstString(Actual) AND (GetStringLength(tokpos, Actual) = 1)   (* if = 1 then it maybe treated as a char *)
   THEN
      ActualType := Char
   ELSIF Actual=Boolean
   THEN
      ActualType := Actual
   ELSE
      ActualType := GetDType(Actual)
   END ;
   IF TypeList=NIL
   THEN
      NewList := TRUE ;
      InitList(TypeList)
   ELSE
      NewList := FALSE
   END ;
   IF IsItemInList(TypeList, ActualType)
   THEN
      (* no need to check *)
      RETURN
   END ;
   IncludeItemIntoList(TypeList, ActualType) ;
   IF IsProcType(FormalType)
   THEN
      IF (NOT IsProcedure(Actual)) AND ((ActualType=NulSym) OR (NOT IsProcType(SkipType(ActualType))))
      THEN
         FailParameter(tokpos,
                       'expecting a procedure or procedure variable as a parameter',
                       Actual, Formal, ProcSym, i) ;
         RETURN
      END ;
      IF IsProcedure(Actual) AND IsProcedureNested(Actual)
      THEN
         MetaError2 ('cannot pass a nested procedure {%1Ea} seen in the {%2N} parameter as the outer scope will be unknown at runtime', Actual, i)
      END ;
      (* we can check the return type of both proc types *)
      IF (ActualType#NulSym) AND IsProcType(ActualType)
      THEN
         IF ((GetSType(ActualType)#NulSym) AND (GetSType(FormalType)=NulSym))
         THEN
            FailParameter(tokpos,
                          'the item being passed is a function whereas the formal procedure parameter is a procedure',
                          Actual, Formal, ProcSym, i) ;
            RETURN
         ELSIF ((GetSType(ActualType)=NulSym) AND (GetSType(FormalType)#NulSym))
         THEN
            FailParameter(tokpos,
                          'the item being passed is a procedure whereas the formal procedure parameter is a function',
                          Actual, Formal, ProcSym, i) ;
            RETURN
         ELSIF AssignmentRequiresWarning(GetSType(ActualType), GetSType(FormalType))
         THEN
            WarnParameter(tokpos,
                          'the return result of the procedure variable parameter may not be compatible on other targets with the return result of the item being passed',
                          Actual, Formal, ProcSym, i) ;
            RETURN
         ELSIF IsGenericSystemType (GetSType(FormalType)) OR
               IsGenericSystemType (GetSType(ActualType)) OR
               IsAssignmentCompatible(GetSType(ActualType), GetSType(FormalType))
         THEN
            (* pass *)
         ELSE
            FailParameter(tokpos,
                          'the return result of the procedure variable parameter is not compatible with the return result of the item being passed',
                          Actual, Formal, ProcSym, i) ;
            RETURN
         END
      END ;
      (* now to check each parameter of the proc type *)
      CheckProcTypeAndProcedure (tokpos, FormalType, Actual)
   ELSIF (ActualType#FormalType) AND (ActualType#NulSym)
   THEN
      IF IsUnknown(FormalType)
      THEN
         FailParameter(tokpos,
                       'procedure parameter type is undeclared',
                       Actual, Formal, ProcSym, i) ;
         RETURN
      END ;
      IF IsUnbounded(ActualType) AND (NOT IsUnboundedParam(ProcSym, i))
      THEN
         FailParameter(tokpos,
                       'attempting to pass an unbounded array to a NON unbounded parameter',
                       Actual, Formal, ProcSym, i) ;
         RETURN
      ELSIF IsUnboundedParam(ProcSym, i)
      THEN
         IF NOT LegalUnboundedParam(tokpos, ProcSym, i, ActualType, Actual, Dimension, Formal)
         THEN
            RETURN
         END
      ELSIF ActualType#FormalType
      THEN
         IF AssignmentRequiresWarning(FormalType, ActualType)
         THEN
            WarnParameter (tokpos,
                           'identifier being passed to this procedure may contain a possibly incompatible type when compiling for a different target',
                           Actual, Formal, ProcSym, i)
         ELSIF IsGenericSystemType (FormalType) OR
               IsGenericSystemType (ActualType) OR
               IsAssignmentCompatible (ActualType, FormalType)
         THEN
            (* so far we know it is legal, but not all types have been resolved
               and so this is checked later on in another pass.  *)
         ELSE
            FailParameter (tokpos,
                           'identifier with an incompatible type is being passed to this procedure',
                           Actual, Formal, ProcSym, i)
         END
      END
   END ;
   IF NewList
   THEN
      KillList(TypeList)
   END
END CheckParameter ;


(*
   DescribeType - returns a String describing a symbol, Sym, name and its type.
*)

PROCEDURE DescribeType (Sym: CARDINAL) : String ;
VAR
   s, s1, s2: String ;
   Low, High,
   Subrange,
   Subscript,
   Type     : CARDINAL ;
BEGIN
   s := NIL ;
   IF IsConstString(Sym)
   THEN
      (* If = 1 then it maybe treated as a char.  *)
      IF IsConstStringKnown (Sym) AND (GetStringLength (GetDeclaredMod (Sym), Sym) = 1)
      THEN
         s := InitString('(constant string) or {%kCHAR}')
      ELSE
         s := InitString('(constant string)')
      END
   ELSIF IsConst(Sym)
   THEN
      s := InitString('(constant)')
   ELSIF IsUnknown(Sym)
   THEN
      s := InitString('(unknown)')
   ELSE
      Type := GetSType(Sym) ;
      IF Type=NulSym
      THEN
         s := InitString('(unknown)')
      ELSIF IsUnbounded(Type)
      THEN
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetSType(Type))))) ;
         s := Sprintf1(Mark(InitString('{%%kARRAY} {%%kOF} %s')), s1)
      ELSIF IsArray(Type)
      THEN
         s := InitString('{%kARRAY} [') ;
         Subscript := GetArraySubscript(Type) ;
         IF Subscript#NulSym
         THEN
            Assert(IsSubscript(Subscript)) ;
            Subrange := GetSType(Subscript) ;
            IF NOT IsSubrange(Subrange)
            THEN
               MetaError3 ('error in definition of array {%1Ead} in the {%2N} subscript which has no subrange, instead type given is {%3a}',
                            Sym, Subscript, Subrange)
            END ;
            Assert(IsSubrange(Subrange)) ;
            GetSubrange(Subrange, High, Low) ;
            s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Low)))) ;
            s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(High)))) ;
            s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s..%s')),
                                         s1, s2)))
         END ;
         s1 := Mark(DescribeType(Type)) ;
         s := ConCat(ConCat(s, Mark(InitString('] OF '))), s1)
      ELSE
         IF IsUnknown(Type)
         THEN
            s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Type)))) ;
            s := Sprintf1(Mark(InitString('%s (currently unknown, check declaration or import)')),
                          s1)
         ELSE
            s := InitStringCharStar(KeyToCharStar(GetSymName(Type)))
         END
      END
   END ;
   RETURN( s )
END DescribeType ;


(*
   FailParameter - generates an error message indicating that a parameter
                   declaration has failed.

                   The parameters are:

                   CurrentState  - string describing the current failing state.
                   Given         - the token that the source code provided.
                   Expecting     - token or identifier that was expected.
                   ParameterNo   - parameter number that has failed.
                   ProcedureSym  - procedure symbol where parameter has failed.

                   If any parameter is Nul then it is ignored.
*)

PROCEDURE FailParameter (tokpos       : CARDINAL;
                         CurrentState : ARRAY OF CHAR;
                         Given        : CARDINAL;
                         Expecting    : CARDINAL;
                         ProcedureSym : CARDINAL;
                         ParameterNo  : CARDINAL) ;
VAR
   First,
   ExpectType: CARDINAL ;
   s, s1, s2 : String ;
BEGIN
   MetaErrorT2 (tokpos,
                'parameter mismatch between the {%2N} parameter of procedure {%1Ead}',
                ProcedureSym, ParameterNo) ;
   s := InitString ('{%kPROCEDURE} {%1Eau} (') ;
   IF NoOfParam(ProcedureSym)>=ParameterNo
   THEN
      IF ParameterNo>1
      THEN
         s := ConCat(s, Mark(InitString('.., ')))
      END ;
      IF IsVarParam(ProcedureSym, ParameterNo)
      THEN
         s := ConCat(s, Mark(InitString('{%kVAR} ')))
      END ;

      First := GetDeclaredMod(GetNthParam(ProcedureSym, ParameterNo)) ;
      ExpectType := GetSType(Expecting) ;
      IF IsUnboundedParam(ProcedureSym, ParameterNo)
      THEN
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetSType(ExpectType))))) ;
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: {%%kARRAY} {%%kOF} %s')),
                                      s1, s2)))
      ELSE
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ExpectType)))) ;
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: %s')), s1, s2)))
      END ;
      IF ParameterNo<NoOfParam(ProcedureSym)
      THEN
         s := ConCat(s, Mark(InitString('; ... ')))
      END
   ELSE
      First := GetDeclaredMod(ProcedureSym) ;
      IF NoOfParam(ProcedureSym)>0
      THEN
         s := ConCat(s, Mark(InitString('..')))
      END
   END ;
   s := ConCat (s, Mark (InitString ('){%1Tau:% : {%1Tau}} ;'))) ;
   MetaErrorStringT1 (First, Dup (s), ProcedureSym) ;
   MetaErrorStringT1 (tokpos, s, ProcedureSym) ;
   IF GetLType (Given) = NulSym
   THEN
      MetaError1 ('item being passed is {%1EDda} {%1Dad}', Given)
   ELSE
      MetaError1 ('item being passed is {%1EDda} {%1Dad} of type {%1Dts}',
                  Given)
   END
END FailParameter ;


(*
   WarnParameter - generates a warning message indicating that a parameter
                   use might cause problems on another target.

                   The parameters are:

                   CurrentState  - string describing the current failing state.
                   Given         - the token that the source code provided.
                   Expecting     - token or identifier that was expected.
                   ParameterNo   - parameter number that has failed.
                   ProcedureSym  - procedure symbol where parameter has failed.

                   If any parameter is Nul then it is ignored.
*)

PROCEDURE WarnParameter (tokpos       : CARDINAL;
                         CurrentState : ARRAY OF CHAR;
                         Given        : CARDINAL;
                         Expecting    : CARDINAL;
                         ProcedureSym : CARDINAL;
                         ParameterNo  : CARDINAL) ;
VAR
   First,
   ExpectType,
   ReturnType: CARDINAL ;
   s, s1, s2 : String ;
BEGIN
   s := InitString('{%W}') ;
   IF CompilingImplementationModule()
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString('warning issued while compiling the implementation module\n'))))
   ELSIF CompilingProgramModule()
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString('warning issued while compiling the program module\n'))))
   END ;
   s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ProcedureSym)))) ;
   s := ConCat(s, Mark(Sprintf2(Mark(InitString('problem in parameter %d, PROCEDURE %s (')),
                                ParameterNo,
                                s1))) ;
   IF NoOfParam(ProcedureSym)>=ParameterNo
   THEN
      IF ParameterNo>1
      THEN
         s := ConCat(s, Mark(InitString('.., ')))
      END ;
      IF IsVarParam(ProcedureSym, ParameterNo)
      THEN
         s := ConCat(s, Mark(InitString('{%kVAR} ')))
      END ;

      First := GetDeclaredMod(GetNthParam(ProcedureSym, ParameterNo)) ;
      ExpectType := GetSType(Expecting) ;
      IF IsUnboundedParam(ProcedureSym, ParameterNo)
      THEN
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(GetSType(ExpectType))))) ;
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: {%%kARRAY} {%%kOF} %s')),
                                      s1, s2)))
      ELSE
         s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Expecting)))) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ExpectType)))) ;
         s := ConCat(s, Mark(Sprintf2(Mark(InitString('%s: %s')), s1, s2)))
      END ;
      IF ParameterNo<NoOfParam(ProcedureSym)
      THEN
         s := ConCat(s, Mark(InitString('; ... ')))
      END
   ELSE
      First := GetDeclaredMod(ProcedureSym) ;
      IF NoOfParam(ProcedureSym)>0
      THEN
         s := ConCat(s, Mark(InitString('..')))
      END
   END ;
   ReturnType := GetSType(ProcedureSym) ;
   IF ReturnType=NulSym
   THEN
      s := ConCat(s, Sprintf0(Mark(InitString(') ;\n'))))
   ELSE
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(ReturnType)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString(') : %s ;\n')), s1)))
   END ;
   IF IsConstString(Given)
   THEN
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Given)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString("item being passed is '%s'")),
                                   s1)))
   ELSIF IsTemporary(Given)
   THEN
      s := ConCat(s, Mark(InitString("item being passed has type")))
   ELSE
      s1 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Given)))) ;
      s := ConCat(s, Mark(Sprintf1(Mark(InitString("item being passed is '%s'")),
                                   s1)))
   END ;
   s1 := DescribeType(Given) ;
   s2 := Mark(InitString(CurrentState)) ;
   s := ConCat(s, Mark(Sprintf2(Mark(InitString(': %s\nparameter mismatch: %s')),
                                s1, s2))) ;
   MetaErrorStringT0 (tokpos, Dup (s)) ;
   MetaErrorStringT0 (First, Dup (s))
END WarnParameter ;


(*
   ExpectVariable - checks to see whether, sym, is declared as a variable.
                    If not then it generates an error message.
*)

(*
PROCEDURE ExpectVariable (a: ARRAY OF CHAR; sym: CARDINAL) ;
VAR
   e         : Error ;
   s1, s2, s3: String ;
BEGIN
   IF NOT IsVar(sym)
   THEN
      e := NewError(GetTokenNo()) ;
      IF IsUnknown(sym)
      THEN
         s1 := ConCat (InitString (a),
                       Mark (InitString ('but was given an undeclared symbol {%E1a}'))) ;

         ErrorString(e, Sprintf2(Mark(InitString('%s but was given an undeclared symbol %s')), s1, s2))
      ELSE
         s1 := Mark(InitString(a)) ;
         s2 := Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym)))) ;
         s3 := Mark(DescribeType(sym)) ;
         ErrorString(e, Sprintf3(Mark(InitString('%s but was given %s: %s')),
                                 s1, s2, s3))
      END
   END
END ExpectVariable ;
*)


(*
   doIndrX - perform des = *exp with a conversion if necessary.
*)

PROCEDURE doIndrX (tok: CARDINAL;
                   des, exp: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   IF GetDType(des)=GetDType(exp)
   THEN
      GenQuadOtok (tok, IndrXOp, des, GetSType (des), exp, TRUE,
                   tok, tok, tok)
   ELSE
      t := MakeTemporary (tok, RightValue) ;
      PutVar (t, GetSType (exp)) ;
      GenQuadOtok (tok, IndrXOp, t, GetSType (exp), exp, TRUE,
                   tok, tok, tok) ;
      GenQuadOtok (tok, BecomesOp, des, NulSym, doVal (GetSType(des), t), TRUE,
                   tok, UnknownTokenNo, tok)
   END
END doIndrX ;


(*
   MakeRightValue - returns a temporary which will have the RightValue of symbol, Sym.
                    If Sym is a right value and has type, type, then no quadruples are
                    generated and Sym is returned. Otherwise a new temporary is created
                    and an IndrX quadruple is generated.
*)

PROCEDURE MakeRightValue (tok: CARDINAL;
                          Sym: CARDINAL; type: CARDINAL) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF GetMode (Sym) = RightValue
   THEN
      IF GetSType(Sym) = type
      THEN
         RETURN Sym    (* already a RightValue with desired type *)
      ELSE
         (*
            type change or mode change, type changes are a pain, but I've
            left them here as it is perhaps easier to remove them later.
         *)
         t := MakeTemporary (tok, RightValue) ;
         PutVar (t, type) ;
         GenQuadOtok (tok, BecomesOp, t, NulSym, doVal (type, Sym), TRUE,
                      tok, tok, tok) ;
         RETURN t
      END
   ELSE
      t := MakeTemporary (tok, RightValue) ;
      PutVar (t, type) ;
      CheckPointerThroughNil (tok, Sym) ;
      doIndrX (tok, t, Sym) ;
      RETURN t
   END
END MakeRightValue ;


(*
   MakeLeftValue - returns a temporary coresponding to the LeftValue of
                   symbol, Sym. No quadruple is generated if Sym is already
                   a LeftValue and has the same type.
*)

PROCEDURE MakeLeftValue (tok: CARDINAL;
                         Sym: CARDINAL; with: ModeOfAddr; type: CARDINAL) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF GetMode (Sym) = LeftValue
   THEN
      IF GetSType (Sym) = type
      THEN
         RETURN Sym
      ELSE
         (*
            type change or mode change, type changes are a pain, but I've
            left them here as it is perhaps easier to remove them later
         *)
         t := MakeTemporary (tok, with) ;
         PutVar (t, type) ;
         GenQuadOtok (tok, BecomesOp, t, NulSym, Sym, TRUE,
                      tok, UnknownTokenNo, tok) ;
         RETURN t
      END
   ELSE
      t := MakeTemporary (tok, with) ;
      PutVar (t, type) ;
      GenQuadOtok (tok, AddrOp, t, NulSym, Sym, TRUE,
                   tok, UnknownTokenNo, tok) ;
      RETURN t
   END
END MakeLeftValue ;


(*
   ManipulatePseudoCallParameters - manipulates the parameters to a pseudo function or
                                    procedure. It dereferences all LeftValue parameters
                                    and Boolean parameters.
                                    The Stack:


                                    Entry                      Exit

                             Ptr ->                            exactly the same
                                    +----------------+
                                    | NoOfParameters |
                                    |----------------|
                                    | Param 1        |
                                    |----------------|
                                    | Param 2        |
                                    |----------------|
                                    .                .
                                    .                .
                                    .                .
                                    |----------------|
                                    | Param #        |
                                    |----------------|
                                    | ProcSym | Type |
                                    |----------------|

*)

PROCEDURE ManipulatePseudoCallParameters ;
VAR
   NoOfParameters,
   ProcSym, Proc,
   i, pi         : CARDINAL ;
   f             : BoolFrame ;
BEGIN
   PopT(NoOfParameters) ;
   PushT(NoOfParameters) ;    (* restored to original state *)
   (* Ptr points to the ProcSym *)
   ProcSym := OperandT(NoOfParameters+1+1) ;
   IF IsVar(ProcSym)
   THEN
      InternalError ('expecting a pseudo procedure or a type')
   ELSE
      Proc := ProcSym
   END ;
   i := 1 ;
   pi := NoOfParameters+1 ;
   WHILE i<=NoOfParameters DO
      IF (GetMode(OperandT(pi))=LeftValue) AND
         (Proc#Adr) AND (Proc#Size) AND (Proc#TSize) AND (Proc#High) AND
         (* procedures which have first parameter as a VAR param *)
         (((Proc#Inc) AND (Proc#Incl) AND (Proc#Dec) AND (Proc#Excl) AND (Proc#New) AND (Proc#Dispose)) OR (i>1))
      THEN
         (* must dereference LeftValue *)
         f := PeepAddress(BoolStack, pi) ;
         f^.TrueExit := MakeRightValue (GetTokenNo(), OperandT(pi), GetSType(OperandT(pi)))
      END ;
      INC(i) ;
      DEC(pi)
   END
END ManipulatePseudoCallParameters ;


(*
   ManipulateParameters - manipulates the procedure parameters in
                          preparation for a procedure call.
                          Prepares Boolean, Unbounded and VAR parameters.
                          The Stack:


                          Entry                      Exit

                   Ptr ->                            exactly the same
                          +----------------+
                          | NoOfParameters |
                          |----------------|
                          | Param 1        |
                          |----------------|
                          | Param 2        |
                          |----------------|
                          .                .
                          .                .
                          .                .
                          |----------------|
                          | Param #        |
                          |----------------|
                          | ProcSym | Type |
                          |----------------|
*)

PROCEDURE ManipulateParameters (IsForC: BOOLEAN) ;
VAR
   tokpos,
   np           : CARDINAL ;
   s            : String ;
   ArraySym,
   UnboundedType,
   ParamType,
   NoOfParameters,
   i, pi,
   ProcSym, rw,
   Proc,
   t            : CARDINAL ;
   f            : BoolFrame ;
BEGIN
   PopT(NoOfParameters) ;
   ProcSym := OperandT(NoOfParameters+1) ;
   tokpos := OperandTtok(NoOfParameters+1) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable ? *)
      Proc := SkipType(OperandF(NoOfParameters+1))
   ELSE
      Proc := SkipConst(ProcSym)
   END ;

   IF IsForC AND UsesVarArgs(Proc)
   THEN
      IF NoOfParameters<NoOfParam(Proc)
      THEN
         s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Proc)))) ;
         np := NoOfParam(Proc) ;
         ErrorStringAt2(Sprintf3(Mark(InitString('attempting to pass (%d) parameters to procedure (%s) which was declared with varargs but contains at least (%d) parameters')),
                                 NoOfParameters, s, np),
                        tokpos, GetDeclaredMod(ProcSym))
      END
   ELSIF UsesOptArg(Proc)
   THEN
      IF NOT ((NoOfParameters=NoOfParam(Proc)) OR (NoOfParameters+1=NoOfParam(Proc)))
      THEN
         s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Proc)))) ;
         np := NoOfParam(Proc) ;
         ErrorStringAt2(Sprintf3(Mark(InitString('attempting to pass (%d) parameters to procedure (%s) which was declared with an optarg with a maximum of (%d) parameters')),
                                 NoOfParameters, s, np),
                        tokpos, GetDeclaredMod(ProcSym))
      END
   ELSIF NoOfParameters#NoOfParam(Proc)
   THEN
      s := Mark(InitStringCharStar(KeyToCharStar(GetSymName(Proc)))) ;
      np := NoOfParam(Proc) ;
      ErrorStringAt2(Sprintf3(Mark(InitString('attempting to pass (%d) parameters to procedure (%s) which was declared with (%d) parameters')),
                              NoOfParameters, s, np),
                     tokpos, GetDeclaredMod(ProcSym))
   END ;
   i := 1 ;
   pi := NoOfParameters ;
   WHILE i<=NoOfParameters DO
      f := PeepAddress(BoolStack, pi) ;
      rw := OperandMergeRW(pi) ;
      Assert(IsLegal(rw)) ;
      IF i>NoOfParam(Proc)
      THEN
         IF IsForC AND UsesVarArgs(Proc)
         THEN
            IF (GetSType(OperandT(pi))#NulSym) AND IsArray(GetDType(OperandT(pi)))
            THEN
               f^.TrueExit := MakeLeftValue(OperandTok(pi), OperandT(pi), RightValue, Address) ;
               MarkAsReadWrite(rw)
            ELSIF IsConstString (OperandT (pi))
            THEN
               f^.TrueExit := MakeLeftValue (OperandTok (pi),
                                             DeferMakeConstStringCnul (OperandTok (pi), OperandT (pi)), RightValue, Address) ;
               MarkAsReadWrite(rw)
            ELSIF (GetSType(OperandT(pi))#NulSym) AND IsUnbounded(GetSType(OperandT(pi)))
            THEN
               MarkAsReadWrite(rw) ;
               (* pass the address field of an unbounded variable *)
               PushTFtok (Adr, Address, OperandTok (pi)) ;
               PushTFAD (f^.TrueExit, f^.FalseExit, f^.Unbounded, f^.Dimension) ;
               PushT(1) ;
               BuildAdrFunction ;
               PopT(f^.TrueExit)
            ELSIF GetMode(OperandT(pi))=LeftValue
            THEN
               MarkAsReadWrite(rw) ;
               (* must dereference LeftValue (even if we are passing variable as a vararg) *)
               t := MakeTemporary (OperandTok (pi), RightValue) ;
               PutVar(t, GetSType (OperandT (pi))) ;
               CheckPointerThroughNil (tokpos, OperandT (pi)) ;
               doIndrX (OperandTok(pi), t, OperandT (pi)) ;
               f^.TrueExit := t
            END
         ELSE
            MetaErrorT2 (tokpos,
                         'attempting to pass too many parameters to procedure {%1a}, the {%2N} parameter does not exist',
                         Proc, i)
         END
      ELSIF IsForC AND IsUnboundedParam(Proc, i) AND
            (GetSType(OperandT(pi))#NulSym) AND IsArray(GetDType(OperandT(pi)))
      THEN
         f^.TrueExit := MakeLeftValue(OperandTok(pi), OperandT(pi), RightValue, Address) ;
         MarkAsReadWrite(rw)
      ELSIF IsForC AND IsUnboundedParam(Proc, i) AND
            (GetSType(OperandT(pi))#NulSym) AND IsUnbounded(GetDType(OperandT(pi)))
      THEN
         MarkAsReadWrite(rw) ;
         (* pass the address field of an unbounded variable *)
         PushTFtok (Adr, Address, OperandTok (pi)) ;
         PushTFAD (f^.TrueExit, f^.FalseExit, f^.Unbounded, f^.Dimension) ;
         PushT(1) ;
         BuildAdrFunction ;
         PopT(f^.TrueExit)
      ELSIF IsForC AND IsConstString(OperandT(pi)) AND
                        (IsUnboundedParam(Proc, i) OR (GetDType(GetParam(Proc, i))=Address))
      THEN
         f^.TrueExit := MakeLeftValue (OperandTok (pi),
                                       DeferMakeConstStringCnul (OperandTok (pi), OperandT (pi)),
                                       RightValue, Address) ;
         MarkAsReadWrite (rw)
      ELSIF IsUnboundedParam(Proc, i)
      THEN
         (* always pass constant strings with a nul terminator, but leave the HIGH as before.  *)
         IF IsConstString (OperandT(pi))
         THEN
            (* this is a Modula-2 string which must be nul terminated.  *)
            f^.TrueExit := DeferMakeConstStringM2nul (OperandTok (pi), OperandT (pi))
         END ;
         t := MakeTemporary (OperandTok (pi), RightValue) ;
         UnboundedType := GetSType(GetParam(Proc, i)) ;
         PutVar(t, UnboundedType) ;
         ParamType := GetSType(UnboundedType) ;
         IF OperandD(pi)=0
         THEN
            ArraySym := OperandT(pi)
         ELSE
            ArraySym := OperandA(pi)
         END ;
         IF IsVarParam(Proc, i)
         THEN
            MarkArrayWritten (OperandT (pi)) ;
            MarkArrayWritten (OperandA (pi)) ;
            MarkAsReadWrite(rw) ;
            AssignUnboundedVar (OperandTtok (pi), OperandT (pi), ArraySym, t, ParamType, OperandD (pi))
         ELSE
            MarkAsRead(rw) ;
            AssignUnboundedNonVar (OperandTtok (pi), OperandT (pi), ArraySym, t, ParamType, OperandD (pi))
         END ;
         f^.TrueExit := t
      ELSIF IsVarParam(Proc, i)
      THEN
         (* must reference by address, but we contain the type of the referenced entity *)
         MarkArrayWritten(OperandT(pi)) ;
         MarkArrayWritten(OperandA(pi)) ;
         MarkAsReadWrite(rw) ;
         f^.TrueExit := MakeLeftValue(OperandTok(pi), OperandT(pi), LeftValue, GetSType(GetParam(Proc, i)))
      ELSIF (NOT IsVarParam(Proc, i)) AND (GetMode(OperandT(pi))=LeftValue)
      THEN
         (* must dereference LeftValue *)
         t := MakeTemporary (OperandTok (pi), RightValue) ;
         PutVar(t, GetSType(OperandT(pi))) ;
         CheckPointerThroughNil (tokpos, OperandT (pi)) ;
         doIndrX (OperandTok(pi), t, OperandT(pi)) ;
         f^.TrueExit := t ;
         MarkAsRead(rw)
      ELSE
         MarkAsRead(rw)
      END ;
      INC(i) ;
      DEC(pi)
   END ;
   PushT(NoOfParameters)
END ManipulateParameters ;


(*
   CheckParameterOrdinals - check that ordinal values are within type range.
*)

PROCEDURE CheckParameterOrdinals ;
VAR
   tokno     : CARDINAL ;
   Proc,
   ProcSym   : CARDINAL ;
   Actual,
   FormalI   : CARDINAL ;
   ParamTotal,
   pi, i     : CARDINAL ;
BEGIN
   PopT (ParamTotal) ;
   PushT (ParamTotal) ;  (* Restore stack to origional state *)
   ProcSym := OperandT (ParamTotal+1+1) ;
   IF IsVar(ProcSym) AND IsProcType(GetDType(ProcSym))
   THEN
      (* Indirect procedure call.  *)
      Proc := SkipType(OperandF(ParamTotal+1+1))
   ELSE
      Proc := SkipConst(ProcSym)
   END ;
   i := 1 ;
   pi := ParamTotal+1 ;   (* stack index referencing stacked parameter, i *)
   WHILE i<=ParamTotal DO
      IF i<=NoOfParam(Proc)
      THEN
         FormalI := GetParam (Proc, i) ;
         Actual := OperandT (pi) ;
         tokno := OperandTok (pi) ;
         IF IsOrdinalType (GetLType (FormalI))
         THEN
            IF NOT IsSet (GetDType (FormalI))
            THEN
               (* tell code generator to test runtime values of assignment so ensure we
                  catch overflow and underflow *)
               BuildRange (InitParameterRangeCheck (tokno, Proc, i, FormalI, Actual))
            END
         END
      END ;
      INC (i) ;
      DEC (pi)
   END
END CheckParameterOrdinals ;


(*
   IsSameUnbounded - returns TRUE if unbounded types, t1, and, t2,
                     are compatible.
*)

PROCEDURE IsSameUnbounded (t1, t2: CARDINAL) : BOOLEAN ;
BEGIN
   Assert(IsUnbounded(t1)) ;
   Assert(IsUnbounded(t2)) ;
   RETURN( GetDType(t1)=GetDType(t2) )
END IsSameUnbounded ;


(*
   AssignUnboundedVar - assigns an Unbounded symbol fields,
                        ArrayAddress and ArrayHigh, from an array symbol.
                        UnboundedSym is not a VAR parameter and therefore
                        this procedure can complete both of the fields.
                        Sym can be a Variable with type Unbounded.
                        Sym can be a Variable with type Array.
                        Sym can be a String Constant.

                        ParamType is the TYPE of the parameter
*)

PROCEDURE AssignUnboundedVar (tok: CARDINAL;
                              Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   Type: CARDINAL ;
BEGIN
   IF IsConst(Sym)
   THEN
      MetaErrorT1 (tok, '{%1ad} cannot be passed to a VAR formal parameter', Sym)
   ELSIF IsVar(Sym)
   THEN
      Type := GetDType(Sym) ;
      IF Type = NulSym
      THEN
         MetaErrorT1 (tok, '{%1ad} has no type and cannot be passed to a VAR formal parameter', Sym)
      ELSIF IsUnbounded(Type)
      THEN
         IF Type = GetSType (UnboundedSym)
         THEN
            (* Copy Unbounded Symbol ie. UnboundedSym := Sym *)
            PushT (UnboundedSym) ;
            PushT (Sym) ;
            BuildAssignmentWithoutBounds (tok, FALSE, TRUE)
         ELSIF IsSameUnbounded (Type, GetSType (UnboundedSym)) OR
               IsGenericSystemType (ParamType)
         THEN
            UnboundedVarLinkToArray (tok, Sym, ArraySym, UnboundedSym, ParamType, dim)
         ELSE
            MetaErrorT1 (tok, '{%1ad} cannot be passed to a VAR formal parameter', Sym)
         END
      ELSIF IsArray (Type) OR IsGenericSystemType (ParamType)
      THEN
         UnboundedVarLinkToArray (tok, Sym, ArraySym, UnboundedSym, ParamType, dim)
      ELSE
         MetaErrorT1 (tok, '{%1ad} cannot be passed to a VAR formal parameter', Sym)
      END
   ELSE
      MetaErrorT1 (tok, '{%1ad} cannot be passed to a VAR formal parameter', Sym)
   END
END AssignUnboundedVar ;


(*
   AssignUnboundedNonVar - assigns an Unbounded symbol fields,
                           The difference between this procedure and
                           AssignUnboundedVar is that this procedure cannot
                           set the Unbounded.Address since the data from
                           Sym will be copied because parameter is NOT a VAR
                           parameter.
                           UnboundedSym is not a VAR parameter and therefore
                           this procedure can only complete the HIGH field
                           and not the ADDRESS field.
                           Sym can be a Variable with type Unbounded.
                           Sym can be a Variable with type Array.
                           Sym can be a String Constant.

                           ParamType is the TYPE of the paramater
*)

PROCEDURE AssignUnboundedNonVar (tok: CARDINAL;
                                 Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   Type: CARDINAL ;
BEGIN
   IF IsConst (Sym)  (* was IsConstString(Sym) *)
   THEN
      UnboundedNonVarLinkToArray (tok, Sym, ArraySym, UnboundedSym, ParamType, dim)
   ELSIF IsVar (Sym)
   THEN
      Type := GetDType (Sym) ;
      IF Type = NulSym
      THEN
         MetaErrorT1 (tok, '{%1ad} has no type and cannot be passed to a non VAR formal parameter', Sym)
      ELSIF IsUnbounded (Type)
      THEN
         UnboundedNonVarLinkToArray (tok, Sym, ArraySym, UnboundedSym, ParamType, dim)
      ELSIF IsArray (Type) OR IsGenericSystemType (ParamType)
      THEN
         UnboundedNonVarLinkToArray (tok, Sym, ArraySym, UnboundedSym, ParamType, dim)
      ELSE
         MetaErrorT1 (tok, 'illegal type parameter {%1Ead} expecting array or dynamic array', Sym)
      END
   ELSE
      MetaErrorT1 (tok, 'illegal parameter {%1Ead} which cannot be passed as {%kVAR} {%kARRAY} {%kOF} {%1tsad}', Sym)
   END
END AssignUnboundedNonVar ;


(*
   GenHigh - generates a HighOp but it checks if op3 is a
             L value and if so it dereferences it.  This
             is inefficient, however it is clean and we let the gcc
             backend detect these as common subexpressions.
             It will also detect that a R value -> L value -> R value
             via indirection and eleminate these.
*)

PROCEDURE GenHigh (tok: CARDINAL;
                   op1, op2, op3: CARDINAL) ;
VAR
   sym: CARDINAL ;
BEGIN
   IF (GetMode(op3)=LeftValue) AND IsUnbounded(GetSType(op3))
   THEN
      sym := MakeTemporary (tok, RightValue) ;
      PutVar (sym, GetSType (op3)) ;
      doIndrX (tok, sym, op3) ;
      GenQuadO (tok, HighOp, op1, op2, sym, TRUE)
   ELSE
      GenQuadO (tok, HighOp, op1, op2, op3, TRUE)
   END
END GenHigh ;


(*
   AssignHighField -
*)

PROCEDURE AssignHighField (tok: CARDINAL;
                           Sym, ArraySym, UnboundedSym, ParamType: CARDINAL;
                           actuali, formali: CARDINAL) ;
VAR
   ReturnVar,
   ArrayType,
   Field    : CARDINAL ;
BEGIN
   (* Unbounded.ArrayHigh := HIGH(ArraySym) *)
   PushTFtok (UnboundedSym, GetSType (UnboundedSym), tok) ;
   Field := GetUnboundedHighOffset (GetSType (UnboundedSym), formali) ;
   PushTFtok (Field, GetSType (Field), tok) ;
   PushT (1) ;
   BuildDesignatorRecord (tok) ;
   IF IsGenericSystemType (ParamType)
   THEN
      IF IsConstString (Sym)
      THEN
         PushTtok (DeferMakeLengthConst (tok, Sym), tok)
      ELSE
         ArrayType := GetSType (Sym) ;
         IF IsUnbounded (ArrayType)
         THEN
            (*
             *  SIZE(parameter) DIV TSIZE(ParamType)
             *  however in this case parameter
             *  is an unbounded symbol and therefore we must use
             *  (HIGH(parameter)+1)*SIZE(unbounded type) DIV TSIZE(ParamType)
             *
             *  we call upon the function SIZE(ArraySym)
             *  remember SIZE doubles as
             *  (HIGH(a)+1) * SIZE(ArrayType) for unbounded symbols
             *)
            PushTFtok (calculateMultipicand (tok, ArraySym, ArrayType, actuali-1), Cardinal, tok) ;
            PushT (DivideTok) ;        (* Divide by                    *)
            PushTFtok (TSize, Cardinal, tok) ; (* TSIZE(ParamType)     *)
            PushTtok (ParamType, tok) ;
            PushT (1) ;                (* 1 parameter for TSIZE()      *)
            BuildFunctionCall (FALSE) ;
            BuildBinaryOp
         ELSE
            (* SIZE(parameter) DIV TSIZE(ParamType)                    *)
            PushTFtok (TSize, Cardinal, tok) ;  (* TSIZE(ArrayType)    *)
            PushTtok (ArrayType, tok) ;
            PushT (1) ;                (* 1 parameter for TSIZE()      *)
            BuildFunctionCall (TRUE) ;
            PushT (DivideTok) ;        (* Divide by                    *)
            PushTFtok (TSize, Cardinal, tok) ; (* TSIZE(ParamType)     *)
            PushTtok (ParamType, tok) ;
            PushT (1) ;                (* 1 parameter for TSIZE()      *)
            BuildFunctionCall (TRUE) ;
            BuildBinaryOp
         END ;
         (* now convert from no of elements into HIGH by subtracting 1 *)
         PushT (MinusTok) ;           (* -1                            *)
         PushTtok (MakeConstLit (tok, MakeKey('1'), Cardinal), tok) ;
         BuildBinaryOp
      END
   ELSE
      ReturnVar := MakeTemporary (tok, RightValue) ;
      PutVar (ReturnVar, Cardinal) ;
      IF (actuali # formali) AND (ArraySym # NulSym) AND IsUnbounded (GetSType (ArraySym))
      THEN
         GenHigh (tok, ReturnVar, actuali, ArraySym)
      ELSE
         GenHigh (tok, ReturnVar, formali, Sym)
      END ;
      PushTFtok (ReturnVar, GetSType(ReturnVar), tok)
   END ;
   BuildAssignmentWithoutBounds (tok, FALSE, TRUE)
END AssignHighField ;


(*
   AssignHighFields -
*)

PROCEDURE AssignHighFields (tok: CARDINAL;
                            Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   type            : CARDINAL ;
   actuali, formali,
   actualn, formaln: CARDINAL ;
BEGIN
   type := GetDType (Sym) ;
   actualn := 1 ;
   IF (type # NulSym) AND (IsUnbounded (type) OR IsArray (type))
   THEN
      actualn := GetDimension (type)
   END ;
   actuali := dim + 1 ;
   formali := 1 ;
   formaln := GetDimension (GetDType (UnboundedSym)) ;
   WHILE (actuali < actualn) AND (formali < formaln) DO
      AssignHighField (tok, Sym, ArraySym, UnboundedSym, NulSym, actuali, formali) ;
      INC (actuali) ;
      INC (formali)
   END ;
   AssignHighField (tok, Sym, ArraySym, UnboundedSym, ParamType, actuali, formali)
END AssignHighFields ;


(*
   UnboundedNonVarLinkToArray - links an array, ArraySym, to an unbounded
                                array, UnboundedSym. The parameter is a
                                NON VAR variety.
*)

PROCEDURE UnboundedNonVarLinkToArray (tok: CARDINAL;
                                      Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   Field,
   AddressField: CARDINAL ;
BEGIN
   (* Unbounded.ArrayAddress := to be assigned at runtime.  *)
   PushTFtok (UnboundedSym, GetSType (UnboundedSym), tok) ;

   Field := GetUnboundedAddressOffset(GetSType(UnboundedSym)) ;
   PushTFtok (Field, GetSType(Field), tok) ;
   PushT (1) ;
   BuildDesignatorRecord (tok) ;
   PopT (AddressField) ;

   (* caller saves non var unbounded array contents.  *)
   GenQuadO (tok, UnboundedOp, AddressField, NulSym, Sym, FALSE) ;

   AssignHighFields (tok, Sym, ArraySym, UnboundedSym, ParamType, dim)
END UnboundedNonVarLinkToArray ;


(*
   UnboundedVarLinkToArray - links an array, ArraySym, to an unbounded array,
                             UnboundedSym. The parameter is a VAR variety.
*)

PROCEDURE UnboundedVarLinkToArray (tok: CARDINAL;
                                   Sym, ArraySym, UnboundedSym, ParamType: CARDINAL; dim: CARDINAL) ;
VAR
   SymType,
   Field  : CARDINAL ;
BEGIN
   SymType := GetSType (Sym) ;
   (* Unbounded.ArrayAddress := ADR(Sym) *)
   PushTFtok (UnboundedSym, GetSType (UnboundedSym), tok) ;
   Field := GetUnboundedAddressOffset (GetSType (UnboundedSym)) ;
   PushTFtok (Field, GetSType (Field), tok) ;
   PushT (1) ;
   BuildDesignatorRecord (tok) ;
   PushTFtok (Adr, Address, tok) ;   (* ADR (Sym).  *)
   IF IsUnbounded (SymType) AND (dim = 0)
   THEN
      PushTFADtok (Sym, SymType, UnboundedSym, dim, tok)
   ELSE
      PushTFADtok (Sym, SymType, ArraySym, dim, tok)
   END ;
   PushT (1) ;               (* 1 parameter for ADR().  *)
   BuildFunctionCall (FALSE) ;
   BuildAssignmentWithoutBounds (tok, FALSE, TRUE) ;

   AssignHighFields (tok, Sym, ArraySym, UnboundedSym, ParamType, dim)
END UnboundedVarLinkToArray ;


(*
   BuildPseudoProcedureCall - builds a pseudo procedure call.
                              This procedure does not directly alter the
                              stack, but by calling routines the stack
                              will change in the following way when this
                              procedure returns.

                              The Stack:


                              Entry                      Exit

                       Ptr ->
                              +----------------+
                              | NoOfParam      |
                              |----------------|
                              | Param 1        |
                              |----------------|
                              | Param 2        |
                              |----------------|
                              .                .
                              .                .
                              .                .
                              |----------------|
                              | Param #        |
                              |----------------|
                              | ProcSym | Type |         Empty
                              |----------------|
*)

PROCEDURE BuildPseudoProcedureCall (tokno: CARDINAL) ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   ProcSym := OperandT (NoOfParam + 1) ;
   PushT (NoOfParam) ;
   (* Compile time stack restored to entry state *)
   IF ProcSym = New
   THEN
      BuildNewProcedure (tokno)
   ELSIF ProcSym = Dispose
   THEN
      BuildDisposeProcedure (tokno)
   ELSIF ProcSym = Inc
   THEN
      BuildIncProcedure
   ELSIF ProcSym = Dec
   THEN
      BuildDecProcedure
   ELSIF ProcSym = Incl
   THEN
      BuildInclProcedure
   ELSIF ProcSym = Excl
   THEN
      BuildExclProcedure
   ELSIF ProcSym = Throw
   THEN
      BuildThrowProcedure
   ELSE
      InternalError  ('pseudo procedure not implemented yet')
   END
END BuildPseudoProcedureCall ;


(*
   GetItemPointedTo - returns the symbol type that is being pointed to
                      by Sym.
*)

PROCEDURE GetItemPointedTo (Sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsPointer (Sym)
   THEN
      RETURN GetSType (Sym)
   ELSIF IsVar (Sym) OR IsType (Sym)
   THEN
      RETURN GetItemPointedTo (GetSType (Sym))
   ELSE
      RETURN NulSym
   END
END GetItemPointedTo ;


(*
   BuildThrowProcedure - builds the pseudo procedure call M2RTS.Throw.
                         The Stack:


                         Entry                      Exit

                Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |
                         |----------------|
                         .                .
                         .                .
                         .                .
                         |----------------|
                         | Param #        |
                         |----------------|
                         | ProcSym | Type |         Empty
                         |----------------|
*)

PROCEDURE BuildThrowProcedure ;
VAR
   functok  : CARDINAL ;
   op       : CARDINAL ;
   NoOfParam: CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok  := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      op := OperandT (NoOfParam) ;
      GenQuadO (functok, ThrowOp, NulSym, NulSym, op, FALSE)
   ELSE
      MetaErrorT1 (functok, 'the pseudo procedure %{1Ea} takes one INTEGER parameter', Throw)
   END ;
   PopN (NoOfParam+1)
END BuildThrowProcedure ;


(*
   BuildReThrow - creates a ThrowOp _ _ NulSym, indicating that
                  the exception needs to be rethrown.  The stack
                  is unaltered.
*)

PROCEDURE BuildReThrow (tokenno: CARDINAL) ;
BEGIN
   GenQuadO (tokenno, ThrowOp, NulSym, NulSym, NulSym, FALSE)
END BuildReThrow ;


(*
   BuildNewProcedure - builds the pseudo procedure call NEW.
                       This procedure is traditionally a "macro" for
                       NEW(x, ...) --> ALLOCATE(x, TSIZE(x^, ...))
                       One method of implementation is to emulate a "macro"
                       processor by pushing the relevant input tokens
                       back onto the input stack.
                       However this causes two problems:

                       (i)  Unnecessary code is produced for x^
                       (ii) SIZE must be imported from SYSTEM
                       Therefore we chose an alternative method of
                       implementation;
                       generate quadruples for ALLOCATE(x, TSIZE(x^, ...))
                       this, although slightly more efficient,
                       is more complex and circumvents problems (i) and (ii).

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildNewProcedure (functok: CARDINAL) ;
VAR
   NoOfParam,
   SizeSym,
   PtrSym,
   ProcSym    : CARDINAL ;
   paramtok,
   combinedtok: CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   IF NoOfParam>=1
   THEN
      ProcSym := RequestSym (functok, MakeKey('ALLOCATE')) ;
      IF (ProcSym#NulSym) AND IsProcedure(ProcSym)
      THEN
         PtrSym := OperandT (NoOfParam) ;
         paramtok := OperandTtok (1) ;
         IF IsReallyPointer(PtrSym)
         THEN
            combinedtok := MakeVirtualTok (functok, functok, paramtok) ;
            (*
               Build macro: ALLOCATE( PtrSym, SIZE(PtrSym^) )
            *)
            PushTFtok (TSize, Cardinal, paramtok) ;(* Procedure      *)
                                      (* x^             *)
            PushTtok (GetItemPointedTo (PtrSym), paramtok) ;
            PushT (1) ;               (* One parameter  *)
            BuildFunctionCall (FALSE) ;
            PopT (SizeSym) ;

            PushTtok (ProcSym, combinedtok) ;  (* ALLOCATE       *)
            PushTtok (PtrSym, paramtok) ;      (* x              *)
            PushTtok (SizeSym, paramtok) ;     (* TSIZE(x^)      *)
            PushT (2) ;                        (* Two parameters *)
            BuildProcedureCall (combinedtok)
         ELSE
            MetaErrorT0 (paramtok, 'parameter to {%EkNEW} must be a pointer')
         END
      ELSE
         MetaErrorT0 (functok, '{%E}ALLOCATE procedure not found for NEW substitution')
      END
   ELSE
      MetaErrorT0 (functok, 'the pseudo procedure {%EkNEW} has one or more parameters')
   END ;
   PopN (NoOfParam+1)
END BuildNewProcedure ;


(*
   BuildDisposeProcedure - builds the pseudo procedure call DISPOSE.
                           This procedure is traditionally a "macro" for
                           DISPOSE(x) --> DEALLOCATE(x, TSIZE(x^))
                           One method of implementation is to emulate a "macro"
                           processor by pushing the relevant input tokens
                           back onto the input stack.
                           However this causes two problems:

                           (i)  Unnecessary code is produced for x^
                           (ii) TSIZE must be imported from SYSTEM
                           Therefore we chose an alternative method of
                           implementation;
                           generate quadruples for DEALLOCATE(x, TSIZE(x^))
                           this, although slightly more efficient,
                           is more complex and circumvents problems (i)
                           and (ii).

                           The Stack:


                           Entry                      Exit

                    Ptr ->
                           +----------------+
                           | NoOfParam      |
                           |----------------|
                           | Param 1        |
                           |----------------|
                           | Param 2        |
                           |----------------|
                           .                .
                           .                .
                           .                .
                           |----------------|
                           | Param #        |
                           |----------------|
                           | ProcSym | Type |         Empty
                           |----------------|
*)

PROCEDURE BuildDisposeProcedure (functok: CARDINAL) ;
VAR
   NoOfParam,
   SizeSym,
   PtrSym,
   ProcSym    : CARDINAL ;
   combinedtok,
   paramtok   : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   IF NoOfParam>=1
   THEN
      ProcSym := RequestSym (functok, MakeKey ('DEALLOCATE')) ;
      IF (ProcSym # NulSym) AND IsProcedure (ProcSym)
      THEN
         PtrSym := OperandT (NoOfParam) ;
         paramtok := OperandTtok (1) ;
         IF IsReallyPointer (PtrSym)
         THEN
            combinedtok := MakeVirtualTok (functok, functok, paramtok) ;
            (*
               Build macro: DEALLOCATE( PtrSym, TSIZE(PtrSym^) )
            *)
            PushTFtok (TSize, Cardinal, paramtok) ;(* Procedure      *)
                                      (* x^             *)
            PushTtok (GetItemPointedTo(PtrSym), paramtok) ;
            PushT (1) ;               (* One parameter  *)
            BuildFunctionCall (FALSE) ;
            PopT (SizeSym) ;

            PushTtok (ProcSym, combinedtok) ;         (* DEALLOCATE     *)
            PushTtok (PtrSym, paramtok) ;             (* x              *)
            PushTtok (SizeSym, paramtok) ;            (* TSIZE(x^)      *)
            PushT (2) ;                               (* Two parameters *)
            BuildProcedureCall (combinedtok)
         ELSE
            MetaErrorT0 (paramtok, 'argument to {%EkDISPOSE} must be a pointer')
         END
      ELSE
         MetaErrorT0 (functok, '{%E}DEALLOCATE procedure not found for DISPOSE substitution')
      END
   ELSE
      MetaErrorT0 (functok, 'the pseudo procedure {%EkDISPOSE} has one or more parameters')
   END ;
   PopN (NoOfParam+1)
END BuildDisposeProcedure ;


(*
   CheckRangeIncDec - performs des := des <tok> expr
                      with range checking (if enabled).

                               Stack
                      Entry              Exit

                                     +------------+
                      empty          | des + expr |
                                     |------------|
*)

PROCEDURE CheckRangeIncDec (tokenpos: CARDINAL; des, expr: CARDINAL; tok: Name) ;
VAR
   dtype, etype: CARDINAL ;
BEGIN
   dtype := GetDType(des) ;
   etype := GetDType(expr) ;
   IF (etype = NulSym) AND IsPointer (GetTypeMode (des))
   THEN
      expr := ConvertToAddress (tokenpos, expr) ;
      etype := Address
   END ;
   IF WholeValueChecking AND (NOT MustNotCheckBounds)
   THEN
      IF tok=PlusTok
      THEN
         BuildRange (InitIncRangeCheck (des, expr))
      ELSE
         BuildRange (InitDecRangeCheck (des, expr))
      END
   END ;

   IF IsExpressionCompatible (dtype, etype)
   THEN
      (* the easy case simulate a straightforward macro *)
      PushTF (des, dtype) ;
      PushT (tok) ;
      PushTF (expr, etype) ;
      doBuildBinaryOp (FALSE, TRUE)
   ELSE
      IF (IsOrdinalType (dtype) OR (dtype = Address) OR IsPointer (dtype)) AND
         (IsOrdinalType (etype) OR (etype = Address) OR IsPointer (etype))
      THEN
         PushTF (des, dtype) ;
         PushT (tok) ;
         PushTF (Convert, NulSym) ;
         PushT (dtype) ;
         PushT (expr) ;
         PushT (2) ;          (* Two parameters *)
         BuildConvertFunction (Convert, FALSE) ;
         doBuildBinaryOp (FALSE, TRUE)
      ELSE
         IF tok=PlusTok
         THEN
            MetaError0 ('cannot perform {%EkINC} using non ordinal types')
         ELSE
            MetaError0 ('cannot perform {%EkDEC} using non ordinal types')
         END ;
         PushTFtok (MakeConstLit (tokenpos, MakeKey ('0'), NulSym), NulSym, tokenpos)
      END
   END
END CheckRangeIncDec ;


(*
   BuildIncProcedure - builds the pseudo procedure call INC.
                       INC is a procedure which increments a variable.
                       It takes one or two parameters:
                       INC(a, b)  or  INC(a)
                       a := a+b   or  a := a+1

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildIncProcedure ;
VAR
   proctok   : CARDINAL ;
   NoOfParam,
   dtype,
   OperandSym,
   VarSym,
   TempSym   : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   proctok := OperandTtok (NoOfParam + 1) ;
   IF (NoOfParam = 1) OR (NoOfParam = 2)
   THEN
      VarSym := OperandT (NoOfParam) ;  (* bottom/first parameter *)
      IF IsVar (VarSym)
      THEN
         dtype := GetDType (VarSym) ;
         IF NoOfParam = 2
         THEN
            OperandSym := DereferenceLValue (OperandTok (1), OperandT (1))
         ELSE
            PushOne (proctok, dtype,
                     'the {%EkINC} will cause an overflow {%1ad}') ;
	    PopT (OperandSym)
         END ;

         PushT (VarSym) ;
         TempSym := DereferenceLValue (OperandTok (NoOfParam), VarSym) ;
         CheckRangeIncDec (proctok, TempSym, OperandSym, PlusTok) ;  (* TempSym + OperandSym *)
         BuildAssignmentWithoutBounds (proctok, FALSE, TRUE)   (* VarSym := TempSym + OperandSym *)
      ELSE
         MetaErrorT1 (proctok,
                      'base procedure {%EkINC} expects a variable as a parameter but was given {%1Ed}',
                      VarSym)
      END
   ELSE
      MetaErrorT0 (proctok,
                   'the base procedure {%EkINC} expects 1 or 2 parameters')
   END ;
   PopN (NoOfParam + 1)
END BuildIncProcedure ;


(*
   BuildDecProcedure - builds the pseudo procedure call DEC.
                       DEC is a procedure which decrements a variable.
                       It takes one or two parameters:
                       DEC(a, b)  or  DEC(a)
                       a := a-b   or  a := a-1

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildDecProcedure ;
VAR
   proctok,
   NoOfParam,
   dtype,
   OperandSym,
   VarSym,
   TempSym   : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   proctok := OperandTtok (NoOfParam + 1) ;
   IF (NoOfParam = 1) OR (NoOfParam = 2)
   THEN
      VarSym := OperandT (NoOfParam) ;  (* bottom/first parameter *)
      IF IsVar (VarSym)
      THEN
         dtype := GetDType (VarSym) ;
         IF NoOfParam = 2
         THEN
            OperandSym := DereferenceLValue (OperandTok (1), OperandT (1))
         ELSE
            PushOne (proctok, dtype,
                     'the {%EkDEC} will cause an overflow {%1ad}') ;
	    PopT (OperandSym)
         END ;

         PushT (VarSym) ;
         TempSym := DereferenceLValue (OperandTok (NoOfParam), VarSym) ;
         CheckRangeIncDec (proctok, TempSym, OperandSym, MinusTok) ;  (* TempSym - OperandSym *)
         BuildAssignmentWithoutBounds (proctok, FALSE, TRUE)   (* VarSym := TempSym - OperandSym *)
      ELSE
         MetaErrorT1 (proctok,
                      'base procedure {%EkDEC} expects a variable as a parameter but was given {%1Ed}',
                      VarSym)
      END
   ELSE
      MetaErrorT0 (proctok,
                   'the base procedure {%EkDEC} expects 1 or 2 parameters')
   END ;
   PopN (NoOfParam + 1)
END BuildDecProcedure ;


(*
   DereferenceLValue - checks to see whether, operand, is declare as an LValue
                       and if so it dereferences it.
*)

PROCEDURE DereferenceLValue (tok: CARDINAL; operand: CARDINAL) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   IF GetMode (operand) = LeftValue
   THEN
      (* dereference the pointer *)
      sym := MakeTemporary (tok, AreConstant(IsConst(operand))) ;
      PutVar(sym, GetSType (operand)) ;

      PushTtok (sym, tok) ;
      PushTtok (operand, tok) ;
      BuildAssignmentWithoutBounds (tok, FALSE, TRUE) ;
      RETURN sym
   ELSE
      RETURN operand
   END
END DereferenceLValue ;


(*
   BuildInclProcedure - builds the pseudo procedure call INCL.
                        INCL is a procedure which adds bit b into a BITSET a.
                        It takes two parameters:
                        INCL(a, b)

                        a := a + {b}

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildInclProcedure ;
VAR
   proctok,
   optok     : CARDINAL ;
   NoOfParam,
   DerefSym,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   proctok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam = 2
   THEN
      VarSym := OperandT (2) ;
      MarkArrayWritten (OperandA (2)) ;
      OperandSym := OperandT (1) ;
      optok := OperandTok (1) ;
      IF IsVar (VarSym)
      THEN
         IF IsSet (GetDType (VarSym))
         THEN
            DerefSym := DereferenceLValue (optok, OperandSym) ;
            BuildRange (InitInclCheck (VarSym, DerefSym)) ;
            GenQuadO (proctok, InclOp, VarSym, NulSym, DerefSym, FALSE)
         ELSE
            MetaErrorT1 (proctok,
                         'the first parameter to {%EkINCL} must be a set variable but is {%1Ed}',
                         VarSym)
         END
      ELSE
         MetaErrorT1 (proctok,
                      'base procedure {%EkINCL} expects a variable as a parameter but is {%1Ed}',
                      VarSym)
      END
   ELSE
      MetaErrorT0 (proctok, 'the base procedure {%EkINCL} expects 1 or 2 parameters')
   END ;
   PopN (NoOfParam + 1)
END BuildInclProcedure ;


(*
   BuildExclProcedure - builds the pseudo procedure call EXCL.
                        INCL is a procedure which removes bit b from SET a.
                        It takes two parameters:
                        EXCL(a, b)

                        a := a - {b}

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildExclProcedure ;
VAR
   proctok,
   optok     : CARDINAL ;
   NoOfParam,
   DerefSym,
   OperandSym,
   VarSym    : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   proctok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT (2) ;
      MarkArrayWritten (OperandA(2)) ;
      OperandSym := OperandT (1) ;
      optok := OperandTok (1) ;
      IF IsVar (VarSym)
      THEN
         IF IsSet (GetDType (VarSym))
         THEN
            DerefSym := DereferenceLValue (optok, OperandSym) ;
            BuildRange (InitExclCheck (VarSym, DerefSym)) ;
            GenQuadO (proctok, ExclOp, VarSym, NulSym, DerefSym, FALSE)
         ELSE
            MetaErrorT1 (proctok,
                         'the first parameter to {%EkEXCL} must be a set variable but is {%1Ed}',
                         VarSym)
         END
      ELSE
         MetaErrorT1 (proctok,
                      'base procedure {%EkEXCL} expects a variable as a parameter but is {%1Ed}',
                      VarSym)
      END
   ELSE
      MetaErrorT0 (proctok,
                   'the base procedure {%EkEXCL} expects 1 or 2 parameters')
   END ;
   PopN (NoOfParam + 1)
END BuildExclProcedure ;


(*
   CheckBuildFunction - checks to see whether ProcSym is a function
                        and if so it adds a TempSym value which will
                        hold the return value once the function finishes.
                        This procedure also generates an error message
                        if the user is calling a function and ignoring
                        the return result.  The additional TempSym
                        is not created if ProcSym is a procedure
                        and the stack is unaltered.

                        The Stack:


                       Entry                      Exit

                Ptr ->

                                                  +----------------+
                                                  | ProcSym | Type |
                       +----------------+         |----------------|
                       | ProcSym | Type |         | TempSym | Type |
                       |----------------|         |----------------|
*)

PROCEDURE CheckBuildFunction () : BOOLEAN ;
VAR
   n            : Name ;
   tokpos,
   TempSym,
   ProcSym, Type: CARDINAL ;
BEGIN
   PopTFtok(ProcSym, Type, tokpos) ;
   IF IsVar(ProcSym) AND IsProcType(Type)
   THEN
      IF GetSType(Type)#NulSym
      THEN
         TempSym := MakeTemporary (tokpos, RightValue) ;
         PutVar(TempSym, GetSType(Type)) ;
         PushTFtok(TempSym, GetSType(Type), tokpos) ;
         PushTFtok(ProcSym, Type, tokpos) ;
         IF NOT IsReturnOptional(Type)
         THEN
            IF IsTemporary(ProcSym)
            THEN
               ErrorFormat0 (NewError (tokpos),
                             'function is being called but its return value is ignored')
            ELSE
               n := GetSymName (ProcSym) ;
               ErrorFormat1 (NewError (tokpos),
                            'function (%a) is being called but its return value is ignored', n)
            END
         END ;
         RETURN TRUE
      END
   ELSIF IsProcedure(ProcSym) AND (Type#NulSym)
   THEN
      TempSym := MakeTemporary (tokpos, RightValue) ;
      PutVar(TempSym, Type) ;
      PushTFtok(TempSym, Type, tokpos) ;
      PushTFtok(ProcSym, Type, tokpos) ;
      IF NOT IsReturnOptional(ProcSym)
      THEN
         n := GetSymName(ProcSym) ;
         ErrorFormat1(NewError(tokpos),
                      'function (%a) is being called but its return value is ignored', n)
      END ;
      RETURN TRUE
   END ;
   PushTFtok (ProcSym, Type, tokpos) ;
   RETURN FALSE
END CheckBuildFunction ;


(*
   BuildFunctionCall - builds a function call.
                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|
*)

PROCEDURE BuildFunctionCall (ConstExpr: BOOLEAN) ;
VAR
   paramtok,
   combinedtok,
   functok,
   NoOfParam,
   ProcSym    : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok  := OperandTtok (NoOfParam + 1) ;
   ProcSym := OperandT (NoOfParam + 1) ;
   ProcSym := SkipConst (ProcSym) ;
   PushT (NoOfParam) ;
   (* Compile time stack restored to entry state.  *)
   IF IsUnknown (ProcSym)
   THEN
      paramtok := OperandTtok (1) ;
      combinedtok := MakeVirtual2Tok (functok, paramtok) ;
      MetaErrorT1 (functok, 'procedure function {%1Ea} is undefined', ProcSym) ;
      PopN (NoOfParam + 2) ;
      (* Fake return value to continue compiling.  *)
      PushT (MakeConstLit (combinedtok, MakeKey ('0'), NulSym))
   ELSIF IsAModula2Type (ProcSym)
   THEN
      ManipulatePseudoCallParameters ;
      BuildTypeCoercion (ConstExpr)
   ELSIF IsPseudoSystemFunction (ProcSym) OR
         IsPseudoBaseFunction (ProcSym)
   THEN
      ManipulatePseudoCallParameters ;
      BuildPseudoFunctionCall (ConstExpr)
   ELSE
      BuildRealFunctionCall (functok, ConstExpr)
   END
END BuildFunctionCall ;


(*
   BuildConstFunctionCall - builds a function call and checks that this function can be
                            called inside a ConstExpression.

                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param 1        |
                            |----------------|
                            | Param 2        |
                            |----------------|
                            .                .
                            .                .
                            .                .
                            |----------------|
                            | Param #        |                        <- Ptr
                            |----------------|         +------------+
                            | ProcSym | Type |         | ReturnVar  |
                            |----------------|         |------------|

*)

PROCEDURE BuildConstFunctionCall ;
VAR
   functok,
   combinedtok,
   paramtok,
   ConstExpression,
   NoOfParam,
   ProcSym        : CARDINAL ;
BEGIN
   DisplayStack ;
   PopT(NoOfParam) ;
   ProcSym := OperandT (NoOfParam + 1) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF CompilerDebugging
   THEN
      printf2 ('procsym = %d  token = %d\n', ProcSym, functok) ;
      (* ErrorStringAt (InitString ('constant function'), functok).  *)
   END ;
   PushT (NoOfParam) ;
   IF (ProcSym # Convert) AND
      (IsPseudoBaseFunction (ProcSym) OR
       IsPseudoSystemFunctionConstExpression (ProcSym) OR
       (IsProcedure (ProcSym) AND IsProcedureBuiltin (ProcSym)))
   THEN
      BuildFunctionCall (TRUE)
   ELSE
      IF IsAModula2Type (ProcSym)
      THEN
         (* Type conversion.  *)
         IF NoOfParam = 1
         THEN
            ConstExpression := OperandT (NoOfParam + 1) ;
            paramtok := OperandTtok (NoOfParam + 1) ;
            PopN (NoOfParam + 2) ;
            (* Build macro: CONVERT( ProcSym, ConstExpression ).  *)
            PushTFtok (Convert, NulSym, functok) ;
            PushTtok (ProcSym, functok) ;
            PushTtok (ConstExpression, paramtok) ;
            PushT (2) ;  (* Two parameters.  *)
            BuildConvertFunction (Convert, TRUE)
         ELSE
            MetaErrorT0 (functok, '{%E}a constant type conversion can only have one argument')
         END
      ELSE
         (* Error issue message and fake return stack.  *)
         IF Iso
         THEN
            MetaErrorT0 (functok, 'the only functions permissible in a constant expression are: {%kCAP}, {%kCHR}, {%kCMPLX}, {%kFLOAT}, {%kHIGH}, {%kIM}, {%kLENGTH}, {%kMAX}, {%kMIN}, {%kODD}, {%kORD}, {%kRE}, {%kSIZE}, {%kTSIZE}, {%kTRUNC}, {%kVAL} and gcc builtins')
         ELSE
            MetaErrorT0 (functok, 'the only functions permissible in a constant expression are: {%kCAP}, {%kCHR}, {%kFLOAT}, {%kHIGH}, {%kMAX}, {%kMIN}, {%kODD}, {%kORD}, {%kSIZE}, {%kTSIZE}, {%kTRUNC}, {%kVAL} and gcc builtins')
         END ;
	 IF NoOfParam > 0
	 THEN
            paramtok := OperandTtok (NoOfParam + 1) ;
            combinedtok := MakeVirtualTok (functok, functok, paramtok)
         ELSE
	    combinedtok := functok
         END ;
         PopN (NoOfParam+2) ;
         PushT (MakeConstLit (combinedtok, MakeKey('0'), NulSym))   (* Fake return value to continue compiling.  *)
      END
   END
END BuildConstFunctionCall ;


(*
   BuildTypeCoercion - builds the type coersion.
                       Modula-2 allows types to be coersed with no runtime
                       penility.
                       It insists that the TSIZE(t1)=TSIZE(t2) where
                       t2 variable := t2(variable of type t1).
                       The ReturnVar on the stack is of type t2.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|

                       Quadruples:

                       CoerceOp  ReturnVar  Type  Param1

                       A type coercion will only be legal if the different
                       types have exactly the same size.
                       Since we can only decide this after M2Eval has processed
                       the symbol table then we create a quadruple explaining
                       the coercion taking place, the code generator can test
                       this assertion and report an error if the type sizes
                       differ.
*)

PROCEDURE BuildTypeCoercion (ConstExpr: BOOLEAN) ;
VAR
   resulttok,
   proctok,
   exptok   : CARDINAL ;
   r,
   exp,
   NoOfParam,
   ReturnVar,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   ProcSym := OperandT (NoOfParam+1) ;
   proctok := OperandTok (NoOfParam+1) ;
   IF NOT IsAModula2Type (ProcSym)
   THEN
      MetaError1 ('coersion expecting a type, seen {%1Ea} which is {%1Ed}', ProcSym)
   END ;
   IF NoOfParam = 1
   THEN
      PopTrwtok (exp, r, exptok) ;
      MarkAsRead (r) ;
      resulttok := MakeVirtual2Tok (proctok, exptok) ;
      PopN (1) ;   (* Pop procedure.  *)
      IF ConstExprError (ProcSym, exp, exptok, ConstExpr)
      THEN
         ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
         PutVar (ReturnVar, ProcSym) ;  (* Set ReturnVar's TYPE.  *)
      ELSIF IsConst (exp) OR IsVar (exp)
      THEN
         ReturnVar := MakeTemporary (resulttok, AreConstant (IsConst (exp))) ;
         PutVar (ReturnVar, ProcSym) ;  (* Set ReturnVar's TYPE.  *)
         GenQuad (CoerceOp, ReturnVar, ProcSym, exp)
      ELSE
         MetaError2 ('trying to coerse {%1EMRad} which is not a variable or constant into {%2ad}',
                     exp, ProcSym) ;
         MetaError2 ('trying to coerse {%1ECad} which is not a variable or constant into {%2ad}',
                     exp, ProcSym) ;
         ReturnVar := MakeTemporary (resulttok, RightValue) ;
         PutVar (ReturnVar, ProcSym)   (* Set ReturnVar's TYPE.  *)
      END ;
      PushTFtok (ReturnVar, ProcSym, resulttok)
   ELSE
      MetaError0 ('{%E}only one parameter expected in a TYPE coersion')
   END
END BuildTypeCoercion ;


(*
   BuildRealFunctionCall - builds a function call.
                           The Stack:


                           Entry                      Exit

                    Ptr ->
                           +----------------+
                           | NoOfParam      |
                           |----------------|
                           | Param 1        |
                           |----------------|
                           | Param 2        |
                           |----------------|
                           .                .
                           .                .
                           .                .
                           |----------------|
                           | Param #        |                        <- Ptr
                           |----------------|         +------------+
                           | ProcSym | Type |         | ReturnVar  |
                           |----------------|         |------------|
*)

PROCEDURE BuildRealFunctionCall (tokno: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT(NoOfParam) ;
   PushT(NoOfParam) ;
   ProcSym := OperandT (NoOfParam+2) ;
   ProcSym := SkipConst (ProcSym) ;
   IF IsVar(ProcSym)
   THEN
      (* Procedure Variable therefore get its type to see if it is a FOR "C" call.  *)
      ProcSym := SkipType (OperandF (NoOfParam+2))
   END ;
   IF IsDefImp (GetScope (ProcSym)) AND IsDefinitionForC (GetScope (ProcSym))
   THEN
      BuildRealFuncProcCall (tokno, TRUE, TRUE, ConstExpr)
   ELSE
      BuildRealFuncProcCall (tokno, TRUE, FALSE, ConstExpr)
   END
END BuildRealFunctionCall ;


(*
   BuildPseudoFunctionCall - builds the pseudo function
                             The Stack:


                             Entry                      Exit

                      Ptr ->
                             +----------------+
                             | NoOfParam      |
                             |----------------|
                             | Param 1        |
                             |----------------|
                             | Param 2        |
                             |----------------|
                             .                .
                             .                .
                             .                .
                             |----------------|
                             | Param #        |                        <- Ptr
                             |----------------|         +------------+
                             | ProcSym | Type |         | ReturnVar  |
                             |----------------|         |------------|

*)

PROCEDURE BuildPseudoFunctionCall (ConstExpr: BOOLEAN) ;
VAR
   NoOfParam,
   ProcSym  : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   ProcSym := OperandT (NoOfParam+1) ;
   ProcSym := SkipConst (ProcSym) ;
   PushT (NoOfParam) ;
   (* Compile time stack restored to entry state.  *)
   IF ProcSym = High
   THEN
      BuildHighFunction
   ELSIF ProcSym = LengthS
   THEN
      BuildLengthFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Adr
   THEN
      BuildAdrFunction
   ELSIF ProcSym = Size
   THEN
      BuildSizeFunction
   ELSIF ProcSym = TSize
   THEN
      BuildTSizeFunction
   ELSIF ProcSym = TBitSize
   THEN
      BuildTBitSizeFunction
   ELSIF ProcSym = Convert
   THEN
      BuildConvertFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Odd
   THEN
      BuildOddFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Abs
   THEN
      BuildAbsFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Cap
   THEN
      BuildCapFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Val
   THEN
      BuildValFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Chr
   THEN
      BuildChrFunction (ProcSym, ConstExpr)
   ELSIF IsOrd (ProcSym)
   THEN
      BuildOrdFunction (ProcSym, ConstExpr)
   ELSIF IsInt (ProcSym)
   THEN
      BuildIntFunction (ProcSym, ConstExpr)
   ELSIF IsTrunc (ProcSym)
   THEN
      BuildTruncFunction (ProcSym, ConstExpr)
   ELSIF IsFloat (ProcSym)
   THEN
      BuildFloatFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Min
   THEN
      BuildMinFunction
   ELSIF ProcSym = Max
   THEN
      BuildMaxFunction
   ELSIF ProcSym = AddAdr
   THEN
      BuildAddAdrFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = SubAdr
   THEN
      BuildSubAdrFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = DifAdr
   THEN
      BuildDifAdrFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Cast
   THEN
      BuildCastFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Shift
   THEN
      BuildShiftFunction
   ELSIF ProcSym = Rotate
   THEN
      BuildRotateFunction
   ELSIF ProcSym = MakeAdr
   THEN
      BuildMakeAdrFunction
   ELSIF ProcSym = Re
   THEN
      BuildReFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Im
   THEN
      BuildImFunction (ProcSym, ConstExpr)
   ELSIF ProcSym = Cmplx
   THEN
      BuildCmplxFunction (ProcSym, ConstExpr)
   ELSE
      InternalError  ('pseudo function not implemented yet')
   END
END BuildPseudoFunctionCall ;


(*
   BuildAddAdrFunction - builds the pseudo procedure call ADDADR.

                         PROCEDURE ADDADR (addr: ADDRESS; offset: CARDINAL): ADDRESS ;

                         Which returns address given by (addr + offset),
                         [ the standard says that it _may_
                           "raise an exception if this address is not valid."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildAddAdrFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   functok,
   vartok,
   optok      : CARDINAL ;
   opa,
   ReturnVar,
   NoOfParam,
   OperandSym,
   VarSym     : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam=2
   THEN
      VarSym := OperandT (2) ;
      vartok := OperandTok (2) ;
      OperandSym := OperandT (1) ;
      optok := OperandTok (1) ;
      combinedtok := MakeVirtual2Tok (functok, optok) ;
      PopN (NoOfParam + 1) ;
      IF ConstExprError (ProcSym, VarSym, vartok, ConstExpr) OR
         ConstExprError (ProcSym, OperandSym, optok, ConstExpr)
      THEN
         (* Fake return result.  *)
         PushTFtok (MakeConstLit (combinedtok, MakeKey('0'), Address),
                    Address, combinedtok)
      ELSIF IsVar (VarSym)
      THEN
         IF IsReallyPointer (VarSym) OR (GetSType (VarSym) = Address)
         THEN
            ReturnVar := MakeTemporary (combinedtok, RightValue) ;
            PutVar (ReturnVar, Address) ;
            opa := ConvertToAddress (optok, DereferenceLValue (optok, OperandSym)) ;
            GenQuadOtok (combinedtok, AddOp, ReturnVar, VarSym, opa, TRUE,
                         combinedtok, combinedtok, combinedtok) ;
            PushTFtok (ReturnVar, Address, combinedtok)
         ELSE
            MetaErrorT1 (functok,
                         'the first parameter to ADDADR {%1Ea} must be a variable of type ADDRESS or a {%EkPOINTER}, rather than a {%1Etsd}',
                         VarSym) ;
            PushTFtok (MakeConstLit (combinedtok, MakeKey('0'), Address), Address, combinedtok)
         END
      ELSE
         MetaErrorT0 (functok, '{%E}SYSTEM procedure ADDADR expects a variable of type ADDRESS or POINTER as its first parameter') ;
         PushTFtok (MakeConstLit (combinedtok, MakeKey('0'), Address), Address, combinedtok)
      END
   ELSE
      MetaErrorT0 (functok,
                   '{%E}SYSTEM procedure {%EkADDADR} expects 2 parameters') ;
      PopN (NoOfParam+1) ;
      PushTFtok (MakeConstLit (functok, MakeKey('0'), Address), Address, functok)
   END
END BuildAddAdrFunction ;


(*
   BuildSubAdrFunction - builds the pseudo procedure call ADDADR.

                         PROCEDURE SUBADR (addr: ADDRESS; offset: CARDINAL): ADDRESS ;

                         Which returns address given by (addr - offset),
                         [ the standard says that it _may_
                           "raise an exception if this address is not valid."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildSubAdrFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   functok,
   combinedtok,
   optok,
   vartok     : CARDINAL ;
   ReturnVar,
   NoOfParam,
   OperandSym,
   opa,
   VarSym     : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam = 2
   THEN
      optok := OperandTok (1) ;
      OperandSym := OperandT (1) ;
      VarSym := OperandT (2) ;
      vartok := OperandTok (2) ;
      combinedtok := MakeVirtualTok (functok, functok, optok) ;
      PopN (NoOfParam + 1) ;
      IF ConstExprError (ProcSym, VarSym, vartok, ConstExpr) OR
         ConstExprError (ProcSym, OperandSym, optok, ConstExpr)
      THEN
         (* Fake return result.  *)
         PushTFtok (MakeConstLit (combinedtok, MakeKey('0'), Address),
                    Address, combinedtok)
      ELSIF IsVar (VarSym)
      THEN
         IF IsReallyPointer (VarSym) OR (GetSType (VarSym) = Address)
         THEN
            ReturnVar := MakeTemporary (combinedtok, RightValue) ;
            PutVar (ReturnVar, Address) ;
            opa := ConvertToAddress (optok, DereferenceLValue (optok, OperandSym)) ;
            GenQuadOtok (combinedtok, SubOp, ReturnVar, VarSym, opa, TRUE,
                         combinedtok, combinedtok, combinedtok) ;
            PushTFtok (ReturnVar, Address, combinedtok)
         ELSE
            MetaErrorT1 (functok,
                         'the first parameter to {%EkSUBADR} {%1Ea} must be a variable of type ADDRESS or a {%EkPOINTER}, rather than a {%1Etsd}',
                         VarSym) ;
            PushTFtok (MakeConstLit (vartok, MakeKey('0'), Address), Address, vartok)
         END
      ELSE
         combinedtok := MakeVirtualTok (functok, functok, optok) ;
         MetaErrorT0 (combinedtok,
                      '{%E}SYSTEM procedure {%EkSUBADR} expects a variable of type ADDRESS or POINTER as its first parameter') ;
         PushTFtok (MakeConstLit (combinedtok, MakeKey ('0'), Address), Address, combinedtok)
      END
   ELSE
      MetaErrorT0 (functok,
                   '{%E}SYSTEM procedure {%EkSUBADR} expects 2 parameters') ;
      PopN (NoOfParam+1) ;
      PushTFtok (MakeConstLit (functok, MakeKey('0'), Address), Address, functok)
   END
END BuildSubAdrFunction ;


(*
   BuildDifAdrFunction - builds the pseudo procedure call DIFADR.

                         PROCEDURE DIFADR (addr1, addr2: ADDRESS): INTEGER ;

                         Which returns address given by (addr1 - addr2),
                         [ the standard says that it _may_
                           "raise an exception if this address is invalid or
                            address space is non-contiguous."
                           currently we do not generate any exception code ]

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildDifAdrFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   functok,
   optok,
   vartok,
   combinedtok: CARDINAL ;
   TempVar,
   NoOfParam,
   OperandSym,
   opa,
   VarSym     : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam >= 1
   THEN
      OperandSym := OperandT (1) ;
      optok := OperandTok (1)
   ELSE
      optok := functok
   END ;
   IF NoOfParam = 2
   THEN
      VarSym := OperandT (2) ;
      vartok := OperandTok (2) ;
      combinedtok := MakeVirtualTok (functok, functok, optok) ;
      PopN (NoOfParam + 1) ;
      IF ConstExprError (ProcSym, VarSym, vartok, ConstExpr) OR
         ConstExprError (ProcSym, OperandSym, optok, ConstExpr)
      THEN
         (* Fake return result.  *)
         PushTFtok (MakeConstLit (combinedtok, MakeKey('0'), Integer),
                    Integer, combinedtok)
      ELSIF IsVar (VarSym)
      THEN
         IF IsReallyPointer (VarSym) OR (GetSType (VarSym) = Address)
         THEN
            IF IsReallyPointer (OperandSym) OR (GetSType (OperandSym) = Address)
            THEN
               TempVar := MakeTemporary (vartok, RightValue) ;
               PutVar (TempVar, Address) ;
               opa := ConvertToAddress (optok, DereferenceLValue (optok, OperandSym)) ;
               GenQuadOtok (combinedtok, SubOp, TempVar, VarSym, opa, TRUE,
                            combinedtok, combinedtok, combinedtok) ;
               (*
                  Build macro: CONVERT( INTEGER, TempVar )
               *)
               PushTFtok (Convert, NulSym, functok) ;
               PushTtok (Integer, functok) ;
               PushTtok (TempVar, vartok) ;
               PushT (2) ;          (* Two parameters *)
               BuildConvertFunction (Convert, ConstExpr)
            ELSE
               MetaError1 ('the second parameter to {%EkDIFADR} {%1Ea} must be a variable of type ADDRESS or a {%EkPOINTER}, rather than a {%1Etsd}',
                           OperandSym) ;
               PushTFtok (MakeConstLit (combinedtok, MakeKey ('0'), Integer), Integer, combinedtok)
            END
         ELSE
            MetaErrorT1 (vartok,
                         'the first parameter to {%EkDIFADR} {%1Ea} must be a variable of type ADDRESS or a {%EkPOINTER}, rather than a {%1Etsd}',
                         VarSym) ;
            PushTFtok (MakeConstLit (combinedtok, MakeKey ('0'), Integer), Integer, combinedtok)
         END
      ELSE
         MetaError0 ('{%E}SYSTEM procedure {%EkDIFADR} expects a variable of type ADDRESS or POINTER as its first parameter') ;
         PushTFtok (MakeConstLit (combinedtok, MakeKey('0'), Integer), Integer, combinedtok)
      END
   ELSE
      combinedtok := MakeVirtual2Tok (functok, optok) ;
      MetaErrorT0 (combinedtok, '{%E}SYSTEM procedure {%EkDIFADR} expects 2 parameters') ;
      PopN (NoOfParam+1) ;
      PushTFtok (MakeConstLit (combinedtok, MakeKey('0'), Integer), Integer, combinedtok)
   END
END BuildDifAdrFunction ;


(*
   BuildHighFunction - checks the stack in preparation for generating
                       quadruples which perform HIGH.
                       This procedure does not alter the stack but
                       determines whether, a, in HIGH(a) is an ArraySym
                       or UnboundedSym.
                       Both cases are different and appropriate quadruple
                       generating routines are called.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|

*)

PROCEDURE BuildHighFunction ;
VAR
   functok,
   combinedtok,
   paramtok   : CARDINAL ;
   ProcSym,
   Type,
   NoOfParam,
   Param      : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   ProcSym := OperandT (NoOfParam+1) ;
   functok := OperandTok (NoOfParam + 1) ;
   BuildSizeCheckEnd (ProcSym) ;   (* quadruple generation now on *)
   IF NoOfParam = 1
   THEN
      Param := OperandT (1) ;
      paramtok := OperandTok (1) ;
      combinedtok := MakeVirtualTok (paramtok, functok, paramtok) ;
      Type := GetDType (Param) ;
      (* Restore stack to original form *)
      PushT (NoOfParam) ;
      IF (NOT IsVar(Param)) AND (NOT IsConstString(Param)) AND (NOT IsConst(Param))
      THEN
         (* we cannot test for IsConst(Param) AND (GetSType(Param)=Char)  as the type might not be assigned yet *)
         MetaError1 ('base procedure {%EkHIGH} expects a variable or string constant as its parameter {%1d:rather than {%1d}} {%1asa}', Param)
      ELSIF IsUnbounded(Type)
      THEN
         BuildHighFromUnbounded (combinedtok)
      ELSE
         BuildConstHighFromSym (combinedtok)
      END
   ELSE
      MetaError0 ('base procedure {%EkHIGH} requires one parameter') ;
      PopN (2) ;
      PushTFtok (MakeConstLit (functok, MakeKey ('0'), Cardinal), Cardinal, functok)
   END
END BuildHighFunction ;


(*
   BuildConstHighFromSym - builds the pseudo function HIGH from an Sym.
                           Sym is a constant or an array which has constant bounds
                           and therefore it can be calculated at compile time.

                           The Stack:


                           Entry                      Exit

                   Ptr ->
                           +----------------+
                           | NoOfParam      |
                           |----------------|
                           | Param 1        |
                           |----------------|
                           | Param 2        |
                           |----------------|
                           .                .
                           .                .
                           .                .
                           |----------------|
                           | Param #        |                        <- Ptr
                           |----------------|         +------------+
                           | ProcSym | Type |         | ReturnVar  |
                           |----------------|         |------------|
*)

PROCEDURE BuildConstHighFromSym (tok: CARDINAL) ;
VAR
   NoOfParam,
   ReturnVar: CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   ReturnVar := MakeTemporary (tok, ImmediateValue) ;
   PutConst (ReturnVar, Cardinal) ;
   GenHigh (tok, ReturnVar, 1, OperandT (1)) ;
   PopN (NoOfParam+1) ;
   PushTtok (ReturnVar, tok)
END BuildConstHighFromSym ;


(*
   BuildHighFromUnbounded - builds the pseudo function HIGH from an
                            UnboundedSym.

                            The Stack:


                            Entry                      Exit

                     Ptr ->
                            +----------------+
                            | NoOfParam      |
                            |----------------|
                            | Param #        |                        <- Ptr
                            |----------------|         +------------+
                            | ProcSym | Type |         | ReturnVar  |
                            |----------------|         |------------|

*)

PROCEDURE BuildHighFromUnbounded (tok: CARDINAL) ;
VAR
   Dim,
   NoOfParam,
   ReturnVar: CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   Assert (NoOfParam=1) ;
   ReturnVar := MakeTemporary (tok, RightValue) ;
   PutVar (ReturnVar, Cardinal) ;
   Dim := OperandD (1) ;
   INC (Dim) ;
   IF Dim > 1
   THEN
      GenHigh (tok, ReturnVar, Dim, OperandA(1))
   ELSE
      GenHigh (tok, ReturnVar, Dim, OperandT(1))
   END ;
   PopN (2) ;
   PushTFtok (ReturnVar, GetSType(ReturnVar), tok)
END BuildHighFromUnbounded ;


(*
   GetQualidentImport - returns the symbol as if it were qualified from, module.n.
                        This is used to reference runtime support procedures and an
                        error is generated if the symbol cannot be obtained.
*)

PROCEDURE GetQualidentImport (tokno: CARDINAL;
                              n: Name; module: Name) : CARDINAL ;
VAR
   ModSym: CARDINAL ;
BEGIN
   ModSym := MakeDefinitionSource (tokno, module) ;
   IF ModSym=NulSym
   THEN
      MetaErrorNT2 (tokno,
                    'module %a cannot be found and is needed to import %a', module, n) ;
      FlushErrors ;
      RETURN NulSym
   END ;
   Assert(IsDefImp(ModSym)) ;
   IF (GetExported (tokno, ModSym, n)=NulSym) OR IsUnknown (GetExported (tokno, ModSym, n))
   THEN
      MetaErrorN2 ('module %a does not export procedure %a which is a necessary component of the runtime system, hint check the path and library/language variant',
                   module, n) ;
      FlushErrors ;
      RETURN NulSym
   END ;
   RETURN GetExported (tokno, MakeDefinitionSource (tokno, module), n)
END GetQualidentImport ;


(*
   ConstExprError - return TRUE if a constant expression is being built and Var is a variable.
*)

PROCEDURE ConstExprError (Func, Var: CARDINAL; optok: CARDINAL; ConstExpr: BOOLEAN) : BOOLEAN ;
BEGIN
   IF ConstExpr AND IsVar (Var)
   THEN
      MetaErrorT2 (optok,
                   'the procedure function {%1Ea} is being called from within a constant expression and therefore the parameter {%2a} must be a constant, seen a {%2da}',
                   Func, Var) ;
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END ConstExprError ;


(*
   DeferMakeLengthConst - creates a constant which contains the length of string, sym.
*)

PROCEDURE DeferMakeLengthConst (tok: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   const: CARDINAL ;
BEGIN
   const := MakeTemporary (tok, ImmediateValue) ;
   PutVar (const, ZType) ;
   GenQuadO (tok, StringLengthOp, const, 0, sym, FALSE) ;
   RETURN const
END DeferMakeLengthConst ;


(*
   BuildLengthFunction - builds the inline standard function LENGTH.

                         The Stack:


                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|

*)

PROCEDURE BuildLengthFunction  (Function: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   paramtok,
   functok    : CARDINAL ;
   ProcSym,
   Type,
   NoOfParam,
   Param,
   ReturnVar  : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   Param := OperandT (1) ;
   paramtok := OperandTok (1) ;
   functok := OperandTok (NoOfParam + 1) ;
   (* Restore stack to origional form.  *)
   PushT (NoOfParam) ;
   Type := GetSType (Param) ;  (* Get the type from the symbol, not the stack.  *)
   IF NoOfParam # 1
   THEN
      MetaErrorT1 (functok, 'base procedure {%1EkLENGTH} expects 1 parameter, seen {%1n} parameters', NoOfParam)
   END ;
   IF NoOfParam >= 1
   THEN
      combinedtok := MakeVirtual2Tok (functok, paramtok) ;
      IF IsConst (Param) AND (GetSType (Param) = Char)
      THEN
         PopT (NoOfParam) ;
         PopN (NoOfParam + 1) ;
         ReturnVar := MakeConstLit (combinedtok, MakeKey ('1'), Cardinal) ;
         PushTtok (ReturnVar, combinedtok)
      ELSIF IsConstString (Param)
      THEN
         PopT (NoOfParam) ;
         ReturnVar := DeferMakeLengthConst (combinedtok, OperandT (1)) ;
         PopN (NoOfParam + 1) ;
         PushTtok (ReturnVar, combinedtok)
      ELSE
         ProcSym := GetQualidentImport (functok, MakeKey ('Length'), MakeKey ('M2RTS')) ;
         IF (ProcSym # NulSym) AND IsProcedure (ProcSym)
         THEN
            PopT (NoOfParam) ;
            IF IsConst (Param)
            THEN
               (* This can be folded in M2GenGCC.  *)
               ReturnVar := MakeTemporary (combinedtok, ImmediateValue) ;
               PutVar (ReturnVar, Cardinal) ;
               GenQuad (StandardFunctionOp, ReturnVar, ProcSym, Param) ;
               PopN (NoOfParam + 1) ;
               PushTtok (ReturnVar, combinedtok)
            ELSIF ConstExprError (Function, Param, paramtok, ConstExpr)
            THEN
               (* Fake a result as we have detected and reported an error.  *)
               PopN (NoOfParam + 1) ;
               ReturnVar := MakeConstLit (combinedtok, MakeKey ('1'), Cardinal) ;
               PushTtok (ReturnVar, combinedtok)
            ELSE
               (* We must resolve this at runtime or in the GCC optimizer.  *)
               PopTF (Param, Type);
	       PopN (NoOfParam) ;
	       PushTtok (ProcSym, functok) ;
               PushTFtok (Param, Type, paramtok) ;
	       PushT (NoOfParam) ;
	       BuildRealFunctionCall (functok, FALSE)
            END
         ELSE
            PopT (NoOfParam) ;
            PopN (NoOfParam + 1) ;
            PushTtok (MakeConstLit (combinedtok, MakeKey ('0'), Cardinal), combinedtok) ;
            MetaErrorT0 (functok, 'no procedure Length found for substitution to the standard function {%1EkLENGTH} which is required to calculate non constant string lengths')
         END
      END
   ELSE
      (* NoOfParam is _very_ wrong, we flush all outstanding errors *)
      FlushErrors
   END
END BuildLengthFunction ;


(*
   BuildOddFunction - builds the pseudo procedure call ODD.
                      This procedure is actually a "macro" for
                      ORD(x) --> VAL(BOOLEAN, x MOD 2)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildOddFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   optok,
   functok    : CARDINAL ;
   NoOfParam,
   Res, Var   : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam=1
   THEN
      Var := OperandT (1) ;
      optok := OperandTok (1) ;
      combinedtok := MakeVirtualTok (functok, functok, optok) ;
      IF ConstExprError (ProcSym, Var, optok, ConstExpr)
      THEN
         (* Nothing to do.  *)
         PushTtok (False, combinedtok)
      ELSIF IsVar(Var) OR IsConst(Var)
      THEN
         PopN (NoOfParam + 1) ;
         (*
            Build macro: VAL(BOOLEAN, (x MOD 2))
         *)

         (* compute (x MOD 2) *)
         PushTFtok (Var, GetSType (Var), optok) ;
         PushT (ModTok) ;
         PushTFtok (MakeConstLit (optok, MakeKey ('2'), ZType), ZType, optok) ;
         BuildBinaryOp ;
         PopT (Res) ;

         (* compute IF ...=0 *)
         PushTtok (Res, optok) ;
         PushT (EqualTok) ;
         PushTFtok (MakeConstLit (optok, MakeKey ('0'), ZType), ZType, optok) ;
         BuildRelOp (combinedtok) ;
         BuildThenIf ;

         Res := MakeTemporary (combinedtok, RightValue) ;
         PutVar (Res, Boolean) ;

         PushTtok (Res, combinedtok) ;
         PushTtok (False, combinedtok) ;
         BuildAssignment (combinedtok) ;
         BuildElse ;
         PushTtok (Res, combinedtok) ;
         PushTtok (True, combinedtok) ;
         BuildAssignment (combinedtok) ;
         BuildEndIf ;

         PushTtok (Res, combinedtok)
      ELSE
         MetaErrorT1 (optok,
                      'the parameter to {%1EkODD} must be a variable or constant, seen {%1ad}',
                      Var) ;
         PushTtok (False, combinedtok)
      END
   ELSE
      MetaErrorT1 (functok,
                   'the pseudo procedure {%E1kODD} only has one parameter, seen {%1n} parameters',
                   NoOfParam) ;
      PushTtok (False, functok)
   END
END BuildOddFunction ;


(*
   BuildAbsFunction - builds a call to the standard function ABS.

                      We cannot implement it as a macro or inline an
                      IF THEN statement as the IF THEN ELSE requires
                      we write the value to the same variable (or constant)
                      twice. The macro implementation will fail as
                      the compiler maybe building a function
                      call and expecting a ReturnVar on the stack.
                      The only method to implement this is to pass it to the
                      gcc backend.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildAbsFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   vartok,
   functok,
   combinedtok: CARDINAL ;
   NoOfParam,
   Res, Var : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      Var := OperandT (1) ;
      vartok := OperandTok (1) ;
      PopN (NoOfParam + 1) ;
      combinedtok := MakeVirtualTok (functok, functok, vartok) ;
      IF ConstExprError (ProcSym, Var, vartok, ConstExpr)
      THEN
         (* Create fake result.  *)
         Res := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (Res, GetSType (Var)) ;
         PushTFtok (Res, GetSType (Var), combinedtok)
      ELSIF IsVar(Var) OR IsConst(Var)
      THEN
         Res := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (Res, GetSType (Var)) ;

         GenQuadO (combinedtok, StandardFunctionOp, Res, ProcSym, Var, FALSE) ;
         PushTFtok (Res, GetSType (Var), combinedtok)
      ELSE
         MetaErrorT1 (vartok,
                      'the parameter to {%AkABS} must be a variable or constant, seen {%1ad}',
                      Var)
      END
   ELSE
      MetaErrorT1 (functok,
                   'the pseudo procedure {%AkABS} only has one parameter, seen {%1n} parameters',
                   NoOfParam)
   END
END BuildAbsFunction ;


(*
   BuildCapFunction - builds the pseudo procedure call CAP.
                      We generate a the following quad:


                      StandardFunctionOp  ReturnVal  Cap  Param1

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam = 1  |
                      |----------------|
                      | Param 1        |
                      |----------------|         +-------------+
                      | ProcSym | Type |         | ReturnVal   |
                      |----------------|         |-------------|
*)

PROCEDURE BuildCapFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   optok,
   functok,
   combinedtok: CARDINAL ;
   NoOfParam,
   Res, Var : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      Var := OperandT (1) ;
      optok := OperandTok (1) ;
      PopN (NoOfParam + 1) ;
      IF ConstExprError (ProcSym, Var, optok, ConstExpr)
      THEN
         (* Create fake result.  *)
         combinedtok := MakeVirtual2Tok (functok, optok) ;
         Res := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (Res, Char) ;
         PushTFtok (Res, Char, combinedtok)
      ELSIF IsVar (Var) OR IsConst (Var)
      THEN
         combinedtok := MakeVirtual2Tok (functok, optok) ;
         Res := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (Res, Char) ;
         GenQuadO (combinedtok, StandardFunctionOp, Res, ProcSym, Var, FALSE) ;
         PushTFtok (Res, Char, combinedtok)
      ELSE
         MetaErrorT1 (optok,
                      'the parameter to {%AkCAP} must be a variable or constant, seen {%1ad}',
                      Var)
      END
   ELSE
      MetaErrorT1 (functok,
                   'the pseudo procedure {%AkCAP} only has one parameter, seen {%1n} parameters',
                   NoOfParam)
   END
END BuildCapFunction ;


(*
   BuildChrFunction - builds the pseudo procedure call CHR.
                      This procedure is actually a "macro" for
                      CHR(x) --> CONVERT(CHAR, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildChrFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   functok,
   combinedtok,
   optok      : CARDINAL ;
   ReturnVar,
   NoOfParam,
   Var        : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      Var := OperandT (1) ;
      optok := OperandTok (1) ;
      PopN (NoOfParam + 1) ;
      IF ConstExprError (ProcSym, Var, optok, ConstExpr)
      THEN
         (* Generate fake result.  *)
         combinedtok := MakeVirtual2Tok (functok, optok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (ReturnVar, Char) ;
         PushTFtok (ReturnVar, Char, combinedtok)
      ELSIF IsVar (Var) OR IsConst (Var)
      THEN
         (*
            Build macro: CONVERT( CHAR, Var )
         *)
         PushTFtok (Convert, NulSym, functok) ;
         PushTtok (Char, functok) ;
         PushTtok (Var, optok) ;
         PushT (2) ;          (* Two parameters *)
         BuildConvertFunction (Convert, ConstExpr)
      ELSE
         MetaErrorT1 (optok,
                      'the parameter to {%AkCHR} must be a variable or constant, seen {%1ad}',
                      Var)
      END
   ELSE
      MetaErrorT1 (functok,
                   'the pseudo procedure {%AkCHR} only has one parameter, seen {%1n} parameters',
                   NoOfParam)
   END
END BuildChrFunction ;


(*
   BuildOrdFunction - builds the pseudo procedure call ORD.
                      This procedure is actually a "macro" for
                      ORD(x) --> CONVERT(GetSType(sym), x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildOrdFunction (Sym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   functok,
   optok      : CARDINAL ;
   ReturnVar,
   NoOfParam,
   Type, Var  : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      Var := OperandT (1) ;
      optok := OperandTok (1) ;
      PopN (NoOfParam + 1) ;
      IF ConstExprError (Sym, Var, optok, ConstExpr)
      THEN
         (* Generate fake result.  *)
         combinedtok := MakeVirtual2Tok (functok, optok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (ReturnVar, Cardinal) ;
         PushTFtok (ReturnVar, Cardinal, combinedtok)
      ELSIF IsVar (Var) OR IsConst (Var)
      THEN
         Type := GetSType (Sym) ;
         (*
            Build macro: CONVERT( CARDINAL, Var )
         *)
         PushTFtok (Convert, NulSym, functok) ;
         PushTtok (Type, optok) ;
         PushTtok (Var, optok) ;
         PushT (2) ;          (* Two parameters *)
         BuildConvertFunction (Convert, ConstExpr)
      ELSE
         MetaErrorT2 (optok,
                      'the parameter to {%1Aa} must be a variable or constant, seen {%2ad}',
                      Sym, Var)
      END
   ELSE
      MetaErrorT2 (functok,
                   'the pseudo procedure {%1Aa} only has one parameter, seen {%2n} parameters',
                   Sym, NoOfParam)
   END
END BuildOrdFunction ;


(*
   BuildIntFunction - builds the pseudo procedure call INT.
                      This procedure is actually a "macro" for
                      INT(x) --> CONVERT(INTEGER, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildIntFunction (Sym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   functok,
   optok      : CARDINAL ;
   ReturnVar,
   NoOfParam,
   Type, Var  : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      Var := OperandT (1) ;
      optok := OperandTok (1) ;
      PopN (NoOfParam + 1) ;
      IF ConstExprError (Sym, Var, optok, ConstExpr)
      THEN
         (* Generate fake result.  *)
         combinedtok := MakeVirtual2Tok (functok, optok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (ReturnVar, Integer) ;
         PushTFtok (ReturnVar, Integer, combinedtok)
      ELSIF IsVar (Var) OR IsConst (Var)
      THEN
         Type := GetSType (Sym) ;  (* return type of function *)
         (* Build macro: CONVERT( CARDINAL, Var ).  *)
         PushTFtok (Convert, NulSym, functok) ;
         PushTtok (Type, functok) ;
         PushTtok (Var, optok) ;
         PushT (2) ;          (* Two parameters *)
         BuildConvertFunction (Convert, ConstExpr)
      ELSE
         combinedtok := MakeVirtualTok (functok, optok, optok) ;
         MetaErrorT2 (optok,
                      'the parameter to {%1Ea} must be a variable or constant, seen {%2ad}',
                      Sym, Var) ;
         PushTtok (combinedtok, MakeConstLit (combinedtok, MakeKey ('0'), ZType))
      END
   ELSE
      MetaErrorT2 (functok,
                   'the pseudo procedure {%1Ea} only has one parameter, seen {%2n} parameters',
                   Sym, NoOfParam) ;
      PushTtok (functok, MakeConstLit (functok, MakeKey ('0'), ZType))
   END
END BuildIntFunction ;


(*
   BuildMakeAdrFunction - builds the pseudo procedure call MAKEADR.

                          The Stack:


                          Entry                      Exit

                   Ptr ->
                          +----------------+
                          | NoOfParam      |
                          |----------------|
                          | Param 1        |
                          |----------------|
                          | Param 2        |
                          |----------------|
                          .                .
                          .                .
                          .                .
                          |----------------|
                          | Param #        |
                          |----------------|
                          | ProcSym | Type |         Empty
                          |----------------|
*)

PROCEDURE BuildMakeAdrFunction ;
VAR
   functok,
   starttok,
   endtok,
   resulttok     : CARDINAL ;
   AreConst      : BOOLEAN ;
   i, pi,
   NoOfParameters: CARDINAL ;
   ReturnVar     : CARDINAL ;
BEGIN
   PopT (NoOfParameters) ;
   functok := OperandTok (NoOfParameters + 1) ;
   IF NoOfParameters>0
   THEN
      starttok := OperandTok (NoOfParameters + 1) ;  (* ADR token.  *)
      endtok := OperandTok (1) ;  (* last parameter.  *)
      GenQuad (ParamOp, 0, MakeAdr, MakeAdr) ;
      i := NoOfParameters ;
      (* stack index referencing stacked parameter, i *)
      pi := 1 ;
      WHILE i > 0 DO
         GenQuadO (OperandTok (pi), ParamOp, i, MakeAdr, OperandT (pi), TRUE) ;
         DEC (i) ;
         INC (pi)
      END ;
      AreConst := TRUE ;
      i := 1 ;
      WHILE i <= NoOfParameters DO
         IF IsVar (OperandT (i))
         THEN
            AreConst := FALSE ;
         ELSIF NOT IsConst (OperandT (i))
         THEN
            MetaError1 ('problem in the {%1EN} argument for {%kMAKEADR}, all arguments to {%kMAKEADR} must be either variables or constants', i)
         END ;
         INC (i)
      END ;
      (* ReturnVar - will have the type of the procedure *)
      resulttok := MakeVirtualTok (starttok, starttok, endtok) ;
      ReturnVar := MakeTemporary (resulttok, AreConstant(AreConst)) ;
      PutVar (ReturnVar, GetSType(MakeAdr)) ;
      GenQuadO (resulttok, FunctValueOp, ReturnVar, NulSym, MakeAdr, TRUE) ;
      PopN (NoOfParameters+1) ;
      PushTFtok (ReturnVar, GetSType (MakeAdr), resulttok)
   ELSE
      MetaError1 ('the pseudo procedure {%EkMAKEADR} requires at least one parameter, seen {%1n}', NoOfParameters) ;
      PopN (1) ;
      PushTFtok (Nil, GetSType (MakeAdr), functok)
   END
END BuildMakeAdrFunction ;


(*
   BuildShiftFunction - builds the pseudo procedure call SHIFT.

                        PROCEDURE SHIFT (val: <any type>;
                                         num: INTEGER): <any type> ;

                       "Returns a bit sequence obtained from val by
                        shifting up or down (left or right) by the
                        absolute value of num, introducing
                        zeros as necessary.  The direction is down if
                        the sign of num is negative, otherwise the
                        direction is up."

                        The Stack:

                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |                        <- Ptr
                        |----------------|         +------------+
                        | ProcSym | Type |         | ReturnVar  |
                        |----------------|         |------------|
*)

PROCEDURE BuildShiftFunction ;
VAR
   combinedtok,
   paramtok,
   functok,
   vartok,
   exptok     : CARDINAL ;
   r,
   procSym,
   returnVar,
   NoOfParam,
   derefExp,
   Exp,
   varSet     : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   paramtok := OperandTok (1) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam=2
   THEN
      PopTrwtok (Exp, r, exptok) ;
      MarkAsRead (r) ;
      PopTtok (varSet, vartok) ;
      PopT (procSym) ;
      combinedtok := MakeVirtualTok (functok, functok, exptok) ;
      IF (GetSType (varSet) # NulSym) AND IsSet (GetDType (varSet))
      THEN
         derefExp := DereferenceLValue (exptok, Exp) ;
         BuildRange (InitShiftCheck (varSet, derefExp)) ;
         returnVar := MakeTemporary (combinedtok, RightValue) ;
         PutVar (returnVar, GetSType (varSet)) ;
         GenQuadO (combinedtok, LogicalShiftOp, returnVar, varSet, derefExp, TRUE) ;
         PushTFtok (returnVar, GetSType (varSet), combinedtok)
      ELSE
         MetaErrorT1 (vartok,
                      'SYSTEM procedure {%1EkSHIFT} expects a constant or variable which has a type of SET as its first parameter, seen {%1ad}',
                      varSet) ;
         PushTFtok (MakeConstLit (combinedtok, MakeKey ('0'), Cardinal), Cardinal, combinedtok)
      END
   ELSE
      combinedtok := MakeVirtualTok (functok, functok, paramtok) ;
      MetaErrorT1 (functok,
                   'the pseudo procedure {%kSHIFT} requires at least two parameters, seen {%1En}',
                   NoOfParam) ;
      PopN (NoOfParam + 1) ;
      PushTFtok (MakeConstLit (combinedtok, MakeKey ('0'), Cardinal), Cardinal, combinedtok)
   END
END BuildShiftFunction ;


(*
   BuildRotateFunction - builds the pseudo procedure call ROTATE.

                         PROCEDURE ROTATE (val: <any type>;
                                           num: INTEGER): <any type> ;

                        "Returns a bit sequence obtained from val
                         by rotating up or down (left or right) by
                         the absolute value of num.  The direction is
                         down if the sign of num is negative, otherwise
                         the direction is up."

                         The Stack:

                         Entry                      Exit

                  Ptr ->
                         +----------------+
                         | NoOfParam      |
                         |----------------|
                         | Param 1        |
                         |----------------|
                         | Param 2        |                        <- Ptr
                         |----------------|         +------------+
                         | ProcSym | Type |         | ReturnVar  |
                         |----------------|         |------------|
*)

PROCEDURE BuildRotateFunction ;
VAR
   combinedtok,
   functok,
   vartok,
   exptok     : CARDINAL ;
   r,
   procSym,
   returnVar,
   NoOfParam,
   derefExp,
   Exp,
   varSet     : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 2
   THEN
      PopTrwtok (Exp, r, exptok) ;
      MarkAsRead (r) ;
      PopTtok (varSet, vartok) ;
      PopT (procSym) ;
      IF (GetSType (varSet) # NulSym) AND IsSet (GetDType (varSet))
      THEN
         combinedtok := MakeVirtualTok (functok, functok, exptok) ;
         derefExp := DereferenceLValue (exptok, Exp) ;
         BuildRange (InitRotateCheck (varSet, derefExp)) ;
         returnVar := MakeTemporary (combinedtok, RightValue) ;
         PutVar (returnVar, GetSType (varSet)) ;
         GenQuadO (combinedtok, LogicalRotateOp, returnVar, varSet, derefExp, TRUE) ;
         PushTFtok (returnVar, GetSType (varSet), combinedtok)
      ELSE
         MetaErrorT1 (vartok,
                      'SYSTEM procedure {%EkROTATE} expects a constant or variable which has a type of SET as its first parameter, seen {%1ad}',
                      varSet) ;
         PushTFtok (MakeConstLit (functok, MakeKey('0'), Cardinal), Cardinal, functok)
      END
   ELSE
      MetaErrorT1 (functok,
                   'SYSTEM procedure {%EkROTATE} expects 2 parameters and was given {%1n} parameters',
                   NoOfParam) ;
      PopN (NoOfParam + 1) ;
      PushTFtok (MakeConstLit (functok, MakeKey ('0'), Cardinal), Cardinal, functok)
   END
END BuildRotateFunction ;


(*
   BuildValFunction - builds the pseudo procedure call VAL.
                      This procedure is actually a "macro" for
                      VAL(Type, x) --> CONVERT(Type, x)
                      However we cannot push tokens back onto the input stack
                      because the compiler is currently building a function
                      call and expecting a ReturnVar on the stack.
                      Hence we manipulate the stack and call
                      BuildConvertFunction.

                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildValFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   functok    : CARDINAL ;
   ReturnVar,
   NoOfParam,
   Exp, Type  : CARDINAL ;
   tok, r,
   typetok,
   exptok     : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 2
   THEN
      PopTrwtok (Exp, r, exptok) ;
      MarkAsRead (r) ;
      PopTtok (Type, typetok) ;
      PopTtok (ProcSym, tok) ;
      IF IsUnknown (Type)
      THEN
         (* not sensible to try and recover when we dont know the return type.  *)
         MetaErrorT1 (typetok,
                      'undeclared type found in builtin procedure function {%AkVAL} {%1ad}',
                      Type)
         (* non recoverable error.  *)
      ELSIF ConstExprError (ProcSym, Exp, exptok, ConstExpr)
      THEN
         (* Generate fake result.  *)
         combinedtok := MakeVirtualTok (functok, functok, exptok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Exp))) ;
         PutVar (ReturnVar, Type) ;
         PushTFtok (ReturnVar, Type, combinedtok)
      ELSIF (IsSet (Type) OR IsEnumeration (Type) OR IsSubrange (Type) OR
             IsType (Type) OR IsPointer (Type) OR IsProcType (Type)) AND
             (IsVar (Exp) OR IsConst (Exp) OR IsProcedure (Exp))
      THEN
         (*
            Build macro: CONVERT( Type, Var )
         *)
         PushTFtok (Convert, NulSym, tok) ;
         PushTtok (Type, typetok) ;
         PushTtok (Exp, exptok) ;
         PushT (2) ;          (* Two parameters *)
         BuildConvertFunction (Convert, ConstExpr)
      ELSE
         (* not sensible to try and recover when we dont know the return type.  *)
         MetaErrorT0 (functok,
                      'the builtin procedure {%AkVAL} has the following formal parameter declaration {%kVAL} (type, expression)')
         (* non recoverable error.  *)
      END
   ELSE
      (* not sensible to try and recover when we dont know the return type.  *)
      MetaErrorT1 (functok,
                   'the builtin procedure {%AkVAL} expects 2 parameters, a type and an expression, but was given {%1n} parameters', NoOfParam)
      (* non recoverable error.  *)
   END
END BuildValFunction ;


(*
   BuildCastFunction - builds the pseudo procedure call CAST.
                       This procedure is actually a "macro" for
                       CAST(Type, x) --> Type(x)
                       However we cannot push tokens back onto the input stack
                       because the compiler is currently building a function
                       call and expecting a ReturnVar on the stack.
                       Hence we manipulate the stack and call
                       BuildConvertFunction.

                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |
                       |----------------|
                       | ProcSym | Type |         Empty
                       |----------------|
*)

PROCEDURE BuildCastFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   exptok,
   typetok,
   functok    : CARDINAL ;
   ReturnVar,
   NoOfParam,
   Exp, Type  : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 2
   THEN
      Type := OperandT (2) ;
      typetok := OperandTok (2) ;
      Exp := OperandT (1) ;
      exptok := OperandTok (1) ;
      IF IsUnknown (Type)
      THEN
         (* we cannot recover if we dont have a type.  *)
         MetaErrorT1 (typetok, 'undeclared type {%1Aad} found in {%kCAST}', Type)
         (* non recoverable error.  *)
      ELSIF ConstExprError (ProcSym, Exp, exptok, ConstExpr)
      THEN
         (* Generate fake result.  *)
         combinedtok := MakeVirtualTok (functok, functok, exptok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Exp))) ;
         PutVar (ReturnVar, Type) ;
         PushTFtok (ReturnVar, Type, combinedtok)
      ELSIF IsSet (Type) OR IsEnumeration (Type) OR IsSubrange (Type) OR IsType (Type) OR
            IsPointer (Type) OR IsArray (Type) OR IsProcType (Type)
      THEN
         IF IsConst (Exp)
         THEN
            PopN (NoOfParam+1) ;
            (*
               Build macro: Type( Var )
            *)
            PushTFtok (Type, NulSym, typetok) ;
            PushTtok (Exp, exptok) ;
            PushT (1) ;          (* one parameter *)
            BuildTypeCoercion (ConstExpr)
         ELSIF IsVar (Exp) OR IsProcedure (Exp)
         THEN
            PopN (NoOfParam + 1) ;
            combinedtok := MakeVirtual2Tok (functok, exptok) ;
            ReturnVar := MakeTemporary (combinedtok, RightValue) ;
            PutVar (ReturnVar, Type) ;
            GenQuadO (combinedtok, CastOp, ReturnVar, Type, Exp, FALSE) ;
            PushTFtok (ReturnVar, Type, combinedtok)
         ELSE
            (* not sensible to try and recover when we dont know the return type.  *)
            MetaErrorT0 (functok,
                         'the second parameter to the builtin procedure {%AkCAST} must either be a variable, constant or a procedure.  The formal parameters to cast are {%kCAST} (type, variable or constant or procedure)')
            (* non recoverable error.  *)
         END
      ELSE
         (* not sensible to try and recover when we dont know the return type.  *)
         MetaErrorT0 (functok,
                      'the builtin procedure {%AkCAST} has the following formal parameter declaration {%kCAST} (type, expression)')
         (* non recoverable error.  *)
      END
   ELSE
      (* not sensible to try and recover when we dont know the return type.  *)
      MetaErrorT1 (functok,
                   'the builtin procedure {%AkCAST} `expects 2 parameters, a type and an expression, but was given {%1n} parameters', NoOfParam)
      (* non recoverable error.  *)
   END
END BuildCastFunction ;


(*
   BuildConvertFunction - builds the pseudo function CONVERT.
                          CONVERT( Type, Variable ) ;

                          The Stack:


                          Entry                      Exit

                   Ptr ->
                          +----------------+
                          | NoOfParam      |
                          |----------------|
                          | Param 1        |
                          |----------------|
                          | Param 2        |
                          |----------------|
                          .                .
                          .                .
                          .                .
                          |----------------|
                          | Param #        |                                 <- Ptr
                          |----------------|         +---------------------+
                          | ProcSym | Type |         | ReturnVar | Param1  |
                          |----------------|         |---------------------|

                          Quadruples:

                          ConvertOp  ReturnVar  Param1  Param2

                          Converts variable Param2 into a variable Param1
                          with a type Param1.
*)

PROCEDURE BuildConvertFunction (ProcSym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   functok,
   typetok,
   exptok     : CARDINAL ;
   t, r,
   Exp, Type,
   NoOfParam,
   ReturnVar  : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTok (NoOfParam + 1) ;
   IF NoOfParam = 2
   THEN
      PopTrwtok (Exp, r, exptok) ;
      MarkAsRead (r) ;
      PopTtok (Type, typetok) ;
      PopT (ProcSym) ;
      IF IsUnknown (Type)
      THEN
         (* we cannot recover if we dont have a type.  *)
         MetaErrorT1 (typetok, 'undeclared type {%1Aad} found in {%kCONVERT}', Type)
         (* non recoverable error.  *)
      ELSIF IsUnknown (Exp)
      THEN
         (* we cannot recover if we dont have a type.  *)
         MetaErrorT1 (typetok, 'unknown {%1Ad} {%1ad} found in {%kCONVERT}', Exp)
         (* non recoverable error.  *)
      ELSIF ConstExprError (ProcSym, Exp, exptok, ConstExpr)
      THEN
         (* Generate fake result.  *)
         combinedtok := MakeVirtualTok (functok, functok, exptok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Exp))) ;
         PutVar (ReturnVar, Type) ;
         PushTFtok (ReturnVar, Type, combinedtok)
      ELSIF (IsSet (Type) OR IsEnumeration (Type) OR IsSubrange (Type) OR
             IsType (Type) OR IsPointer (Type) OR IsProcType (Type) OR IsRecord (Type)) AND
            (IsVar (Exp) OR IsConst (Exp) OR IsProcedure (Exp))
      THEN
         (* firstly dereference Var *)
         IF GetMode (Exp) = LeftValue
         THEN
            t := MakeTemporary (exptok, RightValue) ;
            PutVar (t, GetSType (Exp)) ;
            CheckPointerThroughNil (exptok, Exp) ;
            doIndrX (exptok, t, Exp) ;
            Exp := t
         END ;

         combinedtok := MakeVirtualTok (functok, functok, exptok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Exp))) ;
         PutVar (ReturnVar, Type) ;
         GenQuadO (combinedtok, ConvertOp, ReturnVar, Type, Exp, TRUE) ;
         PushTFtok (ReturnVar, Type, combinedtok)
      ELSE
         (* not sensible to try and recover when we dont know the return type.  *)
         MetaErrorT0 (functok,
                      'the builtin procedure {%AkCONVERT} has the following formal parameter declaration {%kCONVERT} (type, expression)')
         (* non recoverable error.  *)
      END
   ELSE
      (* not sensible to try and recover when we dont know the return type.  *)
      MetaErrorT1 (functok,
                   'the builtin procedure {%AkCONVERT} expects 2 parameters, a type and an expression, but was given {%1n} parameters', NoOfParam)
      (* non recoverable error.  *)
   END
END BuildConvertFunction ;


(*
   CheckBaseTypeValue - checks to see whether the value, min, really exists.
*)

PROCEDURE CheckBaseTypeValue (tok: CARDINAL;
                              type: CARDINAL;
                              min: CARDINAL;
                              func: CARDINAL) : CARDINAL ;
BEGIN
   IF (type = Real) OR (type = LongReal) OR (type = ShortReal)
   THEN
      PushValue (min) ;
      IF NOT IsValueAndTreeKnown ()
      THEN
         MetaErrorT2 (tok,
                      '{%1Ead} ({%2ad}) cannot be calculated at compile time for the target architecture', func, type) ;
         RETURN MakeConstLit (tok, MakeKey ('1.0'), RType)
      END
   END ;
   RETURN min
END CheckBaseTypeValue ;


(*
   GetTypeMin - returns the minimium value of type.
*)

PROCEDURE GetTypeMin (tok: CARDINAL; func, type: CARDINAL) : CARDINAL ;
VAR
   min, max: CARDINAL ;
BEGIN
   IF IsSubrange (type)
   THEN
      min := MakeTemporary (tok, ImmediateValue) ;
      PutVar (min, type) ;
      GenQuad (SubrangeLowOp, min, NulSym, type) ;
      RETURN min
   ELSIF IsSet (SkipType (type))
   THEN
      RETURN GetTypeMin (tok, func, GetSType (SkipType (type)))
   ELSIF IsBaseType (type) OR IsEnumeration (type)
   THEN
      GetBaseTypeMinMax (type, min, max) ;
      min := CheckBaseTypeValue (tok, type, min, func) ;
      RETURN min
   ELSIF IsSystemType (type)
   THEN
      GetSystemTypeMinMax (type, min, max) ;
      RETURN min
   ELSIF GetSType (type) = NulSym
   THEN
      MetaErrorT1 (tok,
                   'unable to obtain the {%AkMIN} value for type {%1ad}', type) ;
      (* non recoverable error.  *)
      InternalError ('MetaErrorT1 {%AkMIN} should call abort')
   ELSE
      RETURN GetTypeMin (tok, func, GetSType (type))
   END
END GetTypeMin ;


(*
   GetTypeMax - returns the maximum value of type.
*)

PROCEDURE GetTypeMax (tok: CARDINAL; func, type: CARDINAL) : CARDINAL ;
VAR
   min, max: CARDINAL ;
BEGIN
   IF IsSubrange (type)
   THEN
      max := MakeTemporary (tok, ImmediateValue) ;
      PutVar (max, type) ;
      GenQuad (SubrangeHighOp, max, NulSym, type) ;
      RETURN max
   ELSIF IsSet (SkipType (type))
   THEN
      RETURN GetTypeMax (tok, func, GetSType (SkipType (type)))
   ELSIF IsBaseType (type) OR IsEnumeration (type)
   THEN
      GetBaseTypeMinMax (type, min, max) ;
      min := CheckBaseTypeValue (tok, type, min, func) ;
      RETURN max
   ELSIF IsSystemType (type)
   THEN
      GetSystemTypeMinMax (type, min, max) ;
      RETURN max
   ELSIF GetSType (type) = NulSym
   THEN
      MetaErrorT1 (tok,
                   'unable to obtain the {%AkMAX} value for type {%1ad}', type) ;
      (* non recoverable error.  *)
      InternalError ('MetaErrorT1 {%AkMAX} should call abort')
   ELSE
      RETURN GetTypeMax (tok, func, GetSType (type))
   END
END GetTypeMax ;


(*
   BuildMinFunction - builds the pseudo function call Min.

                      The Stack:

                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam=1    |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildMinFunction ;
VAR
   combinedtok,
   functok,
   vartok     : CARDINAL ;
   func,
   min,
   NoOfParam,
   Var        : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   func := OperandT (NoOfParam + 1) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      Var := OperandT (1) ;
      vartok := OperandTok (1) ;
      PopN (NoOfParam+1) ;    (* destroy arguments to this function *)
      combinedtok := MakeVirtualTok (functok, functok, vartok) ;
      IF IsAModula2Type (Var)
      THEN
         min := GetTypeMin (vartok, func, Var) ;
         PushTFtok (min, GetSType (min), combinedtok)
      ELSIF IsVar (Var)
      THEN
         min := GetTypeMin (vartok, func, GetSType (Var)) ;
         PushTFtok (min, GetSType (Var), combinedtok)
      ELSE
         (* we dont know the type therefore cannot fake a return.  *)
         MetaErrorT1 (vartok,
                      'parameter to {%AkMIN} must be a type or a variable, seen {%1ad}',
                      Var)
         (* non recoverable error.  *)
      END
   ELSE
      (* we dont know the type therefore cannot fake a return.  *)
      MetaErrorT1 (functok,
                   'the pseudo builtin procedure function {%AkMIN} only has one parameter, seen  {%1n}',
                   NoOfParam)
      (* non recoverable error.  *)
   END
END BuildMinFunction ;


(*
   BuildMaxFunction - builds the pseudo function call Max.

                      The Stack:

                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam=1    |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | ProcSym | Type |         Empty
                      |----------------|
*)

PROCEDURE BuildMaxFunction ;
VAR
   combinedtok,
   functok,
   vartok     : CARDINAL ;
   func,
   max,
   NoOfParam,
   Var        : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   func := OperandT (NoOfParam + 1) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      Var := OperandT (1) ;
      vartok := OperandTok (1) ;
      PopN (NoOfParam + 1) ;    (* destroy arguments to this function *)
      combinedtok := MakeVirtualTok (functok, functok, vartok) ;
      IF IsAModula2Type (Var)
      THEN
         max := GetTypeMax (vartok, func, Var) ;
         PushTFtok (max, GetSType (max), combinedtok)
      ELSIF IsVar(Var)
      THEN
         max := GetTypeMax (vartok, func, GetSType (Var)) ;
         PushTFtok (max, GetSType (Var), combinedtok)
      ELSE
         (* we dont know the type therefore cannot fake a return.  *)
         MetaErrorT1 (vartok,
                      'parameter to {%AkMAX} must be a type or a variable, seen {%1ad}',
                      Var)
         (* non recoverable error.  *) ;
      END
   ELSE
      (* we dont know the type therefore cannot fake a return.  *)
      MetaErrorT1 (functok,
                   'the pseudo builtin procedure function {%AkMAX} only has one parameter, seen {%1n}',
                   NoOfParam)
      (* non recoverable error.  *)
   END
END BuildMaxFunction ;


(*
   BuildTruncFunction - builds the pseudo procedure call TRUNC.
                        This procedure is actually a "macro" for
                        TRUNC(x) --> CONVERT(INTEGER, x)
                        However we cannot push tokens back onto the input stack
                        because the compiler is currently building a function
                        call and expecting a ReturnVar on the stack.
                        Hence we manipulate the stack and call
                        BuildConvertFunction.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildTruncFunction (Sym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   vartok,
   functok    : CARDINAL ;
   NoOfParam  : CARDINAL ;
   ReturnVar,
   ProcSym,
   Type,
   Var        : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   Assert (IsTrunc (OperandT (NoOfParam+1))) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam = 1
   THEN
      ProcSym := RequestSym (functok, MakeKey ('CONVERT')) ;
      IF (ProcSym # NulSym) AND IsProcedure (ProcSym)
      THEN
         Var := OperandT (1) ;
         vartok := OperandTtok (1) ;
         Type := GetSType (Sym) ;
         PopN (NoOfParam + 1) ;    (* destroy arguments to this function *)
         IF ConstExprError (Sym, Var, vartok, ConstExpr)
         THEN
            (* Generate fake result.  *)
            combinedtok := MakeVirtual2Tok (functok, vartok) ;
            ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
            PutVar (ReturnVar, Type) ;
            PushTFtok (ReturnVar, Type, combinedtok)
         ELSIF IsVar (Var) OR IsConst (Var)
         THEN
            IF IsRealType (GetSType (Var))
            THEN
               (* build macro: CONVERT( INTEGER, Var ).  *)
               PushTFtok (ProcSym, NulSym, functok) ;
               PushTtok (Type, functok) ;
               PushTtok (Var, vartok) ;
               PushT (2) ;          (* two parameters *)
               BuildConvertFunction (Convert, ConstExpr)
            ELSE
               MetaErrorT1 (functok,
                            'argument to {%1Ead} must be a float point type', Sym) ;
               PushTFtok (MakeConstLit (functok, MakeKey('0'), Type), Type, functok)
            END
         ELSE
            MetaErrorT2 (vartok,
                         'argument to {%1Ead} must be a variable or constant, seen {%2ad}',
                         Sym, Var) ;
            PushTFtok (MakeConstLit (functok, MakeKey('0'), Type), Type, functok)
         END
      ELSE
         InternalError  ('CONVERT procedure not found for TRUNC substitution')
      END
   ELSE
      (* we dont know the type therefore cannot fake a return.  *)
      MetaErrorT1 (functok,
                   'the pseudo builtin procedure function {%AkTRUNC} only has one parameter, seen  {%1n}', NoOfParam)
      (* non recoverable error.  *)
   END
END BuildTruncFunction ;


(*
   BuildFloatFunction - builds the pseudo procedure call FLOAT.
                        This procedure is actually a "macro" for
                        FLOAT(x) --> CONVERT(REAL, x)
                        However we cannot push tokens back onto the input stack
                        because the compiler is currently building a function
                        call and expecting a ReturnVar on the stack.
                        Hence we manipulate the stack and call
                        BuildConvertFunction.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildFloatFunction (Sym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   combinedtok,
   vartok,
   functok    : CARDINAL ;
   NoOfParam  : CARDINAL ;
   ReturnVar,
   Type,
   Var,
   ProcSym    : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTtok (NoOfParam + 1) ;
   Type := GetSType (Sym) ;
   IF NoOfParam = 1
   THEN
      ProcSym := RequestSym (functok, MakeKey ('CONVERT')) ;
      IF (ProcSym # NulSym) AND IsProcedure (ProcSym)
      THEN
         Var := OperandT (1) ;
         vartok := OperandTtok (1) ;
         PopN (NoOfParam + 1) ;    (* destroy arguments to this function.  *)
         IF ConstExprError (Sym, Var, vartok, ConstExpr)
         THEN
            (* Generate fake result.  *)
            combinedtok := MakeVirtual2Tok (functok, vartok) ;
            ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
            PutVar (ReturnVar, Type) ;
            PushTFtok (ReturnVar, Type, combinedtok)
         ELSIF IsVar (Var) OR IsConst (Var)
         THEN
            (* build macro: CONVERT (REAL, Var).  *)
            PushTFtok (ProcSym, NulSym, functok) ;
            PushTtok (Type, functok) ;
            PushTtok (Var, vartok) ;
            PushT(2) ;          (* two parameters.  *)
            BuildConvertFunction (ProcSym, ConstExpr)
         ELSE
            MetaErrorT1 (vartok,
                         'argument to {%1Ead} must be a variable or constant', ProcSym) ;
            PushTFtok (MakeConstLit (functok, MakeKey('0.0'), Type), Type, functok)
         END
      ELSE
         InternalError  ('CONVERT procedure not found for FLOAT substitution')
      END
   ELSE
      PopN (NoOfParam + 1) ;    (* destroy arguments to this function.  *)
      MetaErrorT1 (functok,
                   'the builtin procedure function {%1Ead} only has one parameter',
                   Sym) ;
      PushTFtok (MakeConstLit (functok, MakeKey('0.0'), Type), Type, functok)
   END
END BuildFloatFunction ;


(*
   BuildReFunction - builds the pseudo procedure call RE.

                     The Stack:


                         Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildReFunction (Sym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   func,
   combinedtok,
   vartok,
   functok    : CARDINAL ;
   NoOfParam  : CARDINAL ;
   ReturnVar,
   Type,
   Var        : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTtok (NoOfParam + 1) ;
   func := OperandT (NoOfParam + 1) ;
   IF NoOfParam=1
   THEN
      Var := OperandT (1) ;
      vartok := OperandTok (1) ;
      combinedtok := MakeVirtualTok (functok, functok, vartok) ;
      Type := ComplexToScalar (GetDType (Var)) ;
      PopN (NoOfParam+1) ;  (* destroy arguments to this function *)
      IF ConstExprError (Sym, Var, vartok, ConstExpr)
      THEN
         (* Generate fake result.  *)
         combinedtok := MakeVirtual2Tok (functok, vartok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (ReturnVar, Type) ;
         PushTFtok (ReturnVar, Type, combinedtok)
      ELSIF IsVar(Var) OR IsConst(Var)
      THEN
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (ReturnVar, Type) ;
         GenQuadO (combinedtok, StandardFunctionOp, ReturnVar, Re, Var, FALSE) ;
         PushTFtok (ReturnVar, Type, combinedtok)
      ELSE
         PushTFtok (MakeConstLit (combinedtok, MakeKey ('1.0'), RType), RType, combinedtok) ;
         MetaErrorT2 (vartok,
                      'the parameter to the builtin procedure function {%1Ead} must be a constant or a variable, seen {%2ad}',
                      func, Var)
      END
   ELSE
      PopN (NoOfParam+1) ;  (* destroy arguments to this function *)
      PushTFtok (MakeConstLit (functok, MakeKey ('1.0'), RType), RType, functok) ;
      MetaErrorT2 (functok,
                   'the builtin procedure function {%1Ead} only has one parameter, seen {%2n}',
                   func, NoOfParam)
   END
END BuildReFunction ;


(*
   BuildImFunction - builds the pseudo procedure call IM.

                     The Stack:


                         Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildImFunction (Sym: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   func,
   combinedtok,
   vartok,
   functok    : CARDINAL ;
   NoOfParam  : CARDINAL ;
   ReturnVar,
   Type,
   Var        : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTtok (NoOfParam + 1) ;
   func := OperandT (NoOfParam + 1) ;
   IF NoOfParam=1
   THEN
      Var := OperandT (1) ;
      vartok := OperandTok (1) ;
      Type := ComplexToScalar (GetDType (Var)) ;
      combinedtok := MakeVirtualTok (functok, functok, vartok) ;
      PopN (NoOfParam+1) ;  (* destroy arguments to this function *)
      IF ConstExprError (Sym, Var, vartok, ConstExpr)
      THEN
         (* Generate fake result.  *)
         combinedtok := MakeVirtual2Tok (functok, vartok) ;
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (ReturnVar, Type) ;
         PushTFtok (ReturnVar, Type, combinedtok)
      ELSIF IsVar(Var) OR IsConst(Var)
      THEN
         ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (Var))) ;
         PutVar (ReturnVar, ComplexToScalar (GetDType (Var))) ;
         GenQuadO (combinedtok, StandardFunctionOp, ReturnVar, Im, Var, FALSE) ;
         PushTFtok (ReturnVar, GetSType (ReturnVar), combinedtok)
      ELSE
         PushTFtok (MakeConstLit (combinedtok, MakeKey ('1.0'), RType), RType, combinedtok) ;
         MetaErrorT2 (vartok,
                      'the parameter to the builtin procedure function {%1Ead} must be a constant or a variable, seen {%2ad}',
                      func, Var)
      END
   ELSE
      PopN (NoOfParam+1) ;  (* destroy arguments to this function *)
      PushTFtok (MakeConstLit (functok, MakeKey ('1.0'), RType), RType, functok) ;
      MetaErrorT2 (functok,
                   'the builtin procedure function {%1Ead} only has one parameter, seen {%2n}',
                   func, NoOfParam)
   END
END BuildImFunction ;


(*
   BuildCmplxFunction - builds the pseudo procedure call CMPLX.

                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |
                        |----------------|
                        | ProcSym | Type |         Empty
                        |----------------|
*)

PROCEDURE BuildCmplxFunction (func: CARDINAL; ConstExpr: BOOLEAN) ;
VAR
   failure    : BOOLEAN ;
   functok,
   rtok, ltok,
   combinedtok: CARDINAL ;
   NoOfParam  : CARDINAL ;
   type,
   ReturnVar,
   l, r       : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam = 2
   THEN
      l := OperandT (2) ;
      ltok := OperandTtok (2) ;
      r := OperandT (1) ;
      rtok := OperandTtok (1) ;
      combinedtok := MakeVirtual2Tok (functok, rtok) ;
      PopN (NoOfParam+1) ;   (* Destroy arguments to this function.  *)
      type := GetCmplxReturnType (GetDType (l), GetDType (r)) ;
      ReturnVar := MakeTemporary (combinedtok, AreConstant (IsConst (l) AND IsConst (r))) ;
      PutVar (ReturnVar, type) ;
      failure := FALSE ;
      IF ConstExprError (func, l, ltok, ConstExpr)
      THEN
         (* ConstExprError has generated an error message we will fall through
            and check the right operand.  *)
         failure := TRUE
      END ;
      IF ConstExprError (func, r, rtok, ConstExpr)
      THEN
         (* Right operand is in error as a variable.  *)
         failure := TRUE
      END ;
      IF failure
      THEN
         (* Generate a fake result if either operand was a variable (and we
            are in a const expression).  *)
         PushTFtok (ReturnVar, type, combinedtok)
      ELSIF (IsVar (l) OR IsConst (l)) AND
         (IsVar (r) OR IsConst (r))
      THEN
         CheckExpressionCompatible (combinedtok, GetSType(l), GetSType(r)) ;
         GenQuadO (combinedtok, StandardFunctionOp, ReturnVar, Cmplx, Make2Tuple (l, r), TRUE) ;
         PushTFtok (ReturnVar, type, combinedtok)
      ELSE
         IF IsVar (l) OR IsConst (l)
         THEN
            MetaErrorT2 (functok,
                      'the builtin procedure {%1Ead} requires two parameters, both must be variables or constants but the second parameter is {%2d}',
                      func, r)
         ELSE
            MetaErrorT2 (functok,
                         'the builtin procedure {%1Ead} requires two parameters, both must be variables or constants but the first parameter is {%2d}',
                      func, l)
         END ;
         PushTFtok (MakeConstLit (combinedtok, MakeKey ('1.0'), CType), CType, combinedtok)
      END
   ELSE
      MetaErrorT2 (functok,
                   'the builtin procedure {%1Ead} requires two parameters, seen {%2n}',
                   func, NoOfParam) ;
      PopN (NoOfParam + 1) ;  (* destroy arguments to this function *)
      PushTFtok (MakeConstLit (functok, MakeKey ('1.0'), CType), CType, functok)
   END
END BuildCmplxFunction ;


(*
   BuildAdrFunction - builds the pseudo function ADR
                      The Stack:


                      Entry                      Exit

               Ptr ->
                      +----------------+
                      | NoOfParam      |
                      |----------------|
                      | Param 1        |
                      |----------------|
                      | Param 2        |
                      |----------------|
                      .                .
                      .                .
                      .                .
                      |----------------|
                      | Param #        |                        <- Ptr
                      |----------------|         +------------+
                      | ProcSym | Type |         | ReturnVar  |
                      |----------------|         |------------|

*)

PROCEDURE BuildAdrFunction ;
VAR
   endtok,
   combinedTok,
   procTok,
   t,
   UnboundedSym,
   Dim,
   Field,
   noOfParameters,
   procSym,
   returnVar,
   Type, rw    : CARDINAL ;
BEGIN
   DisplayStack ;
   PopT (noOfParameters) ;
   procSym := OperandT (noOfParameters + 1) ;
   procTok := OperandTok (noOfParameters + 1) ;  (* token of procedure ADR.  *)
   endtok := OperandTok (1) ;  (* last parameter.  *)
   combinedTok := MakeVirtualTok (procTok, procTok, endtok) ;
   IF noOfParameters # 1
   THEN
      MetaErrorNT0 (combinedTok,
                    'SYSTEM procedure ADR expects 1 parameter') ;
      PopN (noOfParameters + 1) ;    (* destroy the arguments and function *)
      PushTF (Nil, Address)
   ELSIF IsConstString (OperandT (1))
   THEN
      returnVar := MakeLeftValue (combinedTok, OperandT (1), RightValue,
                                  GetSType (procSym)) ;
      PopN (noOfParameters + 1) ;    (* destroy the arguments and function *)
      PushTFtok (returnVar, GetSType (returnVar), combinedTok)
   ELSIF (NOT IsVar(OperandT(1))) AND (NOT IsProcedure(OperandT(1)))
   THEN
      MetaErrorNT0 (combinedTok,
                    'SYSTEM procedure ADR expects a variable, procedure or a constant string as its parameter') ;
      PopN (noOfParameters + 1) ;    (* destroy the arguments and function *)
      PushTFtok (Nil, Address, combinedTok)
   ELSIF IsProcedure (OperandT (1))
   THEN
      returnVar := MakeLeftValue (combinedTok, OperandT (1), RightValue,
                                  GetSType (procSym)) ;
      PopN (noOfParameters + 1) ;    (* destroy the arguments and function *)
      PushTFtok (returnVar, GetSType (returnVar), combinedTok)
   ELSE
      Type := GetSType (OperandT (1)) ;
      Dim := OperandD (1) ;
      MarkArrayWritten (OperandT (1)) ;
      MarkArrayWritten (OperandA (1)) ;
      (* if the operand is an unbounded which has not been indexed
         then we will lookup its address from the unbounded record.
         Otherwise we obtain the address of the operand.
      *)
      IF IsUnbounded (Type) AND (Dim = 0)
      THEN
         (* we will reference the address field of the unbounded structure *)
         UnboundedSym := OperandT (1) ;
         rw := OperandRW (1) ;
         PushTFrw (UnboundedSym, GetSType (UnboundedSym), rw) ;
         Field := GetUnboundedAddressOffset (GetSType (UnboundedSym)) ;
         PushTF (Field, GetSType (Field)) ;
         PushT (1) ;
         BuildDesignatorRecord (combinedTok) ;
         PopTrw (returnVar, rw) ;
         IF GetMode (returnVar) = LeftValue
         THEN
            t := MakeTemporary (combinedTok, RightValue) ;
            PutVar (t, GetSType (procSym)) ;
            doIndrX (combinedTok, t, returnVar) ;
            returnVar := t
         ELSE
            (* we need to cast returnVar into ADDRESS *)
            t := MakeTemporary (combinedTok, RightValue) ;
            PutVar (t, GetSType (procSym)) ;
            GenQuadO (combinedTok, ConvertOp, t, GetSType (procSym), returnVar, FALSE) ;
            returnVar := t
         END
      ELSE
         returnVar := MakeTemporary (combinedTok, RightValue) ;
         PutVar (returnVar, GetSType (procSym)) ;
         IF GetMode (OperandT (1)) = LeftValue
         THEN
            PutVar (returnVar, GetSType (procSym)) ;
            GenQuadO (combinedTok, ConvertOp, returnVar, GetSType (procSym), OperandT (1), FALSE)
         ELSE
            GenQuadO (combinedTok, AddrOp, returnVar, NulSym, OperandT (1), FALSE)
         END ;
         PutWriteQuad (OperandT (1), GetMode (OperandT (1)), NextQuad-1) ;
         rw := OperandMergeRW (1) ;
         Assert (IsLegal (rw))
      END ;
      PopN (noOfParameters + 1) ;    (* destroy the arguments and function *)
      PushTFrwtok (returnVar, GetSType (returnVar), rw, combinedTok)
   END
END BuildAdrFunction ;


(*
   BuildSizeFunction - builds the pseudo function SIZE
                       The Stack:


                       Entry                      Exit

                Ptr ->
                       +----------------+
                       | NoOfParam      |
                       |----------------|
                       | Param 1        |
                       |----------------|
                       | Param 2        |
                       |----------------|
                       .                .
                       .                .
                       .                .
                       |----------------|
                       | Param #        |                        <- Ptr
                       |----------------|         +------------+
                       | ProcSym | Type |         | ReturnVar  |
                       |----------------|         |------------|
*)

PROCEDURE BuildSizeFunction ;
VAR
   resulttok,
   paramtok,
   functok     : CARDINAL ;
   dim         : CARDINAL ;
   Type,
   NoOfParam,
   ProcSym,
   ReturnVar   : CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   ProcSym := OperandT (NoOfParam + 1) ;
   functok := OperandTtok (NoOfParam + 1) ;
   IF NoOfParam # 1
   THEN
      MetaErrorT1 (functok,
                   '{%E} SYSTEM procedure function {%kSIZE} requires one parameter, seen {%1n}',
                   NoOfParam) ;
      resulttok := functok ;
      ReturnVar := MakeConstLit (resulttok, MakeKey('0'), Cardinal)
   ELSIF IsAModula2Type (OperandT (1))
   THEN
      paramtok := OperandTok (1) ;
      resulttok := MakeVirtualTok (functok, functok, paramtok) ;
      BuildSizeCheckEnd (ProcSym) ;   (* Quadruple generation now on.  *)
      ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
      GenQuadO (resulttok, SizeOp, ReturnVar, NulSym, OperandT(1), TRUE)
   ELSIF IsVar (OperandT (1))
   THEN
      BuildSizeCheckEnd (ProcSym) ;   (* Quadruple generation now on.  *)
      Type := GetSType (OperandT (1)) ;
      paramtok := OperandTok (1) ;
      resulttok := MakeVirtualTok (functok, functok, paramtok) ;
      IF IsUnbounded (Type)
      THEN
         (* Eg. SIZE(a) ; where a is unbounded dereference HIGH and multiply by the TYPE.  *)
         dim := OperandD (1) ;
         IF dim = 0
         THEN
            ReturnVar := calculateMultipicand (resulttok, OperandT (1), Type, dim)
         ELSE
            ReturnVar := calculateMultipicand (resulttok, OperandA (1), Type, dim)
         END
      ELSE
         ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
         IF Type = NulSym
         THEN
            MetaErrorT1 (resulttok,
                         'cannot get the type and size of {%1Ead}', OperandT (1))
         END ;
         GenQuadO (resulttok, SizeOp, ReturnVar, NulSym, Type, TRUE)
      END
   ELSE
      resulttok := functok ;
      MetaErrorT1 (resulttok,
                   '{%E}SYSTEM procedure {%kSIZE} expects a variable as its parameter, seen {%1Ed}',
                   OperandT (1)) ;
      ReturnVar := MakeConstLit (resulttok, MakeKey('0'), Cardinal)
   END ;
   PopN (NoOfParam+1) ;       (* Destroy the arguments and function.  *)
   PushTFtok (ReturnVar, GetSType(ProcSym), resulttok)
END BuildSizeFunction ;


(*
   BuildTSizeFunction - builds the pseudo function TSIZE
                        The Stack:


                        Entry                      Exit

                 Ptr ->
                        +----------------+
                        | NoOfParam      |
                        |----------------|
                        | Param 1        |
                        |----------------|
                        | Param 2        |
                        |----------------|
                        .                .
                        .                .
                        .                .
                        |----------------|
                        | Param #        |                        <- Ptr
                        |----------------|         +------------+
                        | ProcSym | Type |         | ReturnVar  |
                        |----------------|         |------------|

*)

PROCEDURE BuildTSizeFunction ;
VAR
   resulttok,
   paramtok,
   functok  : CARDINAL ;
   NoOfParam: CARDINAL ;
   ProcSym,
   Record,
   ReturnVar: CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   ProcSym := OperandT (NoOfParam + 1) ;
   functok := OperandTtok (NoOfParam) ;
   BuildSizeCheckEnd (ProcSym) ;   (* quadruple generation now on *)
   IF NoOfParam = 1
   THEN
      paramtok := OperandTtok (1) ;
      resulttok := MakeVirtualTok (functok, functok, paramtok) ;
      IF IsAModula2Type (OperandT (1))
      THEN
         ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
         PutVar (ReturnVar, Cardinal) ;
         GenQuadO (resulttok, SizeOp, ReturnVar, NulSym, OperandT (1), FALSE)
      ELSIF IsVar (OperandT (1))
      THEN
         ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
         PutVar (ReturnVar, Cardinal) ;
         GenQuadO (resulttok, SizeOp, ReturnVar, NulSym, GetSType (OperandT (1)), FALSE)
      ELSE
         MetaErrorT1 (resulttok,
                      '{%E}SYSTEM procedure function {%kTSIZE} expects a variable as its first parameter, seen {%1Ed}',
                      OperandT (1)) ;
         ReturnVar := MakeConstLit (resulttok, MakeKey ('0'), Cardinal)
      END
   ELSIF NoOfParam = 0
   THEN
      resulttok := functok ;
      MetaErrorT0 (resulttok,
                   '{%E}SYSTEM procedure function {%kTSIZE} expects either one or two parameters, seen none') ;
      ReturnVar := MakeConstLit (resulttok, MakeKey ('0'), Cardinal)
   ELSE
      Record := OperandT (NoOfParam) ;
      paramtok := OperandTtok (1) ;
      resulttok := OperandTtok (NoOfParam) ;
      IF IsRecord (Record)
      THEN
         paramtok := OperandTtok (1) ;
         resulttok := MakeVirtualTok (functok, functok, paramtok) ;
         ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
         PutVar (ReturnVar, Cardinal) ;
         GenQuadO (resulttok, SizeOp, ReturnVar, NulSym, Record, FALSE)
      ELSE
         resulttok := MakeVirtualTok (functok, functok, paramtok) ;
         MetaErrorT1 (resulttok,
                      '{%E}SYSTEM procedure function {%kTSIZE} expects the first parameter to be a record type, seen {%1d}',
                      Record) ;
         ReturnVar := MakeConstLit (resulttok, MakeKey ('0'), Cardinal)
      END
   END ;
   PopN (NoOfParam+1) ;       (* destroy the arguments and function *)
   PushTFtok (ReturnVar, GetSType (ProcSym), resulttok)
END BuildTSizeFunction ;


(*
   BuildTBitSizeFunction - builds the pseudo function TBITSIZE
                           The Stack:


                           Entry                      Exit

                   Ptr ->
                           +----------------+
                           | NoOfParam      |
                           |----------------|
                           | Param 1        |
                           |----------------|
                           | Param 2        |
                           |----------------|
                           .                .
                           .                .
                           .                .
                           |----------------|
                           | Param #        |                        <- Ptr
                           |----------------|         +------------+
                           | ProcSym | Type |         | ReturnVar  |
                           |----------------|         |------------|

*)

PROCEDURE BuildTBitSizeFunction ;
VAR
   resulttok,
   paramtok,
   functok  : CARDINAL ;
   NoOfParam: CARDINAL ;
   ProcSym,
   Record,
   ReturnVar: CARDINAL ;
BEGIN
   PopT (NoOfParam) ;
   ProcSym := OperandT (NoOfParam + 1) ;
   functok := OperandTtok (NoOfParam) ;
   BuildSizeCheckEnd (ProcSym) ;   (* quadruple generation now on *)
   IF NoOfParam = 1
   THEN
      paramtok := OperandTtok (1) ;
      resulttok := MakeVirtualTok (functok, functok, paramtok) ;
      IF IsAModula2Type (OperandT (1))
      THEN
         ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
         GenQuadO (resulttok, StandardFunctionOp, ReturnVar, ProcSym, OperandT (1), FALSE)
      ELSIF IsVar (OperandT (1))
      THEN
         ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
         GenQuadO (resulttok, StandardFunctionOp, ReturnVar, ProcSym, OperandT(1), FALSE)
      ELSE
         MetaErrorT1 (resulttok,
                      '{%E}SYSTEM procedure function {%kTBITSIZE} expects a variable as its first parameter, seen {%1d}',
                      OperandT (1)) ;
         ReturnVar := MakeConstLit (resulttok, MakeKey ('0'), Cardinal)
      END
   ELSIF NoOfParam = 0
   THEN
      resulttok := functok ;
      MetaErrorT0 (functok,
                   '{%E}SYSTEM procedure function {%kTBITSIZE} expects either one or two parameters, seen none') ;
      ReturnVar := MakeConstLit (functok, MakeKey ('0'), Cardinal)
   ELSE
      Record := OperandT (NoOfParam) ;
      paramtok := OperandTtok (1) ;
      resulttok := OperandTtok (NoOfParam) ;
      IF IsRecord (Record)
      THEN
         paramtok := OperandTtok (1) ;
         resulttok := MakeVirtualTok (functok, functok, paramtok) ;
         ReturnVar := MakeTemporary (resulttok, ImmediateValue) ;
         GenQuad(StandardFunctionOp, ReturnVar, ProcSym, OperandT(1)) ;
      ELSE
         resulttok := MakeVirtualTok (functok, functok, paramtok) ;
         MetaErrorT1 (resulttok,
                      '{%E}SYSTEM procedure function {%kTBITSIZE} expects the first parameter to be a record type, seen {%1d}',
                      Record) ;
         ReturnVar := MakeConstLit (resulttok, MakeKey ('0'), Cardinal)
      END
   END ;
   PopN (NoOfParam + 1) ;       (* destroy the arguments and function *)
   PushTFtok (ReturnVar, GetSType (ProcSym), resulttok)
END BuildTBitSizeFunction ;


(*
   ExpectingParameterType -
*)

PROCEDURE ExpectingParameterType (BlockSym, Type: CARDINAL) ;
BEGIN
   IF NOT IsAModula2Type (Type)
   THEN
      IF (Type = NulSym) OR IsPartialUnbounded (Type) OR IsUnknown (Type)
      THEN
         MetaError1 ('the type used in the formal parameter declaration in {%1Md} {%1a} is unknown',
                     BlockSym)
      ELSE
         MetaError2 ('the type {%1Ead} used in the formal parameter declaration in {%2Md} {%2a} was not declared as a type',
                     Type, BlockSym)
      END
   END
END ExpectingParameterType ;


(*
   ExpectingVariableType -
*)

PROCEDURE ExpectingVariableType (BlockSym, Type: CARDINAL) ;
BEGIN
   IF NOT IsAModula2Type(Type)
   THEN
      IF Type=NulSym
      THEN
         MetaError1 ('the type used during the variable declaration section in procedure {%1EMad} is unknown',
                     BlockSym) ;
         MetaError1 ('the type used during the variable declaration section in procedure {%1Ead} is unknown',
                     BlockSym)
      ELSIF IsPartialUnbounded(Type) OR IsUnknown(Type)
      THEN
         MetaError2 ('the type {%1EMad} used during variable declaration section in procedure {%2ad} is unknown',
                     Type, BlockSym) ;
         MetaError2 ('the type {%1Ead} used during variable declaration section in procedure {%2Mad} is unknown',
                     Type, BlockSym)
      ELSE
         MetaError2 ('the {%1d} {%1Ea} is not a type and therefore cannot be used to declare a variable in {%2d} {%2a}',
                     Type, BlockSym)
      END
   END
END ExpectingVariableType ;


(*
   CheckVariablesAndParameterTypesInBlock - checks to make sure that block, BlockSym, has
                                            parameters types and variable types which are legal.
*)

PROCEDURE CheckVariablesAndParameterTypesInBlock (BlockSym: CARDINAL) ;
VAR
   i, n,
   ParamNo: CARDINAL ;
BEGIN
   IF IsProcedure(BlockSym)
   THEN
      ParamNo := NoOfParam(BlockSym)
   ELSE
      ParamNo := 0
   END ;
   i := 1 ;
   REPEAT
      n := GetNth(BlockSym, i) ;
      IF (n#NulSym) AND (NOT IsTemporary(n)) AND
         (IsProcedure(BlockSym) OR ((IsDefImp(BlockSym) AND (GetMainModule()=BlockSym)) OR IsModule(BlockSym)))
      THEN
         IF i<=ParamNo
         THEN
            (* n is a parameter *)
            ExpectingParameterType(BlockSym, GetSType(n))
         ELSE
            (* n is a local variable *)
            ExpectingVariableType(BlockSym, GetSType(n))
         END
      END ;
      INC(i)
   UNTIL n=NulSym ;
END CheckVariablesAndParameterTypesInBlock ;


(*
   BuildProcedureStart - Builds start of the procedure. Generates a
                         quadruple which indicated the start of
                         this procedure declarations scope.
                         The Stack is expected to contain:


                         Entry                   Exit
                         =====                   ====

                 Ptr ->                                       <- Ptr
                        +------------+          +-----------+
                        | ProcSym    |          | ProcSym   |
                        |------------|          |-----------|
                        | Name       |          | Name      |
                        |------------|          |-----------|


                        Quadruples:

                        q   ProcedureScopeOp  Line#  Scope  ProcSym
*)

PROCEDURE BuildProcedureStart ;
VAR
   ProcSym: CARDINAL ;
BEGIN
   PopT(ProcSym) ;
   Assert(IsProcedure(ProcSym)) ;
   PutProcedureScopeQuad(ProcSym, NextQuad) ;
   GenQuad(ProcedureScopeOp, GetPreviousTokenLineNo(), GetScope(ProcSym), ProcSym) ;
   PushT(ProcSym)
END BuildProcedureStart ;


(*
   BuildProcedureBegin - determines the start of the BEGIN END block of
                         the procedure.
                         The Stack is expected to contain:


                         Entry                   Exit
                         =====                   ====

                 Ptr ->                                       <- Ptr
                        +------------+          +-----------+
                        | ProcSym    |          | ProcSym   |
                        |------------|          |-----------|
                        | Name       |          | Name      |
                        |------------|          |-----------|


                        Quadruples:

                        q   NewLocalVarOp  TokenNo(BEGIN)  _  ProcSym
*)

PROCEDURE BuildProcedureBegin ;
VAR
   ProcSym: CARDINAL ;
BEGIN
   PopT(ProcSym) ;
   Assert(IsProcedure(ProcSym)) ;
   PutProcedureStartQuad(ProcSym, NextQuad) ;
   PutProcedureBegin(ProcSym, GetTokenNo()) ;
   GenQuad(NewLocalVarOp, GetTokenNo(), GetScope(ProcSym), ProcSym) ;
   CurrentProc := ProcSym ;
   PushWord(ReturnStack, 0) ;
   PushT(ProcSym) ;
   CheckVariablesAt(ProcSym) ;
   CheckNeedPriorityBegin(GetTokenNo(), ProcSym, GetCurrentModule()) ;
   PushWord(TryStack, NextQuad) ;
   PushWord(CatchStack, 0) ;
   IF HasExceptionBlock(ProcSym)
   THEN
      GenQuad(TryOp, NulSym, NulSym, 0)
   END
END BuildProcedureBegin ;


(*
   BuildProcedureEnd - Builds end of the procedure. Destroys space for
                       the local variables.
                       The Stack is expected to contain:


                       Entry                   Exit
                       =====                   ====

                Ptr ->                                       <- Ptr
                       +------------+          +-----------+
                       | ProcSym    |          | ProcSym   |
                       |------------|          |-----------|
                       | Name       |          | Name      |
                       |------------|          |-----------|


                       Quadruples:

                       q   KillLocalVarOp  TokenNo(END)  _  ProcSym
*)

PROCEDURE BuildProcedureEnd ;
VAR
   tok    : CARDINAL ;
   ProcSym: CARDINAL ;
BEGIN
   PopTtok(ProcSym, tok) ;
   IF HasExceptionBlock(ProcSym)
   THEN
      BuildRTExceptLeave(tok, TRUE) ;
      GenQuad(CatchEndOp, NulSym, NulSym, NulSym)
   END ;
   IF GetSType(ProcSym)#NulSym
   THEN
      BuildError(InitNoReturnRangeCheck())
   END ;
   BackPatch(PopWord(ReturnStack), NextQuad) ;
   CheckNeedPriorityEnd(tok, ProcSym, GetCurrentModule()) ;
   CurrentProc := NulSym ;
   PutProcedureEnd(ProcSym, GetTokenNo()-1) ;   (* --fixme-- *)
   GenQuad(KillLocalVarOp, GetTokenNo()-1, NulSym, ProcSym) ;
   PutProcedureEndQuad(ProcSym, NextQuad) ;
   GenQuad(ReturnOp, NulSym, NulSym, ProcSym) ;
   CheckFunctionReturn(ProcSym) ;
   CheckVariablesInBlock(ProcSym) ;
   RemoveTop (CatchStack) ;
   RemoveTop (TryStack) ;
   PushT(ProcSym)
END BuildProcedureEnd ;


(*
   IsNeverAltered - returns TRUE if variable, sym, is never altered
                    between quadruples: Start..End
*)

PROCEDURE IsNeverAltered (sym: CARDINAL; Start, End: CARDINAL) : BOOLEAN ;
VAR
   WriteStart, WriteEnd: CARDINAL ;
BEGIN
   GetWriteLimitQuads (sym, GetMode (sym), Start, End, WriteStart, WriteEnd) ;
   RETURN( (WriteStart = 0) AND (WriteEnd = 0) )
END IsNeverAltered ;


(*
   IsConditionVariable - returns TRUE if the condition at quadruple, q, is variable.
*)

PROCEDURE IsConditionVariable (q: CARDINAL; Start, End: CARDINAL) : BOOLEAN ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   LeftFixed,
   RightFixed   : BOOLEAN ;
BEGIN
   GetQuad (q, op, op1, op2, op3) ;
   IF op = GotoOp
   THEN
      RETURN( FALSE )
   ELSE
      LeftFixed  := IsConst(op1) ;
      RightFixed := IsConst(op2) ;
      IF NOT LeftFixed
      THEN
         LeftFixed := IsNeverAltered(op1, Start, End)
      END ;
      IF NOT RightFixed
      THEN
         RightFixed := IsNeverAltered(op2, Start, End)
      END ;
      RETURN( NOT (LeftFixed AND RightFixed) )
   END
END IsConditionVariable ;


(*
   IsInfiniteLoop - returns TRUE if an infinite loop is found.
                    Given a backwards jump at, End, it returns a BOOLEAN which depends on
                    whether a jump is found to jump beyond, End. If a conditonal jump is found
                    to pass over, End, the condition is tested for global variables, procedure variables and
                    constants.

                         constant        - ignored
                         variables       - tested to see whether they are altered inside the loop
                         global variable - the procedure tests to see whether it is altered as above
                                           but will also test to see whether this loop calls a procedure
                                           in which case it believes the loop NOT to be infinite
                                           (as this procedure call might alter the global variable)

                    Note that this procedure can easily be fooled by the user altering variables
                    with pointers.
*)

PROCEDURE IsInfiniteLoop (End: CARDINAL) : BOOLEAN ;
VAR
   SeenCall,
   IsGlobal     : BOOLEAN ;
   Current,
   Start        : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   SeenCall := FALSE ;
   IsGlobal := FALSE ;
   GetQuad(End, op, op1, op2, Start) ;
   Current := Start ;
   WHILE Current#End DO
      GetQuad(Current, op, op1, op2, op3) ;
      (* remember that this function is only called once we have optimized the redundant gotos and conditionals *)
      IF IsConditional(Current) AND (NOT IsGlobal)
      THEN
         IsGlobal := (IsVar(op1) AND (NOT IsProcedure(GetVarScope(op1)))) OR
                     (IsVar(op2) AND (NOT IsProcedure(GetVarScope(op2))))
      END ;
      IF op=CallOp
      THEN
         SeenCall := TRUE
      END ;
      IF (op=GotoOp) OR (IsConditional(Current) AND IsConditionVariable(Current, Start, End))
      THEN
         IF (op3>End) OR (op3<Start)
         THEN
            RETURN( FALSE )    (* may jump out of this loop, good *)
         END
      END ;
      Current := GetNextQuad(Current)
   END ;
   GetQuad(End, op, op1, op2, op3) ;
   IF IsConditional(End)
   THEN
      IF IsConditionVariable(End, Start, End)
      THEN
         RETURN( FALSE )
      ELSE
         IF NOT IsGlobal
         THEN
            IsGlobal := (IsVar(op1) AND (NOT IsProcedure(GetVarScope(op1)))) OR
                        (IsVar(op2) AND (NOT IsProcedure(GetVarScope(op2))))
         END
      END
   END ;
   (* we have found a likely infinite loop if no conditional uses a global and no procedure call was seen *)
   RETURN( NOT (IsGlobal AND SeenCall) )
END IsInfiniteLoop ;


(*
   LoopAnalysis - checks whether an infinite loop exists.
*)

PROCEDURE LoopAnalysis (Scope: CARDINAL; Current, End: CARDINAL) ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   IF Pedantic
   THEN
      WHILE (Current<=End) AND (Current#0) DO
         GetQuad(Current, op, op1, op2, op3) ;
         IF (op=GotoOp) OR IsConditional(Current)
         THEN
            IF op3<=Current
            THEN
               (* found a loop - ie a branch which goes back in quadruple numbers *)
               IF IsInfiniteLoop(Current)
               THEN
                  MetaErrorT1 (QuadToTokenNo(op3),
                               'it is very likely (although not absolutely certain) that the top of an infinite loop exists here in {%1Wad}',
                               Scope) ;
                  MetaErrorT1 (QuadToTokenNo(Current),
                               'and the bottom of the infinite loop is ends here in {%1Wad} or alternatively a component of this loop is never executed',
                               Scope) ;
(*
                  WarnStringAt(InitString('it is very likely (although not absolutely certain) that the top of an infinite loop is here'),
                               QuadToTokenNo(op3)) ;
                  WarnStringAt(InitString('and the bottom of the infinite loop is ends here or alternatively a component of this loop is never executed'),
                               QuadToTokenNo(Current))
*)
               END
            END
         END ;
         Current := GetNextQuad(Current)
      END
   END
END LoopAnalysis ;


(*
   CheckVariablesInBlock - given a block, BlockSym, check whether all variables are used.
*)

PROCEDURE CheckVariablesInBlock (BlockSym: CARDINAL) ;
BEGIN
   CheckVariablesAndParameterTypesInBlock (BlockSym)
END CheckVariablesInBlock ;


(*
   CheckFunctionReturn - checks to see that a RETURN statement was present in a function.
*)

PROCEDURE CheckFunctionReturn (ProcSym: CARDINAL) ;
VAR
   Op           : QuadOperator ;
   Op1, Op2, Op3,
   Scope,
   Start, End   : CARDINAL ;
BEGIN
   IF GetSType(ProcSym)#NulSym
   THEN
      (* yes it is a function *)
      GetProcedureQuads(ProcSym, Scope, Start, End) ;
      GetQuad(Start, Op, Op1, Op2, Op3) ;
      IF Start=0
      THEN
         InternalError ('incorrect start quad')
      END ;
      WHILE (Start#End) AND (Op#ReturnValueOp) AND (Op#InlineOp) DO
         Start := GetNextQuad(Start) ;
         GetQuad(Start, Op, Op1, Op2, Op3)
      END ;
      IF (Op#ReturnValueOp) AND (Op#InlineOp)
      THEN
         (* an InlineOp can always be used to emulate a RETURN *)
         MetaError1 ('procedure function {%1Ea} does not RETURN a value', ProcSym)
      END
   END
END CheckFunctionReturn ;


(*
   CheckReturnType - checks to see that the return type from currentProc is
                     assignment compatible with actualType.
*)

PROCEDURE CheckReturnType (tokno: CARDINAL; currentProc, actualVal, actualType: CARDINAL) ;
VAR
   procType: CARDINAL ;
   s1, s2  : String ;
   n1, n2  : Name ;
BEGIN
   procType := GetSType (currentProc) ;
   IF procType = NulSym
   THEN
      MetaError1 ('attempting to RETURN a value from procedure {%1Ea} which was not a declared as a procedure function', currentProc)
   ELSIF AssignmentRequiresWarning (actualType, GetSType (currentProc))
   THEN
      MetaError2 ('attempting to RETURN a value {%1Wa} with an incompatible type {%1Wtsa} from a procedure function {%1a} which returns {%1tsa}', actualVal, currentProc)
   ELSIF NOT IsAssignmentCompatible (actualType, procType)
   THEN
      n1 := GetSymName(actualType) ;
      n2 := GetSymName(procType) ;
      WriteFormat2('attempting to RETURN a value with an incompatible type (%a) from a function which returns (%a)',
                   n1, n2)
   ELSIF IsProcedure(actualVal) AND (NOT IsAssignmentCompatible(actualVal, procType))
   THEN
(*
      MetaWarnings2('attempting to RETURN a value with an incompatible type {%1ad} from function {%2a} which returns {%2ta}',
                    actualVal, currentProc)

      --fixme--  introduce MetaWarning, MetaWarning2, MetaWarning3 into M2MetaError
*)
      s1 := InitStringCharStar(KeyToCharStar(GetSymName(actualVal))) ;
      s2 := InitStringCharStar(KeyToCharStar(GetSymName(procType))) ;
      ErrorString(NewWarning(GetTokenNo()),
                  Sprintf2(Mark(InitString('attempting to RETURN a value with a (possibly on other targets) incompatible type (%s) from a function which returns (%s)')),
                           s1, s2))
   ELSIF IsProcedure(actualVal) AND (NOT IsAssignmentCompatible(actualVal, GetSType(CurrentProc)))
   THEN
      n1 := GetSymName(actualVal) ;
      n2 := GetSymName(GetSType(currentProc)) ;
      WriteFormat2('attempting to RETURN a value with an incompatible type (%a) from a function which returns (%a)',
                   n1, n2)
   ELSE
      (* this checks the types are compatible, not the data contents.  *)
      BuildRange (InitTypesAssignmentCheck (tokno, currentProc, actualVal))
   END
END CheckReturnType ;


(*
   BuildReturn - Builds the Return part of the procedure.
                 tokreturn is the location of the RETURN keyword.
                 The Stack is expected to contain:


                 Entry                   Exit
                 =====                   ====

         Ptr ->
                 +------------+
                 | e1         |          Empty
                 |------------|
*)

PROCEDURE BuildReturn (tokreturn: CARDINAL) ;
VAR
   tokcombined,
   tokexpr    : CARDINAL ;
   e2, t2,
   e1, t1,
   t, f,
   Des        : CARDINAL ;
BEGIN
   IF IsBoolean (1)
   THEN
      PopBooltok (t, f, tokexpr) ;
      (* Des will be a boolean type *)
      Des := MakeTemporary (tokexpr, RightValue) ;
      PutVar (Des, Boolean) ;
      PushTFtok (Des, Boolean, tokexpr) ;
      PushBooltok (t, f, tokexpr) ;
      BuildAssignmentWithoutBounds (tokreturn, FALSE, TRUE) ;
      PushTFtok (Des, Boolean, tokexpr)
   END ;
   PopTFtok (e1, t1, tokexpr) ;
   tokcombined := MakeVirtualTok (tokreturn, tokreturn, tokexpr) ;
   IF e1 # NulSym
   THEN
      (* this will check that the type returned is compatible with
         the formal return type of the procedure.  *)
      CheckReturnType (tokcombined, CurrentProc, e1, t1) ;
      (* dereference LeftValue if necessary *)
      IF GetMode (e1) = LeftValue
      THEN
         t2 := GetSType (CurrentProc) ;
         e2 := MakeTemporary (tokexpr, RightValue) ;
         PutVar(e2, t2) ;
         CheckPointerThroughNil (tokexpr, e1) ;
         doIndrX (tokexpr, e2, e1) ;
	 (* here we check the data contents to ensure no overflow.  *)
         BuildRange (InitReturnRangeCheck (tokcombined, CurrentProc, e2)) ;
         GenQuadOtok (tokcombined, ReturnValueOp, e2, NulSym, CurrentProc, FALSE,
                      tokcombined, UnknownTokenNo, GetDeclaredMod (CurrentProc))
      ELSE
	 (* here we check the data contents to ensure no overflow.  *)
         BuildRange (InitReturnRangeCheck (tokcombined, CurrentProc, e1)) ;
         GenQuadOtok (tokcombined, ReturnValueOp, e1, NulSym, CurrentProc, FALSE,
                      tokcombined, UnknownTokenNo, GetDeclaredMod (CurrentProc))
      END
   END ;
   GenQuadO (tokcombined, GotoOp, NulSym, NulSym, PopWord (ReturnStack), FALSE) ;
   PushWord (ReturnStack, NextQuad-1)
END BuildReturn ;


(*
   IsReadOnly - a helper procedure function to detect constants.
*)

PROCEDURE IsReadOnly (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN IsConst (sym) OR (IsVar (sym) AND IsVarConst (sym))
END IsReadOnly ;


(*
   BuildDesignatorRecord - Builds the record referencing.
                           The Stack is expected to contain:


                           Entry                   Exit
                           =====                   ====

                   Ptr ->
                           +--------------+
                           | n            |
                           |--------------|
                           | fld1 | type1 |
                           |--------------|
                           .              .
                           .              .
                           .              .
                           |--------------|
                           | fldn | typen |                        <- Ptr
                           |--------------|        +-------------+
                           | Sym  | Type  |        | S    | type1|
                           |--------------|        |-------------|
*)

PROCEDURE BuildDesignatorRecord (dottok: CARDINAL) ;
VAR
   RecordTok,
   FieldTok,
   combinedtok: CARDINAL ;
   n, rw,
   Field,
   FieldType,
   RecordSym,
   Res        : CARDINAL ;
BEGIN
   PopT(n) ;
   RecordSym := OperandT (n+1) ;
   (* RecordType could be found by:  SkipType (OperandF (n+1)).  *)
   RecordTok := OperandTok (n+1) ;
   rw := OperandMergeRW (n+1) ;
   Assert (IsLegal (rw)) ;
   Field := OperandT (n) ;
   FieldType := SkipType (OperandF (n)) ;
   FieldTok := OperandTok (n) ;
   combinedtok := MakeVirtualTok (dottok, RecordTok, FieldTok) ;
   IF n>1
   THEN
      InternalError ('not expecting to see n>1')
   END ;
   IF IsUnused (Field)
   THEN
      MetaErrors1 ('record field {%1Dad} was declared as unused by a pragma',
                   'record field {%1ad} is being used after being declared as unused by a pragma', Field)
   END ;
   Res := MakeComponentRef (MakeComponentRecord (combinedtok,
                                                 RightValue, RecordSym), Field) ;
   PutVarConst (Res, IsReadOnly (RecordSym)) ;
   GenQuadO (combinedtok, RecordFieldOp, Res, RecordSym, Field, FALSE) ;
   PopN (n+1) ;
   PushTFrwtok (Res, FieldType, rw, combinedtok)
END BuildDesignatorRecord ;


(*
   BuildDesignatorError - removes the designator from the stack and replaces
                          it with an error symbol.
*)

PROCEDURE BuildDesignatorError (message: ARRAY OF CHAR) ;
VAR
   combinedTok,
   arrayTok,
   exprTok    : CARDINAL ;
   e, d, error,
   Sym,
   Type       : CARDINAL ;
BEGIN
   PopTtok (e, exprTok) ;
   PopTFDtok (Sym, Type, d, arrayTok) ;
   combinedTok := MakeVirtualTok (arrayTok, arrayTok, exprTok) ;
   error := MakeError (combinedTok, MakeKey (message)) ;
   PushTFDtok (error, Type, d, arrayTok)
END BuildDesignatorError ;



(*
   BuildDesignatorArray - Builds the array referencing.
                          The purpose of this procedure is to work out
                          whether the DesignatorArray is a static or
                          dynamic array and to call the appropriate
                          BuildRoutine.

                          The Stack is expected to contain:


                          Entry                   Exit
                          =====                   ====

                  Ptr ->
                          +--------------+
                          | e            |                        <- Ptr
                          |--------------|        +------------+
                          | Sym  | Type  |        | S    | T   |
                          |--------------|        |------------|
*)

PROCEDURE BuildDesignatorArray ;
VAR
   combinedTok,
   arrayTok,
   exprTok     : CARDINAL ;
   e, type, dim,
   result,
   Sym,
   Type        : CARDINAL ;
BEGIN
   IF IsConst (OperandT (2))
   THEN
      type := GetDType (OperandT (2)) ;
      IF type = NulSym
      THEN
         InternalError ('constant type should have been resolved')
      ELSIF IsArray (type)
      THEN
         PopTtok (e, exprTok) ;
         PopTFDtok (Sym, Type, dim, arrayTok) ;
         result := MakeTemporary (exprTok, RightValue) ;
         PutVar (result, Type) ;
         PushTFtok (result, GetSType (result), exprTok) ;
         PushTtok (Sym, arrayTok) ;
         combinedTok := MakeVirtualTok (arrayTok, arrayTok, exprTok) ;
         PutVarConst (result, TRUE) ;
         BuildAssignConstant (combinedTok) ;
         PushTFDtok (result, GetDType (result), dim, arrayTok) ;
         PushTtok (e, exprTok)
      END
   END ;
   IF (NOT IsVar (OperandT (2))) AND (NOT IsTemporary (OperandT (2)))
   THEN
      MetaErrorT1 (OperandTtok (2),
                   'can only access arrays using variables or formal parameters not {%1Ead}',
                   OperandT (2)) ;
      BuildDesignatorError ('bad array access')
   END ;
   Sym := OperandT (2) ;
   Type := GetDType (Sym) ;
   arrayTok := OperandTtok (2) ;
   IF Type = NulSym
   THEN
      IF (arrayTok = UnknownTokenNo) OR (arrayTok = BuiltinTokenNo)
      THEN
         arrayTok := GetTokenNo ()
      END ;
      MetaErrorT0 (arrayTok, "type of array is undefined") ;
      BuildDesignatorError ('bad array access')
   ELSIF IsUnbounded (Type)
   THEN
      BuildDynamicArray
   ELSIF IsArray (Type)
   THEN
      BuildStaticArray
   ELSE
      MetaErrorT1 (arrayTok,
                   'can only index static or dynamic arrays, {%1Ead} is not an array but a {%tad}',
                   Sym) ;
      BuildDesignatorError ('bad array access')
   END
END BuildDesignatorArray ;


(*
   BuildStaticArray - Builds the array referencing for static arrays.
                      The Stack is expected to contain:


                      Entry                   Exit
                      =====                   ====

              Ptr ->
                      +--------------+
                      | e            |                       <- Ptr
                      |--------------|        +------------+
                      | Sym  | Type  |        | S    | T   |
                      |--------------|        |------------|
*)

PROCEDURE BuildStaticArray ;
VAR
   combinedTok,
   indexTok,
   arrayTok   : CARDINAL ;
   rw,
   Dim,
   Array,
   Index,
   BackEndType,
   Type, Adr  : CARDINAL ;
BEGIN
   Index := OperandT (1) ;
   indexTok := OperandTtok (1) ;
   Array  := OperandT (2) ;
   arrayTok := OperandTtok (2) ;
   Type := SkipType (OperandF (2)) ;
   rw := OperandMergeRW (2) ;
   Assert (IsLegal (rw)) ;
   Dim := OperandD (2) ;
   INC (Dim) ;
   IF GetMode (Index)=LeftValue
   THEN
      Index := MakeRightValue (indexTok, Index, GetSType (Index))
   END ;
   BuildRange (InitStaticArraySubscriptRangeCheck (GetArraySubscript (Type), Index, Dim)) ;

   (* now make Adr point to the address of the indexed element *)
   combinedTok := MakeVirtualTok (arrayTok, arrayTok, indexTok) ;
   Adr := MakeTemporary (combinedTok, LeftValue) ;
   IF IsVar (Array)
   THEN
      (* BuildDesignatorArray may have detected des is a constant.  *)
      PutVarConst (Adr, IsVarConst (Array))
   END ;
   PutVarArrayRef (Adr, TRUE) ;
   (*
      From now on it must reference the array element by its lvalue
      - so we create the type of the referenced entity
   *)

   BackEndType := MakePointer (combinedTok, NulName) ;
   PutPointer (BackEndType, GetDType (Type)) ;
   (* PutVar(Adr, BackEndType) ; *)
   PutLeftValueFrontBackType (Adr, GetDType (Type), BackEndType) ;

   GenQuadO (combinedTok, ArrayOp, Adr, Index, Array, TRUE) ;
   PopN (2) ;   (* remove all parameters to this procedure *)
   PushTFDrwtok (Adr, GetSType (Adr), Dim, rw, combinedTok)
END BuildStaticArray ;


(*
   calculateMultipicand - generates quadruples which calculate the
                          multiplicand for the array at dimension, dim.
*)

PROCEDURE calculateMultipicand (tok: CARDINAL;
                                arraySym, arrayType: CARDINAL; dim: CARDINAL) : CARDINAL ;
VAR
   ti, tj, tk, tl: CARDINAL ;
BEGIN
   IF dim = GetDimension (arrayType)
   THEN
      (* ti has no type since constant *)
      ti := MakeTemporary (tok, ImmediateValue) ;
      PutVar (ti, Cardinal) ;
      GenQuadO (tok, ElementSizeOp, ti, arrayType, 1, TRUE)
   ELSE
      INC(dim) ;
      tk := MakeTemporary (tok, RightValue) ;
      PutVar (tk, Cardinal) ;
      GenHigh (tok, tk, dim, arraySym) ;
      tl := MakeTemporary (tok, RightValue) ;
      PutVar (tl, Cardinal) ;
      GenQuadOtok (tok, AddOp, tl, tk, MakeConstLit (tok, MakeKey ('1'), Cardinal), TRUE,
                   tok, tok, tok) ;
      tj := calculateMultipicand (tok, arraySym, arrayType, dim) ;
      ti := MakeTemporary (tok, RightValue) ;
      PutVar (ti, Cardinal) ;
      GenQuadO (tok, MultOp, ti, tj, tl, TRUE)
   END ;
   RETURN ti
END calculateMultipicand ;


(*
   ConvertToAddress - convert sym to an address.
*)

PROCEDURE ConvertToAddress (tokpos: CARDINAL; sym: CARDINAL) : CARDINAL ;
VAR
   adr: CARDINAL ;
BEGIN
   IF GetSType (sym) = Address
   THEN
      RETURN sym
   ELSE
      PushTF (RequestSym (tokpos, MakeKey ('CONVERT')), NulSym) ;
      PushT (Address) ;
      PushTtok (sym, tokpos) ;
      PushT(2) ;          (* Two parameters *)
      BuildConvertFunction (Convert, FALSE) ;
      PopT (adr) ;
      RETURN adr
   END
END ConvertToAddress ;


(*
   BuildDynamicArray - Builds the array referencing for dynamic arrays.
                       The Stack is expected to contain:


                       Entry                          Exit
                       =====                          ====

               Ptr ->
                       +-----------------------+
                       | Index                 |                                    <- Ptr
                       |-----------------------|      +---------------------------+
                       | ArraySym | Type | Dim |      | S  | T | ArraySym | Dim+1 |
                       |-----------------------|      |---------------------------|


   if Dim=1
   then
      S := base of ArraySym + TSIZE(Type)*Index
   else
      S := S + TSIZE(Type)*Index
   fi
*)

PROCEDURE BuildDynamicArray ;
VAR
   combinedTok,
   arrayTok,
   indexTok     : CARDINAL ;
   Sym, idx,
   Type, Adr,
   ArraySym,
   BackEndType,
   UnboundedType,
   PtrToBase,
   Base,
   Dim, rw,
   ti, tj, tk,
   tka          : CARDINAL ;
BEGIN
   DisplayStack ;
   Sym  := OperandT (2) ;
   Type := SkipType (OperandF (2)) ;
   arrayTok := OperandTok (2) ;
   indexTok := OperandTok (1) ;
   combinedTok := MakeVirtualTok (arrayTok, arrayTok, indexTok) ;
   Dim := OperandD (2) ;
   rw := OperandMergeRW (2) ;
   Assert (IsLegal (rw)) ;
   INC (Dim) ;
   IF Dim = 1
   THEN
      (*
         Base has type address since
         BuildDesignatorRecord references by address.

         Build a record for retrieving the address of dynamic array.
         BuildDesignatorRecord will generate the required quadruples,
         therefore build sets up the stack for BuildDesignatorRecord
         which will generate the quads to access the record.
      *)
      ArraySym := Sym ;
      UnboundedType := GetUnboundedRecordType (GetSType (Sym)) ;
      PushTFrwtok (Sym, UnboundedType, rw, arrayTok) ;
      PushTF (GetUnboundedAddressOffset (GetSType (Sym)),
              GetSType (GetUnboundedAddressOffset (GetSType (Sym)))) ;
      PushT (1) ;  (* One record field to dereference *)
      BuildDesignatorRecord (combinedTok) ;
      PopT (PtrToBase) ;
      DisplayStack ;
      (* Now actually copy Unbounded.ArrayAddress into base *)
      IF GetMode(PtrToBase) = LeftValue
      THEN
         Base := MakeTemporary (arrayTok, RightValue) ;
         PutVar (Base, Address) ;           (* has type ADDRESS *)
         CheckPointerThroughNil (arrayTok, PtrToBase) ;
         GenQuad (IndrXOp, Base, Address, PtrToBase)           (* Base = *PtrToBase *)
      ELSE
         Assert (GetMode (PtrToBase) # ImmediateValue) ;
         Base := PtrToBase
      END
   ELSE
      (* Base already calculated previously and pushed to stack *)
      UnboundedType := SkipType (OperandF (2)) ;
      Base := Sym ;
      ArraySym := OperandA (2)
   END ;
   Assert (GetSType (Sym) = Type) ;
   ti := calculateMultipicand (indexTok, Sym, Type, Dim) ;
   idx := OperandT (1) ;
   IF IsConst (idx) AND IsConst (ti)
   THEN
      (* tj has no type since constant *)
      tj := MakeTemporary (indexTok, ImmediateValue) ;
      tk := MakeTemporary (indexTok, ImmediateValue) ;
      PutVar (tj, Cardinal) ;
      PutVar (tk, Cardinal)
   ELSE
      (* tj has Cardinal type since we have multiplied array indices *)
      tj := MakeTemporary (indexTok, RightValue) ;
      IF GetSType (idx) # Cardinal
      THEN
         PushTF (RequestSym (indexTok, MakeKey ('CONVERT')), NulSym) ;
         PushT (Cardinal) ;
         PushTtok (idx, indexTok) ;
         PushT(2) ;          (* Two parameters *)
         BuildConvertFunction (Convert, FALSE) ;
         PopT (idx)
      END ;
      PutVar (tj, Cardinal) ;
      tk := MakeTemporary (indexTok, RightValue) ;
      PutVar (tk, Cardinal)
   END ;
   BuildRange (InitDynamicArraySubscriptRangeCheck (ArraySym, idx, Dim)) ;

   PushTtok (tj, indexTok) ;
   PushTtok (idx, indexTok) ;
   BuildAssignmentWithoutBounds (indexTok, FALSE, TRUE) ;

   GenQuad (MultOp, tk, ti, tj) ;
   Adr := MakeTemporary (combinedTok, LeftValue) ;
   PutVarArrayRef (Adr, TRUE) ;
   (*
      Ok must reference by address
      - but we contain the type of the referenced entity
   *)
   BackEndType := MakePointer (combinedTok, NulName) ;
   PutPointer (BackEndType, GetSType (Type)) ;
   (* Create a temporary pointer for addition.  *)
   tka := ConvertToAddress (combinedTok, tk) ;

   IF Dim = GetDimension (Type)
   THEN
      PutLeftValueFrontBackType (Adr, GetSType(Type), BackEndType) ;

      GenQuadOtok (combinedTok, AddOp, Adr, Base, tka, FALSE,
                   combinedTok, combinedTok, combinedTok) ;
      PopN (2) ;
      PushTFADrwtok (Adr, GetSType(Adr), ArraySym, Dim, rw, combinedTok)
   ELSE
      (* more to index *)
      PutLeftValueFrontBackType (Adr, Type, BackEndType) ;

      GenQuadOtok (combinedTok, AddOp, Adr, Base, tka, FALSE,
                   combinedTok, combinedTok, combinedTok) ;
      PopN (2) ;
      PushTFADrwtok (Adr, GetSType(Adr), ArraySym, Dim, rw, combinedTok)
   END
END BuildDynamicArray ;


(*
   DebugLocation -
*)

PROCEDURE DebugLocation (tok: CARDINAL; message: ARRAY OF CHAR) ;
BEGIN
   IF DebugTokPos
   THEN
      WarnStringAt (InitString (message), tok)
   END
END DebugLocation ;


(*
   BuildDesignatorPointer - Builds a pointer reference.
                            The Stack is expected to contain:


                            Entry                   Exit
                            =====                   ====

                    Ptr ->                                           <- Ptr
                            +--------------+        +--------------+
                            | Sym1  | Type1|        | Sym2  | Type2|
                            |--------------|        |--------------|
*)

PROCEDURE BuildDesignatorPointer (ptrtok: CARDINAL) ;
VAR
   combinedtok,
   exprtok    : CARDINAL ;
   rw,
   Sym1, Type1,
   Sym2, Type2: CARDINAL ;
BEGIN
   PopTFrwtok (Sym1, Type1, rw, exprtok) ;
   DebugLocation (exprtok, "expression") ;

   Type1 := SkipType (Type1) ;
   IF Type1 = NulSym
   THEN
      MetaErrorT1 (ptrtok, '{%1ad} has no type and therefore cannot be dereferenced by ^', Sym1)
   ELSIF IsUnknown (Sym1)
   THEN
      MetaError1 ('{%1EMad} is undefined and therefore {%1ad}^ cannot be resolved', Sym1)
   ELSIF IsPointer (Type1)
   THEN
      Type2 := GetSType (Type1) ;
      Sym2 := MakeTemporary (ptrtok, LeftValue) ;
      (*
         Ok must reference by address
         - but we contain the type of the referenced entity
      *)
      MarkAsRead (rw) ;
      PutVarPointerCheck (Sym1, TRUE) ;
      CheckPointerThroughNil (ptrtok, Sym1) ;
      IF GetMode (Sym1) = LeftValue
      THEN
         rw := NulSym ;
         PutLeftValueFrontBackType (Sym2, Type2, Type1) ;
         GenQuadO (ptrtok, IndrXOp, Sym2, Type1, Sym1, FALSE)    (* Sym2 := *Sym1 *)
      ELSE
         PutLeftValueFrontBackType (Sym2, Type2, NulSym) ;
         GenQuadO (ptrtok, BecomesOp, Sym2, NulSym, Sym1, FALSE) (* Sym2 :=  Sym1 *)
      END ;
      PutVarPointerCheck (Sym2, TRUE) ;       (* we should check this for *)
                                     (* Sym2 later on (pointer via NIL)   *)
      combinedtok := MakeVirtualTok (exprtok, exprtok, ptrtok) ;
      PushTFrwtok (Sym2, Type2, rw, combinedtok) ;
      DebugLocation (combinedtok, "pointer expression")
   ELSE
      MetaError2 ('{%1ad} is not a pointer type but a {%2d}', Sym1, Type1)
   END
END BuildDesignatorPointer ;


(*
   StartBuildWith - performs the with statement.
                    The Stack:

                    Entry                    Exit

                    +------------+
                    | Sym | Type |           Empty
                    |------------|
*)

PROCEDURE StartBuildWith (withTok: CARDINAL) ;
VAR
   tok      : CARDINAL ;
   Sym, Type,
   Ref      : CARDINAL ;
BEGIN
   DebugLocation (withtok, "with") ;
   BuildStmtNoteTok (withTok) ;
   DisplayStack ;
   PopTFtok (Sym, Type, tok) ;
   DebugLocation (tok, "expression") ;
   Type := SkipType (Type) ;
   IF Type = NulSym
   THEN
      MetaErrorT1 (tok,
                   '{%1Aa} {%1d} has a no type, the {%kWITH} statement requires a variable or parameter of a {%kRECORD} type',
                   Sym)
   ELSE
      Ref := MakeTemporary (tok, LeftValue) ;
      PutVar (Ref, Type) ;
      IF GetMode (Sym) = LeftValue
      THEN
         (* Copy LeftValue.  *)
         GenQuadO (tok, BecomesOp, Ref, NulSym, Sym, TRUE)
      ELSE
         (* Calculate the address of Sym.  *)
         GenQuadO (tok, AddrOp, Ref, NulSym, Sym, TRUE)
      END ;

      PushWith (Sym, Type, Ref, tok) ;
      DebugLocation (tok, "with ref") ;
      IF NOT IsRecord(Type)
      THEN
         MetaErrorT1 (tok,
                      'the {%kWITH} statement requires that {%1Ea} {%1d} be of a {%kRECORD} {%1tsa:type rather than {%1tsa}}',
                      Sym)
      END ;
      StartScope (Type)
   END ;
   DisplayStack ;
END StartBuildWith ;


(*
   EndBuildWith - terminates the innermost with scope.
*)

PROCEDURE EndBuildWith ;
BEGIN
   DisplayStack ;
   EndScope ;
   PopWith
 ; DisplayStack ;
END EndBuildWith ;


(*
   PushWith - pushes sym and type onto the with stack. It checks for
              previous declaration of this record type.
*)

PROCEDURE PushWith (Sym, Type, Ref, Tok: CARDINAL) ;
VAR
   i, n: CARDINAL ;
   f   : WithFrame ;
BEGIN
   IF Pedantic
   THEN
      n := NoOfItemsInStackAddress(WithStack) ;
      i := 1 ;  (* Top of the stack.  *)
      WHILE i <= n DO
         (* Search for other declarations of the with using Type.  *)
         f := PeepAddress(WithStack, i) ;
         IF f^.RecordSym=Type
         THEN
            MetaErrorT1 (Tok,
                         'cannot have nested {%kWITH} statements referencing the same {%kRECORD} {%1Ead}',
                         Sym) ;
            MetaErrorT1 (f^.RecordTokPos,
                         'cannot have nested {%kWITH} statements referencing the same {%kRECORD} {%1Ead}',
                         f^.RecordSym)
         END ;
         INC (i)
      END
   END ;
   NEW (f) ;
   WITH f^ DO
      RecordSym    := Sym ;
      RecordType   := Type ;
      RecordRef    := Ref ;
      rw           := Sym ;
      RecordTokPos := Tok
   END ;
   PushAddress (WithStack, f)
END PushWith ;


PROCEDURE PopWith ;
VAR
   f: WithFrame ;
BEGIN
   f := PopAddress (WithStack) ;
   DISPOSE (f)
END PopWith ;


(*
   CheckWithReference - performs the with statement.
                        The Stack:

                        Entry                    Exit

                        +------------+           +------------+
                        | Sym | Type |           | Sym | Type |
                        |------------|           |------------|
*)

PROCEDURE CheckWithReference ;
VAR
   f        : WithFrame ;
   tokpos,
   i, n, rw,
   Sym, Type: CARDINAL ;
BEGIN
   n := NoOfItemsInStackAddress(WithStack) ;
   IF (n>0) AND (NOT SuppressWith)
   THEN
      PopTFrwtok (Sym, Type, rw, tokpos) ;
      Assert (tokpos # UnknownTokenNo) ;
      (* inner WITH always has precidence *)
      i := 1 ;  (* top of stack *)
      WHILE i<=n DO
         (* WriteString('Checking for a with') ; *)
         f := PeepAddress (WithStack, i) ;
         WITH f^ DO
            IF IsRecordField (Sym) AND (GetRecord (GetParent (Sym)) = RecordType)
            THEN
               IF IsUnused (Sym)
               THEN
                  MetaError1('record field {%1Dad} was declared as unused by a pragma', Sym)
               END ;
               (* Fake a RecordSym.op *)
               PushTFrwtok (RecordRef, RecordType, rw, RecordTokPos) ;
               PushTFtok (Sym, Type, tokpos) ;
               BuildAccessWithField ;
               PopTFrw (Sym, Type, rw) ;
               i := n+1  (* Finish loop.  *)
            ELSE
               INC (i)
            END
         END
      END ;
      PushTFrwtok (Sym, Type, rw, tokpos)
   END
END CheckWithReference ;


(*
   BuildAccessWithField - similar to BuildDesignatorRecord except it
                          does not perform the address operation.
                          The address will have been computed at the
                          beginning of the WITH statement.
                          It also stops the GenQuad procedure from examining the
                          with stack.

                          The Stack

                          Entry

                   Ptr ->
                          +--------------+
                          | Field | Type1|                          <- Ptr
                          |-------|------|          +-------------+
                          | Adr   | Type2|          | Sym  | Type1|
                          |--------------|          |-------------|
*)

PROCEDURE BuildAccessWithField ;
VAR
   rectok, fieldtok  : CARDINAL ;
   OldSuppressWith   : BOOLEAN ;
   rw,
   Field, FieldType,
   Record, RecordType,
   Ref               : CARDINAL ;
BEGIN
   OldSuppressWith := SuppressWith ;
   SuppressWith := TRUE ;
   (*
      now the WITH cannot look at the stack of outstanding WITH records.
   *)
   PopTFtok (Field, FieldType, fieldtok) ;
   PopTFrwtok (Record, RecordType, rw, rectok) ;

   Ref := MakeComponentRef (MakeComponentRecord (fieldtok,
                                                 RightValue, Record), Field) ;
   PutVarConst (Ref, IsReadOnly (Record)) ;
   GenQuadO (fieldtok,
             RecordFieldOp, Ref, Record, Field, TRUE) ;

   PushTFrwtok (Ref, FieldType, rw, fieldtok) ;
   SuppressWith := OldSuppressWith
END BuildAccessWithField ;


(*
   BuildNulExpression - Builds a nul expression on the stack.
                        The Stack:

                        Entry             Exit

                                                         <- Ptr
                        Empty             +------------+
                                          | NulSym     |
                                          |------------|
   tokpos is the position of the RETURN token.
*)

PROCEDURE BuildNulExpression (tokpos: CARDINAL) ;
BEGIN
   PushTtok (NulSym, tokpos)
END BuildNulExpression ;


(*
   BuildTypeForConstructor - pushes the type implied by the current constructor.
                             If no constructor is currently being built then
                             it Pushes a Bitset type.
*)

PROCEDURE BuildTypeForConstructor (tokpos: CARDINAL) ;
VAR
   c: ConstructorFrame ;
BEGIN
   IF NoOfItemsInStackAddress(ConstructorStack)=0
   THEN
      PushTtok (Bitset, tokpos)
   ELSE
      c := PeepAddress(ConstructorStack, 1) ;
      WITH c^ DO
         IF IsArray (type) OR IsSet (type)
         THEN
            PushTtok (GetSType (type), tokpos)
         ELSIF IsRecord (type)
         THEN
            PushTtok (GetSType (GetNth (type, index)), tokpos)
         ELSE
            MetaError1 ('{%1ad} is not a set, record or array type which is expected when constructing an aggregate entity',
                        type)
         END
      END
   END
END BuildTypeForConstructor ;


(*
   BuildSetStart - Pushes a Bitset type on the stack.

                      The Stack:

                      Entry             Exit

               Ptr ->                                        <- Ptr

                      Empty             +--------------+
                                        | Bitset       |
                                        |--------------|
*)

PROCEDURE BuildSetStart (tokpos: CARDINAL) ;
BEGIN
   PushTtok (Bitset, tokpos)
END BuildSetStart ;


(*
   BuildSetEnd - pops the set value and type from the stack
                 and pushes the value,type pair.

                    Entry                   Exit

             Ptr ->
                    +--------------+
                    | Set Value    |                         <- Ptr
                    |--------------|        +--------------+
                    | Set Type     |        | Value | Type |
                    |--------------|        |--------------|
*)

PROCEDURE BuildSetEnd ;
VAR
   valuepos, typepos,
   combined,
   value, type      : CARDINAL ;
BEGIN
   PopTtok (value, valuepos) ;
   PopTtok (type, typepos) ;
   combined := MakeVirtual2Tok (typepos, valuepos) ;
   PushTFtok (value, type, combined) ;
   Assert (IsSet (type))
END BuildSetEnd ;


(*
   BuildEmptySet - Builds an empty set on the stack.
                   The Stack:

                   Entry             Exit

                                                     <- Ptr
                                     +-------------+
            Ptr ->                   | Value       |
                   +-----------+     |-------------|
      	       	   | SetType   |     | SetType     |
                   |-----------|     |-------------|

   tokpos points to the opening '{'.
*)

PROCEDURE BuildEmptySet (tokpos: CARDINAL) ;
VAR
   n      : Name ;
   typepos,
   Type   : CARDINAL ;
   NulSet : CARDINAL ;
BEGIN
   PopTtok (Type, typepos) ;  (* type of set we are building *)
   IF (Type = NulSym) AND Pim
   THEN
      (* allowed generic {} in PIM Modula-2 *)
      typepos := tokpos
   ELSIF IsUnknown (Type)
   THEN
      n := GetSymName (Type) ;
      WriteFormat1 ('set type %a is undefined', n) ;
      Type := Bitset
   ELSIF NOT IsSet (SkipType (Type))
   THEN
      n := GetSymName (Type) ;
      WriteFormat1('expecting a set type %a', n) ;
      Type := Bitset
   ELSE
      Type := SkipType (Type) ;
      Assert (Type # NulSym)
   END ;
   NulSet := MakeTemporary (typepos, ImmediateValue) ;
   PutVar (NulSet, Type) ;
   PutConstSet (NulSet) ;
   IF CompilerDebugging
   THEN
      n := GetSymName (Type) ;
      printf1 ('set type = %a\n', n)
   END ;
   PushNulSet (Type) ;   (* onto the ALU stack  *)
   PopValue (NulSet) ;   (* ALU -> symbol table *)

   (* and now construct the M2Quads stack as defined by the comments above *)
   PushTtok (Type, typepos) ;
   PushTtok (NulSet, typepos) ;
   IF CompilerDebugging
   THEN
      n := GetSymName (Type) ;
      printf2 ('Type = %a  (%d)  built empty set\n', n, Type) ;
      DisplayStack    (* Debugging info *)
   END
END BuildEmptySet ;


(*
   BuildInclRange - includes a set range with a set.


                          Entry                   Exit
                          =====                   ====


                   Ptr ->
                          +------------+
                          | El2        |
                          |------------|
                          | El1        |                                 <- Ptr
                          |------------|           +-------------------+
                          | Set Value  |           | Value + {El1..El2}|
                          |------------|           |-------------------|

                   No quadruples produced as the range info is contained within
                   the set value.
*)

PROCEDURE BuildInclRange ;
VAR
   n       : Name ;
   el1, el2,
   value   : CARDINAL ;
BEGIN
   PopT(el2) ;
   PopT(el1) ;
   PopT(value) ;
   IF NOT IsConstSet(value)
   THEN
      n := GetSymName(el1) ;
      WriteFormat1('can only add bit ranges to a constant set, %a is not a constant set', n)
   END ;
   IF IsConst(el1) AND IsConst(el2)
   THEN
      PushValue(value) ;  (* onto ALU stack *)
      AddBitRange(GetTokenNo(), el1, el2) ;
      PopValue(value)     (* ALU -> symboltable *)
   ELSE
      IF NOT IsConst(el1)
      THEN
         n := GetSymName(el1) ;
         WriteFormat1('must use constants as ranges when defining a set constant, problem with the low value %a', n)
      END ;
      IF NOT IsConst(el2)
      THEN
         n := GetSymName(el2) ;
         WriteFormat1('must use constants as ranges when defining a set constant, problem with the high value %a', n)
      END
   END ;
   PushT(value)
END BuildInclRange ;


(*
   BuildInclBit - includes a bit into the set.

                         Entry                   Exit
                         =====                   ====


                  Ptr ->
                         +------------+
                         | Element    |                         <- Ptr
                         |------------|          +------------+
                         | Value      |          | Value      |
                         |------------|          |------------|

*)

PROCEDURE BuildInclBit ;
VAR
   tok         : CARDINAL ;
   el, value, t: CARDINAL ;
BEGIN
   PopT(el) ;
   PopT(value) ;
   tok := GetTokenNo () ;
   IF IsConst(el)
   THEN
      PushValue(value) ;  (* onto ALU stack *)
      AddBit(tok, el) ;
      PopValue(value)    (* ALU -> symboltable *)
   ELSE
      IF GetMode(el)=LeftValue
      THEN
         t := MakeTemporary(tok, RightValue) ;
         PutVar(t, GetSType(el)) ;
         CheckPointerThroughNil (tok, el) ;
         doIndrX(tok, t, el) ;
         el := t
      END ;
      IF IsConst(value)
      THEN
         (* move constant into a variable to achieve the include *)
         t := MakeTemporary(tok, RightValue) ;
         PutVar(t, GetSType(value)) ;
         GenQuad(BecomesOp, t, NulSym, value) ;
         value := t
      END ;
      GenQuad(InclOp, value, NulSym, el)
   END ;
   PushT(value)
END BuildInclBit ;


(*
   PushConstructor -
*)

PROCEDURE PushConstructor (sym: CARDINAL) ;
VAR
   c: ConstructorFrame ;
BEGIN
   NEW(c) ;
   WITH c^ DO
      type := SkipType(sym) ;
      index := 1
   END ;
   PushAddress(ConstructorStack, c)
END PushConstructor ;


(*
   PopConstructor - removes the top constructor from the top of stack.
*)

PROCEDURE PopConstructor ;
VAR
   c: ConstructorFrame ;
BEGIN
   c := PopAddress (ConstructorStack) ;
   DISPOSE(c)
END PopConstructor ;


(*
   NextConstructorField - increments the top of constructor stacks index by one.
*)

PROCEDURE NextConstructorField ;
VAR
   c: ConstructorFrame ;
BEGIN
   c := PeepAddress(ConstructorStack, 1) ;
   INC(c^.index)
END NextConstructorField ;


(*
   SilentBuildConstructor - places NulSym into the constructor fifo queue.
*)

PROCEDURE SilentBuildConstructor ;
BEGIN
   PutConstructorIntoFifoQueue (NulSym)
END SilentBuildConstructor ;


(*
   BuildConstructor - builds a constructor.
                      Stack

                      Entry                 Exit

               Ptr ->
                      +------------+
                      | Type       |                <- Ptr
                      |------------+
*)

PROCEDURE BuildConstructor (tokcbrpos: CARDINAL) ;
VAR
   tok       : CARDINAL ;
   constValue,
   type      : CARDINAL ;
BEGIN
   PopTtok (type, tok) ;
   constValue := MakeTemporary (tok, ImmediateValue) ;
   PutVar (constValue, type) ;
   PutConstructor (constValue) ;
   PushValue (constValue) ;
   IF type = NulSym
   THEN
      MetaErrorT0 (tokcbrpos,
                   '{%E}constructor requires a type before the opening %{')
   ELSE
      ChangeToConstructor (tok, type) ;
      PutConstructorFrom (constValue, type) ;
      PopValue (constValue) ;
      PutConstructorIntoFifoQueue (constValue)
   END ;
   PushConstructor (type)
END BuildConstructor ;


(*
   SilentBuildConstructorStart - removes an entry from the constructor fifo queue.
*)

PROCEDURE SilentBuildConstructorStart ;
VAR
   constValue: CARDINAL ;
BEGIN
   GetConstructorFromFifoQueue (constValue)
END SilentBuildConstructorStart ;


(*
   BuildConstructorStart - builds a constructor.
                           Stack

                           Entry                 Exit

                    Ptr ->                                          <- Ptr
                           +------------+        +----------------+
                           | Type       |        | ConstructorSym |
                           |------------+        |----------------|
*)

PROCEDURE BuildConstructorStart (cbratokpos: CARDINAL) ;
VAR
   typepos,
   constValue,
   type      : CARDINAL ;
BEGIN
   PopTtok (type, typepos) ;   (* we ignore the type as we already have the constructor symbol from pass C *)
   GetConstructorFromFifoQueue (constValue) ;
   IF type # GetSType (constValue)
   THEN
      MetaErrorT3 (cbratokpos,
                   '{%E}the constructor type is {%1ad} and this is different from the constant {%2ad} which has a type {%2tad}',
                   type, constValue, constValue)
   END ;
   PushTtok (constValue, cbratokpos) ;
   PushConstructor (type)
END BuildConstructorStart ;


(*
   BuildConstructorEnd - removes the current constructor frame from the
                         constructor stack (it does not effect the quad
                         stack)

                         Entry                 Exit

                  Ptr ->                                      <- Ptr
                         +------------+        +------------+
                         | const      |        | const      |
                         |------------|        |------------|

   startpos is the start of the constructor, either the typename or '{'
   cbratokpos is the '}'.
*)

PROCEDURE BuildConstructorEnd (startpos, cbratokpos: CARDINAL) ;
VAR
   value, valtok: CARDINAL ;
BEGIN
   IF DebugTokPos
   THEN
      WarnStringAt (InitString ('startpos'), startpos) ;
      WarnStringAt (InitString ('cbratokpos'), cbratokpos)
   END ;
   PopTtok (value, valtok) ;
   IF DebugTokPos
   THEN
      WarnStringAt (InitString ('value valtok'), valtok)
   END ;
   valtok := MakeVirtual2Tok (startpos, cbratokpos) ;
   PutDeclared (valtok, value) ;
   PushTtok (value, valtok) ;   (* Use valtok as we now know it was a constructor.  *)
   PopConstructor ;
   IF DebugTokPos
   THEN
      WarnStringAt (InitString ('aggregate constant'), valtok)
   END
END BuildConstructorEnd ;


(*
   AddFieldTo - adds field, e, to, value.
*)

PROCEDURE AddFieldTo (value, e: CARDINAL) : CARDINAL ;
BEGIN
   IF IsSet(GetDType(value))
   THEN
      PutConstSet(value) ;
      PushT(value) ;
      PushT(e) ;
      BuildInclBit ;
      PopT(value)
   ELSE
      PushValue(value) ;
      AddField(GetTokenNo(), e) ;
      PopValue(value)
   END ;
   RETURN( value )
END AddFieldTo ;


(*
   BuildComponentValue -  builds a component value.

                          Entry                 Exit

                   Ptr ->                                      <- Ptr


                          +------------+        +------------+
                          | const      |        | const      |
                          |------------|        |------------|
*)

PROCEDURE BuildComponentValue ;
VAR
   const,
   e1, e2   : CARDINAL ;
   nuldotdot,
   nulby    : Name ;
BEGIN
   PopT(nulby) ;
   IF nulby=NulTok
   THEN
      PopT(nuldotdot) ;
      IF nuldotdot=NulTok
      THEN
         PopT(e1) ;
         PopT(const) ;
         PushT(AddFieldTo(const, e1))
      ELSE
         PopT(e2) ;
         PopT(e1) ;
         PopT(const) ;
         PushValue(const) ;
         AddBitRange(GetTokenNo(), e1, e2) ;
         PopValue(const) ;
         PushT(const)
      END
   ELSE
      PopT(e1) ;
      PopT(nuldotdot) ;
      IF nuldotdot=NulTok
      THEN
         PopT(e2) ;
         PopT(const) ;
         PushValue(const) ;
         AddElements(GetTokenNo(), e2, e1) ;
         PopValue(const) ;
         PushT(const)
      ELSE
         PopT(e2) ;
         PopT(e1) ;
         PopT(const) ;
         WriteFormat0('the constant must be either an array constructor or a set constructor') ;
         PushT(const)
      END
   END
END BuildComponentValue ;


(*
   RecordOp - Records the operator passed on the stack.
              This is called when a boolean operator is found in an
              expression.  It is called just after the lhs has been built
              and pushed to the quad stack and prior to the rhs build.
              It checks to see if AND OR or equality tests are required.
              It will short circuit AND and OR expressions.  It also
              converts a lhs to a boolean variable if an xor comparison
              is about to be performed.

              Checks for AND operator or OR operator
              if either of these operators are found then BackPatching
              takes place.
              The Expected Stack:

              Entry                        Exit

       Ptr ->                                               <- Ptr
              +-------------+               +-------------+
              | OperatorTok |               | OperatorTok |
              |-------------|               |-------------|
              | t    | f    |               | t    | f    |
              |-------------|               |-------------|


              If OperatorTok=AndTok
              Then
                 BackPatch(f, NextQuad)
              Elsif OperatorTok=OrTok
              Then
                 BackPatch(t, NextQuad)
              End
*)

PROCEDURE RecordOp ;
VAR
   Op   : Name ;
   tokno: CARDINAL ;
   t, f : CARDINAL ;
BEGIN
   PopTtok(Op, tokno) ;
   IF (Op=AndTok) OR (Op=AmbersandTok)
   THEN
      CheckBooleanId ;
      PopBool(t, f) ;
      BackPatch(t, NextQuad) ;
      PushBool(0, f)
   ELSIF Op=OrTok
   THEN
      CheckBooleanId ;
      PopBool(t, f) ;
      BackPatch(f, NextQuad) ;
      PushBool(t, 0)
   ELSIF IsBoolean (1) AND
      ((Op = EqualTok) OR (Op = LessGreaterTok) OR (Op = HashTok) OR (Op = InTok))
   THEN
      ConvertBooleanToVariable (tokno, 1)
   END ;
   PushTtok(Op, tokno)
END RecordOp ;


(*
   CheckLogicalOperator - returns a logical operator if the operands imply
                          a logical operation should be performed.
*)

PROCEDURE CheckLogicalOperator (Tok: Name; left, lefttype: CARDINAL) : Name ;
BEGIN
   IF (Tok=PlusTok) OR (Tok=TimesTok) OR (Tok=DivideTok) OR (Tok=MinusTok)
   THEN
      (* --fixme-- when we add complex arithmetic, we must check constructor is not a complex constant.  *)
      IF ((lefttype#NulSym) AND IsSet(SkipType(lefttype))) OR
         IsConstSet(left) OR IsConstructor(left)
      THEN
         IF Tok=PlusTok
         THEN
            RETURN( LogicalOrTok )
         ELSIF Tok=DivideTok
         THEN
            RETURN( LogicalXorTok )
         ELSIF Tok=TimesTok
         THEN
            RETURN( LogicalAndTok )
         ELSIF Tok=MinusTok
         THEN
            RETURN( LogicalDifferenceTok )
         END
      END
   END ;
   RETURN( Tok )
END CheckLogicalOperator ;


(*
   doCheckGenericNulSet - checks to see whether e1 is a generic nul set and if so it alters it
                          to the nul set of t2.
*)

(*
PROCEDURE doCheckGenericNulSet (e1: CARDINAL; VAR t1: CARDINAL; t2: CARDINAL) ;
BEGIN
   IF IsConstSet (e1)
   THEN
      IF NOT IsSet (t2)
      THEN
         MetaError2 ('incompatibility between a set constant {%1Ea} of type {%1tsa} and an object of type {%2sa}',
                     e1, t2)
      END ;
      PushValue (e1) ;
      IF IsGenericNulSet ()
      THEN
         PopValue (e1) ;
         PushNulSet (t2) ;
         t1 := t2
      END ;
      PopValue (e1)
   END
END doCheckGenericNulSet ;
*)


(*
   CheckGenericNulSet - if e1 or e2 is the generic nul set then
                        alter it to the nul set of the other operands type.
*)

(*
PROCEDURE CheckGenericNulSet (e1, e2: CARDINAL; VAR t1, t2: CARDINAL) ;
BEGIN
   IF t1#t2
   THEN
      doCheckGenericNulSet(e1, t1, t2) ;
      doCheckGenericNulSet(e2, t2, t1)
   END
END CheckGenericNulSet ;
*)


(*
   CheckDivModRem - initiates calls to check the divisor for DIV, MOD, REM
                    expressions.
*)

PROCEDURE CheckDivModRem (TokPos: CARDINAL; tok: Name; d, e: CARDINAL) ;
BEGIN
   IF tok=DivTok
   THEN
      BuildRange (InitWholeZeroDivisionCheck (TokPos, d, e))
   ELSIF tok=ModTok
   THEN
      BuildRange (InitWholeZeroDivisionCheck (TokPos, d, e))
   ELSIF tok=RemTok
   THEN
      BuildRange (InitWholeZeroRemainderCheck (TokPos, d, e))
   END
END CheckDivModRem ;


(*
   doConvert - convert, sym, to a new symbol with, type.
               Return the new symbol.
*)

PROCEDURE doConvert (type: CARDINAL; sym: CARDINAL) : CARDINAL ;
BEGIN
   IF GetSType(sym)#type
   THEN
      PushTF(Convert, NulSym) ;
      PushT(type) ;
      PushT(sym) ;
      PushT(2) ;          (* Two parameters *)
      BuildConvertFunction (Convert, FALSE) ;
      PopT(sym)
   END ;
   RETURN( sym )
END doConvert ;


(*
   BuildBinaryOp   - Builds a binary operation from the quad stack.
                     Be aware that this procedure will check for
                     the overloading of the bitset operators + - \ *.
                     So do NOT call this procedure if you are building
                     a reference to an array which has a bitset type or
                     the address arithmetic will be wrongly coersed into
                     logical ORs.

                     The Stack is expected to contain:


                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | Sym1       |
                     |------------|
                     | Operator   |                          <- Ptr
                     |------------|          +------------+
                     | Sym2       |          | Temporary  |
                     |------------|          |------------|


                     Quadruples Produced

                     q     Operator  Temporary  Sym1  Sym2


                OR


                     Entry                   Exit
                     =====                   ====

              Ptr ->
                     +------------+
                     | T1   | F1  |
                     |------------|
                     | OrTok      |                          <- Ptr
                     |------------|          +------------+
                     | T2   | F2  |          | T1+T2| F1  |
                     |------------|          |------------|


                     Quadruples Produced

*)

PROCEDURE BuildBinaryOp ;
BEGIN
   doBuildBinaryOp (TRUE, TRUE)
END BuildBinaryOp ;


(*
   doBuildBinaryOp - build the binary op, with or without type
                     checking.
*)

PROCEDURE doBuildBinaryOp (checkTypes, checkOverflow: BOOLEAN) ;
VAR
   s                  : String ;
   NewOp,
   Operator           : Name ;
   OperatorPos,
   OldPos,
   leftrw, rightrw,
   t1, f1,
   t2, f2,
   lefttype, righttype,
   left, right,
   leftpos, rightpos  : CARDINAL ;
   value              : CARDINAL ;
BEGIN
   Operator := OperandT (2) ;
   IF Operator = OrTok
   THEN
      CheckBooleanId ;
      PopBooltok (t1, f1, rightpos) ;
      PopTtok (Operator, OperatorPos) ;
      PopBooltok (t2, f2, leftpos) ;
      Assert (f2=0) ;
      OperatorPos := MakeVirtualTok (OperatorPos, leftpos, rightpos) ;
      PushBooltok (Merge (t1, t2), f1, OperatorPos)
   ELSIF (Operator = AndTok) OR (Operator = AmbersandTok)
   THEN
      CheckBooleanId ;
      PopBooltok (t1, f1, rightpos) ;
      PopTtok (Operator, OperatorPos) ;
      PopBooltok (t2, f2, leftpos) ;
      Assert (t2=0) ;
      OperatorPos := MakeVirtualTok (OperatorPos, leftpos, rightpos) ;
      PushBooltok (t1, Merge (f1, f2), OperatorPos)
   ELSE
      PopTFrwtok (right, righttype, rightrw, rightpos) ;
      PopTtok (Operator, OperatorPos) ;
      PopTFrwtok (left, lefttype, leftrw, leftpos) ;
      MarkAsRead (rightrw) ;
      MarkAsRead (leftrw) ;
      NewOp := CheckLogicalOperator (Operator, (* right, righttype, *) left, lefttype) ;
      IF NewOp = Operator
      THEN
         (*
            BinaryOps and UnaryOps only work with immediate and
            offset addressing.  This is fine for calculating
            array and record offsets but we need to get the real
            values to perform normal arithmetic. Not address
            arithmetic.

            However the set operators will dereference LValues
            (to optimize large set arithemetic)
         *)
         IF GetMode (right) = LeftValue
         THEN
            value := MakeTemporary (rightpos, RightValue) ;
            PutVar (value, righttype) ;
            CheckPointerThroughNil (rightpos, right) ;
            doIndrX (rightpos, value, right) ;
            right := value
         END ;
         IF GetMode (left) = LeftValue
         THEN
            value := MakeTemporary (leftpos, RightValue) ;
            PutVar (value, lefttype) ;
            CheckPointerThroughNil (leftpos, left) ;
            doIndrX (leftpos, value, left) ;
            left := value
         END
      ELSE
         (* CheckForGenericNulSet(e1, e2, t1, t2) *)
      END ;
      OldPos := OperatorPos ;
      OperatorPos := MakeVirtualTok (OperatorPos, leftpos, rightpos) ;
      IF (Operator = PlusTok) AND IsConstString(left) AND IsConstString(right)
      THEN
         value := MakeConstString (OperatorPos, NulName) ;
         PutConstStringKnown (OperatorPos, value, NulName, FALSE, FALSE) ;
         GenQuadOtok (OperatorPos, MakeOp (PlusTok), value, left, right, FALSE,
                      OperatorPos, leftpos, rightpos)
      ELSE
         IF checkTypes
         THEN
            BuildRange (InitTypesExpressionCheck (OperatorPos, left, right, FALSE, FALSE))
         END ;
         value := MakeTemporaryFromExpressions (OperatorPos,
                                                right, left,
                                                AreConstant (IsConst (left) AND IsConst (right))) ;

         CheckDivModRem (OperatorPos, NewOp, value, right) ;

         IF DebugTokPos
         THEN
            s := InitStringCharStar (KeyToCharStar (GetTokenName (Operator))) ;
            WarnStringAt (s, OldPos) ;
            s := InitString ('left') ;
            WarnStringAt (s, leftpos) ;
            s := InitString ('right') ;
            WarnStringAt (s, rightpos) ;
            s := InitString ('caret') ;
            WarnStringAt (s, OldPos) ;
            s := InitString ('combined') ;
            WarnStringAt (s, OperatorPos) ;
            (* MetaErrorT1 (GetDeclaredMod (t), 'in binary with a {%1a}', t) *)
         END ;
         GenQuadOtok (OperatorPos, MakeOp (NewOp), value, left, right, checkOverflow,
                      OperatorPos, leftpos, rightpos)
      END ;
      PushTFtok (value, GetSType (value), OperatorPos)
   END
END doBuildBinaryOp ;


(*
   BuildUnaryOp   - Builds a unary operation from the quad stack.
                    The Stack is expected to contain:


                    Entry                   Exit
                    =====                   ====

             Ptr ->
                    +------------+
                    | Sym        |
                    |------------|          +------------+
                    | Operator   |          | Temporary  | <- Ptr
                    |------------|          |------------|


                    Quadruples Produced

                    q     Operator  Temporary  _ Sym

*)

PROCEDURE BuildUnaryOp ;
VAR
   sympos,
   tokpos    : CARDINAL ;
   Tok       : Name ;
   type,
   Sym,
   SymT, r, t: CARDINAL ;
BEGIN
   PopTrwtok (Sym, r, sympos) ;
   PopTtok (Tok, tokpos) ;
   IF Tok=MinusTok
   THEN
      MarkAsRead(r) ;
      type := NegateType (GetSType (Sym) (* , sympos *) ) ;
      tokpos := MakeVirtualTok (tokpos, tokpos, sympos) ;

      t := MakeTemporary (tokpos, AreConstant(IsConst(Sym))) ;
      PutVar(t, type) ;

      (*
         variables must have a type and REAL/LONGREAL constants must
         be typed
      *)

      IF NOT IsConst(Sym)
      THEN
         IF (type#NulSym) AND IsSet(SkipType(type))
         THEN
            (* do not dereference set variables *)
         ELSIF GetMode(Sym)=LeftValue
         THEN
            (* dereference symbols which are not sets and which are variables *)

            SymT := MakeTemporary (sympos, RightValue) ;
            PutVar (SymT, GetSType (Sym)) ;
            CheckPointerThroughNil (sympos, Sym) ;
            doIndrX (sympos, SymT, Sym) ;
            Sym := SymT
         END
      END ;
      GenQuadO (tokpos, NegateOp, t, NulSym, Sym, TRUE) ;
      PushTtok (t, tokpos)
   ELSIF Tok=PlusTok
   THEN
      tokpos := MakeVirtualTok (tokpos, tokpos, sympos) ;
      PushTrwtok (Sym, r, tokpos)
   ELSE
      MetaErrorNT1 (tokpos,
                    'expecting an unary operator, seen {%Ek%a}', Tok)
   END
END BuildUnaryOp ;


(*
   AreConstant - returns immediate addressing mode if b is true else
                 offset mode is returned. b determines whether the
                 operands are all constant - in which case we can use
                 a constant temporary variable.
*)

PROCEDURE AreConstant (b: BOOLEAN) : ModeOfAddr ;
BEGIN
   IF b
   THEN
      RETURN ImmediateValue
   ELSE
      RETURN RightValue
   END
END AreConstant ;


(*
   ConvertBooleanToVariable - converts a BoolStack(i) from a Boolean True|False
                              exit pair into a variable containing the value TRUE or
                              FALSE.  The parameter i is relative to the top
                              of the stack.
*)

PROCEDURE ConvertBooleanToVariable (tok: CARDINAL; i: CARDINAL) ;
VAR
   Des: CARDINAL ;
   f  : BoolFrame ;
BEGIN
   Assert (IsBoolean (i)) ;
   (* We need to convert the boolean top of stack into a variable or
      constant boolean.  *)
   Des := MakeTemporary (tok, AreConstant (IsInConstExpression ())) ;
   PutVar (Des, Boolean) ;
   PutVarConditional (Des, TRUE) ;
   PushTtok (Des, tok) ;   (* we have just increased the stack so we must use i+1 *)
   f := PeepAddress (BoolStack, i+1) ;
   PushBool (f^.TrueExit, f^.FalseExit) ;
   BuildAssignmentWithoutBounds (tok, FALSE, TRUE) ;
   (* Restored stack after the BuildAssign... above.  *)
   f := PeepAddress (BoolStack, i) ;
   WITH f^ DO
      TrueExit := Des ;  (* Alter Stack(i) to contain the variable.  *)
      FalseExit := Boolean ;
      BooleanOp := FALSE ; (* No longer a Boolean True|False pair.  *)
      Unbounded := NulSym ;
      Dimension := 0 ;
      ReadWrite := NulSym ;
      tokenno := tok ;
      Annotation := KillString (Annotation) ;
      Annotation := InitString ('%1s(%1d)|%2s(%2d)||boolean var|type')
   END
END ConvertBooleanToVariable ;


(*
   BuildBooleanVariable - tests to see whether top of stack is a boolean
                          conditional and if so it converts it into a boolean
                          variable.
*)

PROCEDURE BuildBooleanVariable ;
BEGIN
   IF IsBoolean (1)
   THEN
      ConvertBooleanToVariable (OperandTtok (1), 1)
   END
END BuildBooleanVariable ;


(*
   DumpQuadSummary -
*)

PROCEDURE DumpQuadSummary (quad: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   IF quad # 0
   THEN
      f := GetQF (quad) ;
      printf2 ("%d  op3 = %d\n", quad, f^.Operand3)
   END
END DumpQuadSummary ;



(*
   BuildRelOpFromBoolean - builds a relational operator sequence of quadruples
                           instead of using a temporary boolean variable.
                           This function can only be used when we perform
                           the following translation:

                           (a=b) # (c=d)  alternatively   (a=b) = (c=d)
                                 ^                              ^

                           it only allows # = to be used as >= <= > < all
                           assume a particular value for TRUE and FALSE.
                           (In which case the user should specify ORD)


                           before

                           q      if r1      op1     op2     t2
                           q+1    Goto                       f2
                           ...
                           q+n    if r2      op3     op4     t1
                           q+n+1  Goto                       f1

                           after (in case of =)

                           q    if r1      op1     op2     q+2
                           q+1  Goto                       q+4
                           q+2  if r2      op3     op4     t
                           q+3  Goto                       f
                           q+4  if r2      op3     op4     f
                           q+5  Goto                       t

                           after (in case of #)

                           q      if r1      op1     op2     q+2
                           q+1    Goto                       q+n+2
                           q+2    ...
                           ...    ...
                           q+n    if r2      op3     op4     f
                           q+n+1  Goto                       t
                           q+n+2  if r2      op3     op4     t
                           q+n+3  Goto                       f

                           The Stack is expected to contain:


                           Entry                   Exit
                           =====                   ====

                    Ptr ->
                           +------------+
                           | t1 | f1    |
                           |------------|
                           | Operator   |                          <- Ptr
                           |------------|          +------------+
                           | t2 | f2    |          | t    | f   |
                           |------------|          |------------|


*)

PROCEDURE BuildRelOpFromBoolean (tokpos: CARDINAL) ;
VAR
   Tok,
   t1, f1,
   t2, f2: CARDINAL ;
   f     : QuadFrame ;
BEGIN
   Assert (IsBoolean (1) AND IsBoolean (3)) ;
   IF OperandT (2) = EqualTok
   THEN
      (* Are the two boolean expressions the same?  *)
      PopBool (t1, f1) ;
      PopT (Tok) ;
      PopBool (t2, f2) ;
      (* Give the false exit a second chance.  *)
      BackPatch (t2, t1) ;        (* q    if   _     _    q+2 *)
      BackPatch (f2, NextQuad) ;  (* q+1  if   _     _    q+4 *)
      Assert (NextQuad = f1+1) ;
      f := GetQF (t1) ;
      WITH f^ DO
         GenQuadO (tokpos, Operator, Operand1, Operand2, 0, FALSE)
      END ;
      GenQuadO (tokpos, GotoOp, NulSym, NulSym, 0, FALSE) ;
      PushBooltok (Merge (NextQuad-1, t1), Merge (NextQuad-2, f1), tokpos)
   ELSIF (OperandT (2) = HashTok) OR (OperandT (2) = LessGreaterTok)
   THEN
      IF CompilerDebugging
      THEN
         printf0 ("BuildRelOpFromBoolean (NotEqualTok)\n") ;
         DisplayStack
      END ;
      (* Are the two boolean expressions different?  *)
      PopBool (t1, f1) ;
      PopT (Tok) ;
      PopBool (t2, f2) ;
      IF CompilerDebugging
      THEN
         printf2 ("t1 = %d, f1 = %d\n", t1, f1) ;
         printf2 ("t2 = %d, f2 = %d\n", t2, f2) ;
         DumpQuadSummary (t1) ;
         DumpQuadSummary (f1) ;
         DumpQuadSummary (t2) ;
         DumpQuadSummary (f2) ;
      END ;
      (* Give the false exit a second chance.  *)
      BackPatch (t2, t1) ;        (* q    if   _     _    q+2 *)
      BackPatch (f2, NextQuad) ;  (* q+1  if   _     _    q+4 *)
      Assert (NextQuad = f1+1) ;
      f := GetQF (t1) ;
      WITH f^ DO
         GenQuadO (tokpos, Operator, Operand1, Operand2, 0, FALSE)
      END ;
      GenQuadO (tokpos, GotoOp, NulSym, NulSym, 0, FALSE) ;
      PushBooltok (Merge (NextQuad-2, f1), Merge (NextQuad-1, t1), tokpos)
   ELSE
      MetaError0 ('only allowed to use the relation operators {%Ek=} {%Ek#} rather than {%Ek<} or {%Ek>} on {%EkBOOLEAN} expressions as these do not imply an ordinal value for {%kTRUE} or {%kFALSE}')
   END
END BuildRelOpFromBoolean ;


(*
   CheckVariableOrConstantOrProcedure - checks to make sure sym is a variable, constant or procedure.
*)

PROCEDURE CheckVariableOrConstantOrProcedure (tokpos: CARDINAL; sym: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetSType (sym) ;
   IF IsUnknown (sym)
   THEN
      MetaErrorT1 (tokpos, '{%1EUad} has not been declared', sym) ;
      UnknownReported (sym)
   ELSIF IsPseudoSystemFunction (sym) OR IsPseudoBaseFunction (sym)
   THEN
      MetaErrorT1 (tokpos,
                   '{%1Ead} expected a variable, procedure, constant or expression, not an intrinsic procedure function',
                   sym)
   ELSIF (NOT IsConst(sym)) AND (NOT IsVar(sym)) AND
         (NOT IsProcedure(sym)) AND
         (NOT IsTemporary(sym)) AND (NOT MustNotCheckBounds)
   THEN
      MetaErrorsT1 (tokpos,
                    '{%1Ead} expected a variable, procedure, constant or expression',
                    'and it was declared as a {%1Dd}', sym) ;
   ELSIF (type#NulSym) AND IsArray(type)
   THEN
      MetaErrorsT1 (tokpos,
                    '{%1EU} not expecting an array variable as an operand for either comparison or binary operation',
                    'it was declared as a {%1Dd}', sym)
   ELSIF IsConstString (sym) AND IsConstStringKnown (sym) AND (GetStringLength (tokpos, sym) > 1)
   THEN
      MetaErrorT1 (tokpos,
                   '{%1EU} not expecting a string constant as an operand for either comparison or binary operation',
                   sym)
   END
END CheckVariableOrConstantOrProcedure ;


(*
   BuildRelOp   - Builds a relative operation from the quad stack.
                  The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

           Ptr ->
                  +------------+
                  | e1         |
                  |------------|                          <- Ptr
                  | Operator   |
                  |------------|          +------------+
                  | e2         |          | t    | f   |
                  |------------|          |------------|


                    Quadruples Produced

                    q     IFOperator  e2  e1  TrueExit    ; e2  e1 since
                    q+1   GotoOp              FalseExit   ; relation > etc
                                                          ; requires order.
*)

PROCEDURE BuildRelOp (optokpos: CARDINAL) ;
VAR
   combinedTok,
   rightpos,
   leftpos            : CARDINAL ;
   Op                 : Name ;
   t,
   rightType, leftType,
   right, left        : CARDINAL ;
   s                  : String ;
BEGIN
   IF CompilerDebugging
   THEN
      DisplayStack    (* Debugging info *)
   END ;
   IF IsInConstExpression () AND IsBoolean (1) AND IsBoolean (3)
   THEN
      (*
         we allow # and = to be used with Boolean expressions.
         we do not allow >  <  >=  <=  though.  We only examine
         this case if we are in a const expression as there will be
         no dereferencing of operands.
      *)
      BuildRelOpFromBoolean (optokpos)
   ELSE
      IF IsBoolean (1)
      THEN
         ConvertBooleanToVariable (OperandTtok (1), 1)
      END ;
      IF IsBoolean (3)
      THEN
         ConvertBooleanToVariable (OperandTtok (3), 3)
      END ;
      PopTFtok (right, rightType, rightpos) ;
      PopT (Op) ;
      PopTFtok (left, leftType, leftpos) ;

      CheckVariableOrConstantOrProcedure (rightpos, right) ;
      CheckVariableOrConstantOrProcedure (leftpos, left) ;
      combinedTok := MakeVirtualTok (optokpos, leftpos, rightpos) ;

      IF (left#NulSym) AND (right#NulSym)
      THEN
         (* BuildRange will check the expression later on once gcc knows about all data types.  *)
         BuildRange (InitTypesExpressionCheck (combinedTok, left, right, TRUE,
                                               Op = InTok))
      END ;

      (* Must dereference LeftValue operands.  *)
      IF GetMode(right) = LeftValue
      THEN
         t := MakeTemporary (rightpos, RightValue) ;
         PutVar(t, GetSType(right)) ;
         CheckPointerThroughNil (rightpos, right) ;
         doIndrX (rightpos, t, right) ;
         right := t
      END ;
      IF GetMode(left) = LeftValue
      THEN
         t := MakeTemporary (leftpos, RightValue) ;
         PutVar (t, GetSType (left)) ;
         CheckPointerThroughNil (leftpos, left) ;
         doIndrX (leftpos, t, left) ;
         left := t
      END ;

      IF DebugTokPos
      THEN
         s := InitStringCharStar (KeyToCharStar (GetTokenName (Op))) ;
         WarnStringAt (s, optokpos) ;
         s := InitString ('left') ;
         WarnStringAt (s, leftpos) ;
         s := InitString ('right') ;
         WarnStringAt (s, rightpos) ;
         s := InitString ('caret') ;
         WarnStringAt (s, optokpos) ;
         s := InitString ('combined') ;
         WarnStringAt (s, combinedTok)
      END ;

      GenQuadOtok (combinedTok, MakeOp (Op), left, right, 0, FALSE,
                   leftpos, rightpos, UnknownTokenNo) ;  (* True  Exit *)
      GenQuadO (combinedTok, GotoOp, NulSym, NulSym, 0, FALSE) ;  (* False Exit *)
      PushBooltok (NextQuad-2, NextQuad-1, combinedTok)
   END
END BuildRelOp ;


(*
   BuildNot   - Builds a NOT operation from the quad stack.
                The Stack is expected to contain:


                  Entry                   Exit
                  =====                   ====

           Ptr ->                                        <- Ptr
                  +------------+          +------------+
                  | t    | f   |          | f    | t   |
                  |------------|          |------------|
*)

PROCEDURE BuildNot (notTokPos: CARDINAL) ;
VAR
   combinedTok,
   exprTokPos : CARDINAL ;
   t, f       : CARDINAL ;
BEGIN
   CheckBooleanId ;
   PopBooltok (t, f, exprTokPos) ;
   combinedTok := MakeVirtualTok (notTokPos, notTokPos, exprTokPos) ;
   PushBooltok (f, t, combinedTok)
END BuildNot ;


(*
   MakeOp - returns the equalent quadruple operator to a token, t.
*)

PROCEDURE MakeOp (t: Name) : QuadOperator ;
BEGIN
   IF t=ArithPlusTok
   THEN
      RETURN ArithAddOp
   ELSIF t=PlusTok
   THEN
      RETURN( AddOp )
   ELSIF t=MinusTok
   THEN
      RETURN( SubOp )
   ELSIF t=DivTok
   THEN
      RETURN( DivM2Op )
   ELSIF t=DivideTok
   THEN
      RETURN( DivTruncOp )
   ELSIF t=RemTok
   THEN
      RETURN( ModTruncOp )
   ELSIF t=ModTok
   THEN
      RETURN( ModM2Op )
   ELSIF t=TimesTok
   THEN
      RETURN( MultOp )
   ELSIF t=HashTok
   THEN
      RETURN( IfNotEquOp )
   ELSIF t=LessGreaterTok
   THEN
      RETURN( IfNotEquOp )
   ELSIF t=GreaterEqualTok
   THEN
      RETURN( IfGreEquOp )
   ELSIF t=LessEqualTok
   THEN
      RETURN( IfLessEquOp )
   ELSIF t=EqualTok
   THEN
      RETURN( IfEquOp )
   ELSIF t=LessTok
   THEN
      RETURN( IfLessOp )
   ELSIF t=GreaterTok
   THEN
      RETURN( IfGreOp )
   ELSIF t=InTok
   THEN
      RETURN( IfInOp )
   ELSIF t=LogicalOrTok
   THEN
      RETURN( LogicalOrOp )
   ELSIF t=LogicalAndTok
   THEN
      RETURN( LogicalAndOp )
   ELSIF t=LogicalXorTok
   THEN
      RETURN( LogicalXorOp )
   ELSIF t=LogicalDifferenceTok
   THEN
      RETURN( LogicalDiffOp )
   ELSE
      InternalError('binary operation not implemented yet')
   END
END MakeOp ;


(*
   GenQuadO - generate a quadruple with Operation, Op1, Op2, Op3, overflow.
*)

PROCEDURE GenQuadO (TokPos: CARDINAL;
                    Operation: QuadOperator;
                    Op1, Op2, Op3: CARDINAL; overflow: BOOLEAN) ;
BEGIN
   GenQuadOTrash (TokPos, Operation, Op1, Op2, Op3, overflow, NulSym)
END GenQuadO ;


(*
   GenQuadOTrash - generate a quadruple with Operation, Op1, Op2, Op3, overflow.
*)

PROCEDURE GenQuadOTrash (TokPos: CARDINAL;
                         Operation: QuadOperator;
                         Op1, Op2, Op3: CARDINAL;
                         overflow: BOOLEAN; trash: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   (* WriteString('Potential Quad: ') ; *)
   IF QuadrupleGeneration
   THEN
      IF NextQuad # Head
      THEN
         f := GetQF (NextQuad-1) ;
         f^.Next := NextQuad
      END ;
      PutQuadO (NextQuad, Operation, Op1, Op2, Op3, overflow) ;
      f := GetQF (NextQuad) ;
      WITH f^ DO
         Trash := trash ;
         Next := 0 ;
         LineNo := GetLineNo () ;
         IF TokPos = UnknownTokenNo
         THEN
            TokenNo := GetTokenNo ()
         ELSE
            TokenNo := TokPos
         END ;
         IF GetDebugTraceQuad ()
         THEN
            printf0('generating: ') ;
            DisplayQuad (NextQuad) ;
            (* MetaErrorT1 (TokenNo, '{%1On}', NextQuad) *)
         END
      END ;
      IF NextQuad=BreakAtQuad
      THEN
         stop
      END ;
      NewQuad (NextQuad)
   END
END GenQuadOTrash ;


(*
   GetQuadTrash - return the symbol associated with the trashed operand.
*)

PROCEDURE GetQuadTrash (quad: CARDINAL) : CARDINAL ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (quad) ;
   LastQuadNo := quad ;
   RETURN f^.Trash
END GetQuadTrash ;


(*
   GenQuad - Generate a quadruple with Operation, Op1, Op2, Op3.
*)

PROCEDURE GenQuad (Operation: QuadOperator;
                   Op1, Op2, Op3: CARDINAL) ;
BEGIN
   GenQuadO (UnknownTokenNo, Operation, Op1, Op2, Op3, TRUE)
END GenQuad ;


(*
   GenQuadOtok - generate a quadruple with Operation, Op1, Op2, Op3, overflow.
*)

PROCEDURE GenQuadOtok (TokPos: CARDINAL;
                       Operation: QuadOperator;
                       Op1, Op2, Op3: CARDINAL; overflow: BOOLEAN;
                       Op1Pos, Op2Pos, Op3Pos: CARDINAL) ;
BEGIN
   GenQuadOTypetok (TokPos, Operation, Op1, Op2, Op3, overflow, TRUE,
                    Op1Pos, Op2Pos, Op3Pos)
END GenQuadOtok ;


(*
   GenQuadOTypetok - assigns the fields of the quadruple with
                     the parameters.
*)

PROCEDURE GenQuadOTypetok (TokPos: CARDINAL;
                           Operation: QuadOperator;
                           Op1, Op2, Op3: CARDINAL;
                           overflow, typecheck: BOOLEAN;
                           Op1Pos, Op2Pos, Op3Pos: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   (* WriteString('Potential Quad: ') ; *)
   IF QuadrupleGeneration
   THEN
      IF NextQuad # Head
      THEN
         f := GetQF (NextQuad-1) ;
         f^.Next := NextQuad
      END ;
      PutQuadOType (NextQuad, Operation, Op1, Op2, Op3, overflow, typecheck) ;
      f := GetQF (NextQuad) ;
      WITH f^ DO
         Next := 0 ;
         LineNo := GetLineNo () ;
         IF TokPos = UnknownTokenNo
         THEN
            TokenNo := GetTokenNo ()
         ELSE
            TokenNo := TokPos
         END ;
         op1pos := Op1Pos ;
         op2pos := Op2Pos ;
         op3pos := Op3Pos ;
         IF GetDebugTraceQuad ()
         THEN
            printf0('generating: ') ;
            DisplayQuad (NextQuad) ;
            (* MetaErrorT1 (TokenNo, '{%1On}', NextQuad) *)
         END
      END ;
      IF NextQuad=BreakAtQuad
      THEN
         stop
      END ;
      NewQuad (NextQuad)
   END
END GenQuadOTypetok ;


(*
   DumpUntil - dump all quadruples until we seen the ending quadruple
               with procsym in the third operand.
               Return the quad number containing the match.
*)

PROCEDURE DumpUntil (ending: QuadOperator;
                     procsym: CARDINAL; quad: CARDINAL) : CARDINAL ;
VAR
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
   f            : QuadFrame ;
BEGIN
   fprintf0 (GetDumpFile (), '\n...\n\n');
   REPEAT
      GetQuad (quad, op, op1, op2, op3) ;
      DisplayQuad (quad) ;
      f := GetQF (quad) ;
      quad := f^.Next
   UNTIL (op = ending) AND (op3 = procsym) ;
   RETURN quad
END DumpUntil ;


(*
   GetCtorInit - return the init procedure for the module.
*)

PROCEDURE GetCtorInit (sym: CARDINAL) : CARDINAL ;
VAR
   ctor, init, fini, dep: CARDINAL ;
BEGIN
   GetModuleCtors (sym, ctor, init, fini, dep) ;
   RETURN init
END GetCtorInit ;


(*
   GetCtorFini - return the fini procedure for the module.
*)

PROCEDURE GetCtorFini (sym: CARDINAL) : CARDINAL ;
VAR
   ctor, init, fini, dep: CARDINAL ;
BEGIN
   GetModuleCtors (sym, ctor, init, fini, dep) ;
   RETURN fini
END GetCtorFini ;


(*
   DumpQuadrupleFilter -
*)

PROCEDURE DumpQuadrupleFilter ;
VAR
   f            : QuadFrame ;
   i            : CARDINAL ;
   op           : QuadOperator ;
   op1, op2, op3: CARDINAL ;
BEGIN
   i := Head ;
   WHILE i # 0 DO
      GetQuad (i, op, op1, op2, op3) ;
      IF (op = ProcedureScopeOp) AND IsDumpRequired (op3, TRUE)
      THEN
         i := DumpUntil (KillLocalVarOp, op3, i)
      ELSIF (op = InitStartOp) AND IsDumpRequired (GetCtorInit (op3), TRUE)
      THEN
         i := DumpUntil (InitEndOp, op3, i)
      ELSIF (op = FinallyStartOp) AND IsDumpRequired (GetCtorFini (op3), TRUE)
      THEN
         i := DumpUntil (FinallyEndOp, op3, i)
      ELSE
         f := GetQF (i) ;
         i := f^.Next
      END
   END
END DumpQuadrupleFilter ;


(*
   DumpQuadrupleAll - dump all quadruples.
*)

PROCEDURE DumpQuadrupleAll ;
VAR
   f: QuadFrame ;
   i: CARDINAL ;
BEGIN
   i := Head ;
   WHILE i # 0 DO
      DisplayQuad (i) ;
      f := GetQF (i) ;
      i := f^.Next
   END
END DumpQuadrupleAll ;


(*
   DumpQuadruples - dump all quadruples providing the -fq, -fdump-lang-quad,
                    -fdump-lang-quad= or -fdump-lang-all were issued to the
                    command line.
*)

PROCEDURE DumpQuadruples (title: ARRAY OF CHAR) ;
BEGIN
   IF GetDumpQuad ()
   THEN
      CreateDumpQuad (title) ;
      IF GetM2DumpFilter () = NIL
      THEN
         DumpQuadrupleAll
      ELSE
         DumpQuadrupleFilter
      END ;
      CloseDumpQuad
   END
END DumpQuadruples ;


(*
   DisplayQuadRange - displays all quads in list range, start..end.
*)

PROCEDURE DisplayQuadRange (scope: CARDINAL; start, end: CARDINAL) ;
VAR
   f: QuadFrame ;
BEGIN
   fprintf1 (GetDumpFile (), 'Quadruples for scope: %d\n', scope) ;
   WHILE (start <= end) AND (start # 0) DO
      DisplayQuad (start) ;
      f := GetQF (start) ;
      start := f^.Next
   END
END DisplayQuadRange ;


(*
   BackPatch - Makes each of the quadruples on the list pointed to by
               QuadNo take quadruple Value as a target.
*)

PROCEDURE BackPatch (QuadNo, Value: CARDINAL) ;
VAR
   i: CARDINAL ;
   f: QuadFrame ;
BEGIN
   IF QuadrupleGeneration
   THEN
      WHILE QuadNo#0 DO
         f := GetQF (QuadNo) ;
         WITH f^ DO
            i := Operand3 ;                       (* Next Link along the BackPatch *)
            ManipulateReference (QuadNo, Value)   (* Filling in the BackPatch.     *)
         END ;
         QuadNo := i
      END
   END
END BackPatch ;


(*
   Merge - joins two quad lists, QuadList2 to the end of QuadList1.
           A QuadList of value zero is a nul list.
*)

PROCEDURE Merge (QuadList1, QuadList2: CARDINAL) : CARDINAL ;
VAR
   i, j: CARDINAL ;
   f   : QuadFrame ;
BEGIN
   IF QuadList1=0
   THEN
      RETURN( QuadList2 )
   ELSIF QuadList2=0
   THEN
      RETURN( QuadList1 )
   ELSE
      i := QuadList1 ;
      REPEAT
         j := i ;
         f := GetQF(i) ;
         i := f^.Operand3
      UNTIL i=0 ;
      ManipulateReference(j, QuadList2) ;
      RETURN( QuadList1 )
   END
END Merge ;


(*
   Annotate - annotate the top of stack.
*)

PROCEDURE Annotate (a: ARRAY OF CHAR) ;
VAR
   f: BoolFrame ;
BEGIN
   IF DebugStackOn AND CompilerDebugging AND (NoOfItemsInStackAddress(BoolStack)>0)
   THEN
      f := PeepAddress(BoolStack, 1) ;    (* top of stack *)
      WITH f^ DO
         IF Annotation#NIL
         THEN
            Annotation := KillString(Annotation)
         END ;
         Annotation := InitString(a)
      END
   END
END Annotate ;


(*
   OperandAnno - returns the annotation string associated with the
                 position, n, on the stack.
*)

PROCEDURE OperandAnno (n: CARDINAL) : String ;
VAR
   f: BoolFrame ;
BEGIN
   f := PeepAddress (BoolStack, n) ;
   RETURN f^.Annotation
END OperandAnno ;


(*
   DisplayStack - displays the compile time symbol stack.
*)

PROCEDURE DisplayStack ;
BEGIN
   IF DebugStackOn AND CompilerDebugging
   THEN
      DebugStack (NoOfItemsInStackAddress (BoolStack),
                  OperandTno, OperandFno, OperandA,
                  OperandD, OperandRW, OperandTok, OperandAnno)
   END
END DisplayStack ;


(*
   ds - tiny procedure name, useful for calling from the gdb shell.
*)

(*
PROCEDURE ds ;
BEGIN
   DisplayStack
END ds ;
*)


(*
   DisplayQuad - displays a quadruple, QuadNo.
*)

PROCEDURE DisplayQuad (QuadNo: CARDINAL) ;
BEGIN
   DSdbEnter ;
   fprintf1 (GetDumpFile (), '%4d  ', QuadNo) ; WriteQuad(QuadNo) ; fprintf0 (GetDumpFile (), '\n') ;
   DSdbExit
END DisplayQuad ;


(*
   DisplayProcedureAttributes -
*)

PROCEDURE DisplayProcedureAttributes (proc: CARDINAL) ;
BEGIN
   IF IsCtor (proc)
   THEN
      fprintf0 (GetDumpFile (), " (ctor)")
   END ;
   IF IsPublic (proc)
   THEN
      fprintf0 (GetDumpFile (), " (public)")
   END ;
   IF IsExtern (proc)
   THEN
      fprintf0 (GetDumpFile (), " (extern)")
   END ;
   IF IsMonoName (proc)
   THEN
      fprintf0 (GetDumpFile (), " (mononame)")
   END
END DisplayProcedureAttributes ;


(*
   WriteQuad - Writes out the Quad BufferQuad.
*)

PROCEDURE WriteQuad (BufferQuad: CARDINAL) ;
VAR
   n1, n2: Name ;
   f     : QuadFrame ;
   n     : Name ;
   l     : CARDINAL ;
BEGIN
   f := GetQF(BufferQuad) ;
   WITH f^ DO
      WriteOperator(Operator) ;
      fprintf1 (GetDumpFile (), ' [%d]', NoOfTimesReferenced) ;
      IF ConstExpr
      THEN
         fprintf0 (GetDumpFile (), ' const ')
      ELSE
         fprintf0 (GetDumpFile (), '       ')
      END ;
      CASE Operator OF

      HighOp           : WriteOperand(Operand1) ;
                         fprintf1 (GetDumpFile (), '  %4d  ', Operand2) ;
                         WriteOperand(Operand3) |
      InitAddressOp,
      SavePriorityOp,
      RestorePriorityOp,
      SubrangeLowOp,
      SubrangeHighOp,
      BecomesOp,
      InclOp,
      ExclOp,
      UnboundedOp,
      ReturnValueOp,
      FunctValueOp,
      NegateOp,
      AddrOp,
      StringConvertCnulOp,
      StringConvertM2nulOp,
      StringLengthOp    : WriteOperand(Operand1) ;
                          fprintf0 (GetDumpFile (), '  ') ;
                          WriteOperand(Operand3) |
      ElementSizeOp,
      IfInOp,
      IfNotInOp,
      IfNotEquOp,
      IfEquOp,
      IfLessOp,
      IfGreOp,
      IfLessEquOp,
      IfGreEquOp        : WriteOperand(Operand1) ;
                          fprintf0 (GetDumpFile (), '  ') ;
                          WriteOperand(Operand2) ;
                          fprintf1 (GetDumpFile (), '  %4d', Operand3) |

      InlineOp,
      RetryOp,
      TryOp,
      GotoOp            : fprintf1 (GetDumpFile (), '%4d', Operand3) |

      StatementNoteOp   : l := TokenToLineNo(Operand3, 0) ;
                          n := GetTokenName (Operand3) ;
                          fprintf4 (GetDumpFile (), '%a:%d:%a (tokenno %d)', Operand1, l, n, Operand3) |
      LineNumberOp      : fprintf2 (GetDumpFile (), '%a:%d', Operand1, Operand3) |

      EndFileOp         : n1 := GetSymName(Operand3) ;
                          fprintf1 (GetDumpFile (), '%a', n1) |

      ThrowOp,
      ReturnOp,
      CallOp,
      KillLocalVarOp    : WriteOperand(Operand3) |

      ProcedureScopeOp  : n1 := GetSymName(Operand2) ;
                          n2 := GetSymName(Operand3) ;
                          fprintf3 (GetDumpFile (), '  %4d  %a  %a', Operand1, n1, n2) ;
                          DisplayProcedureAttributes (Operand3) |
      NewLocalVarOp,
      FinallyStartOp,
      FinallyEndOp,
      InitEndOp,
      InitStartOp       : n1 := GetSymName(Operand2) ;
                          n2 := GetSymName(Operand3) ;
                          fprintf3 (GetDumpFile (), '  %4d  %a  %a', Operand1, n1, n2) |

      ModuleScopeOp,
      StartModFileOp    : n1 := GetSymName(Operand3) ;
                          fprintf4 (GetDumpFile (), '%a:%d  %a(%d)', Operand2, Operand1, n1, Operand3) |

      StartDefFileOp    : n1 := GetSymName(Operand3) ;
                          fprintf2 (GetDumpFile (), '  %4d  %a', Operand1, n1) |

      OptParamOp,
      ParamOp           : fprintf1 (GetDumpFile (), '%4d  ', Operand1) ;
                          WriteOperand(Operand2) ;
                          fprintf0 (GetDumpFile (), '  ') ;
                          WriteOperand(Operand3) |
      SizeOp,
      RecordFieldOp,
      IndrXOp,
      XIndrOp,
      ArrayOp,
      LogicalShiftOp,
      LogicalRotateOp,
      LogicalOrOp,
      LogicalAndOp,
      LogicalXorOp,
      LogicalDiffOp,
      ArithAddOp,
      CoerceOp,
      ConvertOp,
      CastOp,
      AddOp,
      SubOp,
      MultOp,
      DivM2Op,
      ModM2Op,
      ModFloorOp,
      DivCeilOp,
      ModCeilOp,
      DivFloorOp,
      ModTruncOp,
      DivTruncOp        : WriteOperand(Operand1) ;
                          fprintf0 (GetDumpFile (), '  ') ;
                          WriteOperand(Operand2) ;
                          fprintf0 (GetDumpFile (), '  ') ;
                          WriteOperand(Operand3) |
      DummyOp,
      CodeOnOp,
      CodeOffOp,
      ProfileOnOp,
      ProfileOffOp,
      OptimizeOnOp,
      OptimizeOffOp     : |
      BuiltinConstOp    : WriteOperand(Operand1) ;
                          fprintf1 (GetDumpFile (), '   %a', Operand3) |
      BuiltinTypeInfoOp : WriteOperand(Operand1) ;
                          fprintf1 (GetDumpFile (), '   %a', Operand2) ;
                          fprintf1 (GetDumpFile (), '   %a', Operand3) |
      StandardFunctionOp: WriteOperand(Operand1) ;
                          fprintf0 (GetDumpFile (), '  ') ;
                          WriteOperand(Operand2) ;
                          fprintf0 (GetDumpFile (), '  ') ;
                          WriteOperand(Operand3) |
      CatchBeginOp,
      CatchEndOp        : |

      RangeCheckOp,
      ErrorOp           : WriteRangeCheck (Operand3) |
      SaveExceptionOp,
      RestoreExceptionOp: WriteOperand(Operand1) ;
                          fprintf0 (GetDumpFile (), '  ') ;
                          WriteOperand(Operand3)

      ELSE
         InternalError ('quadruple not recognised')
      END
   END
END WriteQuad ;


(*
   WriteOperator - writes the name of the quadruple operator.
*)

PROCEDURE WriteOperator (Operator: QuadOperator) ;
BEGIN
   CASE Operator OF

   ArithAddOp               : fprintf0 (GetDumpFile (), 'Arith +           ') |
   InitAddressOp            : fprintf0 (GetDumpFile (), 'InitAddress       ') |
   LogicalOrOp              : fprintf0 (GetDumpFile (), 'Or                ') |
   LogicalAndOp             : fprintf0 (GetDumpFile (), 'And               ') |
   LogicalXorOp             : fprintf0 (GetDumpFile (), 'Xor               ') |
   LogicalDiffOp            : fprintf0 (GetDumpFile (), 'Ldiff             ') |
   LogicalShiftOp           : fprintf0 (GetDumpFile (), 'Shift             ') |
   LogicalRotateOp          : fprintf0 (GetDumpFile (), 'Rotate            ') |
   BecomesOp                : fprintf0 (GetDumpFile (), 'Becomes           ') |
   IndrXOp                  : fprintf0 (GetDumpFile (), 'IndrX             ') |
   XIndrOp                  : fprintf0 (GetDumpFile (), 'XIndr             ') |
   ArrayOp                  : fprintf0 (GetDumpFile (), 'Array             ') |
   ElementSizeOp            : fprintf0 (GetDumpFile (), 'ElementSize       ') |
   RecordFieldOp            : fprintf0 (GetDumpFile (), 'RecordField       ') |
   AddrOp                   : fprintf0 (GetDumpFile (), 'Addr              ') |
   SizeOp                   : fprintf0 (GetDumpFile (), 'Size              ') |
   IfInOp                   : fprintf0 (GetDumpFile (), 'If IN             ') |
   IfNotInOp                : fprintf0 (GetDumpFile (), 'If NOT IN         ') |
   IfNotEquOp               : fprintf0 (GetDumpFile (), 'If <>             ') |
   IfEquOp                  : fprintf0 (GetDumpFile (), 'If =              ') |
   IfLessEquOp              : fprintf0 (GetDumpFile (), 'If <=             ') |
   IfGreEquOp               : fprintf0 (GetDumpFile (), 'If >=             ') |
   IfGreOp                  : fprintf0 (GetDumpFile (), 'If >              ') |
   IfLessOp                 : fprintf0 (GetDumpFile (), 'If <              ') |
   GotoOp                   : fprintf0 (GetDumpFile (), 'Goto              ') |
   DummyOp                  : fprintf0 (GetDumpFile (), 'Dummy             ') |
   ModuleScopeOp            : fprintf0 (GetDumpFile (), 'ModuleScopeOp     ') |
   StartDefFileOp           : fprintf0 (GetDumpFile (), 'StartDefFile      ') |
   StartModFileOp           : fprintf0 (GetDumpFile (), 'StartModFile      ') |
   EndFileOp                : fprintf0 (GetDumpFile (), 'EndFileOp         ') |
   InitStartOp              : fprintf0 (GetDumpFile (), 'InitStart         ') |
   InitEndOp                : fprintf0 (GetDumpFile (), 'InitEnd           ') |
   FinallyStartOp           : fprintf0 (GetDumpFile (), 'FinallyStart      ') |
   FinallyEndOp             : fprintf0 (GetDumpFile (), 'FinallyEnd        ') |
   RetryOp                  : fprintf0 (GetDumpFile (), 'Retry             ') |
   TryOp                    : fprintf0 (GetDumpFile (), 'Try               ') |
   ThrowOp                  : fprintf0 (GetDumpFile (), 'Throw             ') |
   CatchBeginOp             : fprintf0 (GetDumpFile (), 'CatchBegin        ') |
   CatchEndOp               : fprintf0 (GetDumpFile (), 'CatchEnd          ') |
   AddOp                    : fprintf0 (GetDumpFile (), '+                 ') |
   SubOp                    : fprintf0 (GetDumpFile (), '-                 ') |
   DivM2Op                  : fprintf0 (GetDumpFile (), 'DIV M2            ') |
   ModM2Op                  : fprintf0 (GetDumpFile (), 'MOD M2            ') |
   DivCeilOp                : fprintf0 (GetDumpFile (), 'DIV ceil          ') |
   ModCeilOp                : fprintf0 (GetDumpFile (), 'MOD ceil          ') |
   DivFloorOp               : fprintf0 (GetDumpFile (), 'DIV floor         ') |
   ModFloorOp               : fprintf0 (GetDumpFile (), 'MOD floor         ') |
   DivTruncOp               : fprintf0 (GetDumpFile (), 'DIV trunc         ') |
   ModTruncOp               : fprintf0 (GetDumpFile (), 'MOD trunc         ') |
   MultOp                   : fprintf0 (GetDumpFile (), '*                 ') |
   NegateOp                 : fprintf0 (GetDumpFile (), 'Negate            ') |
   InclOp                   : fprintf0 (GetDumpFile (), 'Incl              ') |
   ExclOp                   : fprintf0 (GetDumpFile (), 'Excl              ') |
   ReturnOp                 : fprintf0 (GetDumpFile (), 'Return            ') |
   ReturnValueOp            : fprintf0 (GetDumpFile (), 'ReturnValue       ') |
   FunctValueOp             : fprintf0 (GetDumpFile (), 'FunctValue        ') |
   CallOp                   : fprintf0 (GetDumpFile (), 'Call              ') |
   ParamOp                  : fprintf0 (GetDumpFile (), 'Param             ') |
   OptParamOp               : fprintf0 (GetDumpFile (), 'OptParam          ') |
   NewLocalVarOp            : fprintf0 (GetDumpFile (), 'NewLocalVar       ') |
   KillLocalVarOp           : fprintf0 (GetDumpFile (), 'KillLocalVar      ') |
   ProcedureScopeOp         : fprintf0 (GetDumpFile (), 'ProcedureScope    ') |
   UnboundedOp              : fprintf0 (GetDumpFile (), 'Unbounded         ') |
   CoerceOp                 : fprintf0 (GetDumpFile (), 'Coerce            ') |
   ConvertOp                : fprintf0 (GetDumpFile (), 'Convert           ') |
   CastOp                   : fprintf0 (GetDumpFile (), 'Cast              ') |
   HighOp                   : fprintf0 (GetDumpFile (), 'High              ') |
   CodeOnOp                 : fprintf0 (GetDumpFile (), 'CodeOn            ') |
   CodeOffOp                : fprintf0 (GetDumpFile (), 'CodeOff           ') |
   ProfileOnOp              : fprintf0 (GetDumpFile (), 'ProfileOn         ') |
   ProfileOffOp             : fprintf0 (GetDumpFile (), 'ProfileOff        ') |
   OptimizeOnOp             : fprintf0 (GetDumpFile (), 'OptimizeOn        ') |
   OptimizeOffOp            : fprintf0 (GetDumpFile (), 'OptimizeOff       ') |
   InlineOp                 : fprintf0 (GetDumpFile (), 'Inline            ') |
   StatementNoteOp          : fprintf0 (GetDumpFile (), 'StatementNote     ') |
   LineNumberOp             : fprintf0 (GetDumpFile (), 'LineNumber        ') |
   BuiltinConstOp           : fprintf0 (GetDumpFile (), 'BuiltinConst      ') |
   BuiltinTypeInfoOp        : fprintf0 (GetDumpFile (), 'BuiltinTypeInfo   ') |
   StandardFunctionOp       : fprintf0 (GetDumpFile (), 'StandardFunction  ') |
   SavePriorityOp           : fprintf0 (GetDumpFile (), 'SavePriority      ') |
   RestorePriorityOp        : fprintf0 (GetDumpFile (), 'RestorePriority   ') |
   RangeCheckOp             : fprintf0 (GetDumpFile (), 'RangeCheck        ') |
   ErrorOp                  : fprintf0 (GetDumpFile (), 'Error             ') |
   SaveExceptionOp          : fprintf0 (GetDumpFile (), 'SaveException     ') |
   RestoreExceptionOp       : fprintf0 (GetDumpFile (), 'RestoreException  ') |
   StringConvertCnulOp      : fprintf0 (GetDumpFile (), 'StringConvertCnul ') |
   StringConvertM2nulOp     : fprintf0 (GetDumpFile (), 'StringConvertM2nul') |
   StringLengthOp           : fprintf0 (GetDumpFile (), 'StringLength      ') |
   SubrangeHighOp           : fprintf0 (GetDumpFile (), 'SubrangeHigh      ') |
   SubrangeLowOp            : fprintf0 (GetDumpFile (), 'SubrangeLow       ')

   ELSE
      InternalError ('operator not expected')
   END
END WriteOperator ;


(*
   WriteOperand - displays the operands name, symbol id and mode of addressing.
*)

PROCEDURE WriteOperand (Sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   IF Sym = NulSym
   THEN
      fprintf0 (GetDumpFile (), '<nulsym>')
   ELSE
      n := GetSymName (Sym) ;
      fprintf1 (GetDumpFile (), '%a', n) ;
      IF IsVar (Sym) OR IsConst (Sym)
      THEN
         fprintf0 (GetDumpFile (), '[') ; WriteMode (GetMode (Sym)) ; fprintf0 (GetDumpFile (), ']')
      END ;
      fprintf1 (GetDumpFile (), '(%d)', Sym)
   END
END WriteOperand ;


PROCEDURE WriteMode (Mode: ModeOfAddr) ;
BEGIN
   CASE Mode OF

   ImmediateValue: fprintf0 (GetDumpFile (), 'i') |
   NoValue       : fprintf0 (GetDumpFile (), 'n') |
   RightValue    : fprintf0 (GetDumpFile (), 'r') |
   LeftValue     : fprintf0 (GetDumpFile (), 'l')

   ELSE
      InternalError ('unrecognised mode')
   END
END WriteMode ;


(*
   GetQuadOp - returns the operator for quad.
*)

PROCEDURE GetQuadOp (quad: CARDINAL) : QuadOperator ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF (quad) ;
   RETURN f^.Operator
END GetQuadOp ;


(*
   GetM2OperatorDesc - returns the Modula-2 string associated with the quad operator
                       (if possible).  It returns NIL if no there is not an obvious match
                       in Modula-2.  It is assummed that the string will be used during
                       construction of error messages and therefore keywords are
                       wrapped with a format specifier.
*)

PROCEDURE GetM2OperatorDesc (op: QuadOperator) : String ;
BEGIN
   CASE op OF

   NegateOp    :  RETURN InitString ('-') |
   AddOp       :  RETURN InitString ('+') |
   SubOp       :  RETURN InitString ('-') |
   MultOp      :  RETURN InitString ('*') |
   DivM2Op,
   DivCeilOp,
   DivFloorOp,
   DivTruncOp  :  RETURN InitString ('{%kDIV}') |
   ModM2Op,
   ModCeilOp,
   ModFloorOp  :  RETURN InitString ('{%kMOD}') |
   ModTruncOp  :  RETURN InitString ('{%kREM}') |
   LogicalOrOp :  RETURN InitString ('{%kOR}') |
   LogicalAndOp:  RETURN InitString ('{%kAND}') |
   InclOp      :  RETURN InitString ('{%kINCL}') |
   ExclOp      :  RETURN InitString ('{%kEXCL}') |
   IfEquOp     :  RETURN InitString ('=') |
   IfLessEquOp :  RETURN InitString ('<=') |
   IfGreEquOp  :  RETURN InitString ('>=') |
   IfGreOp     :  RETURN InitString ('>') |
   IfLessOp    :  RETURN InitString ('<') |
   IfNotEquOp  :  RETURN InitString ('#') |
   IfInOp      :  RETURN InitString ('IN') |
   IfNotInOp   :  RETURN InitString ('NOT IN')

   ELSE
      RETURN NIL
   END
END GetM2OperatorDesc ;



(*
   PushExit - pushes the exit value onto the EXIT stack.
*)

PROCEDURE PushExit (Exit: CARDINAL) ;
BEGIN
   PushWord(ExitStack, Exit)
END PushExit ;


(*
   PopExit - pops the exit value from the EXIT stack.
*)

PROCEDURE PopExit() : WORD ;
BEGIN
   RETURN( PopWord(ExitStack) )
END PopExit ;


(*
   PushFor - pushes the exit value onto the FOR stack.
*)

PROCEDURE PushFor (Exit: CARDINAL) ;
BEGIN
   PushWord(ForStack, Exit)
END PushFor ;


(*
   PopFor - pops the exit value from the FOR stack.
*)

PROCEDURE PopFor() : WORD ;
BEGIN
   RETURN( PopWord(ForStack) )
END PopFor ;


(*
   OperandTno - returns the ident operand stored in the true position
                on the boolean stack.  This is exactly the same as
                OperandT but it has no IsBoolean checking.
*)

PROCEDURE OperandTno (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.TrueExit )
END OperandTno ;


(*
   OperandFno - returns the ident operand stored in the false position
                on the boolean stack.  This is exactly the same as
                OperandF but it has no IsBoolean checking.
*)

PROCEDURE OperandFno (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   f := PeepAddress (BoolStack, pos) ;
   RETURN f^.FalseExit
END OperandFno ;


(*
   OperandTtok - returns the token associated with the position, pos
                 on the boolean stack.
*)

PROCEDURE OperandTtok (pos: CARDINAL) : CARDINAL ;
VAR
   f: BoolFrame ;
BEGIN
   Assert (pos > 0) ;
   f := PeepAddress (BoolStack, pos) ;
   RETURN f^.tokenno
END OperandTtok ;


(*
   PopBooltok - Pops a True and a False exit quad number from the True/False
                stack.
*)

PROCEDURE PopBooltok (VAR True, False: CARDINAL; VAR tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress (BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      tokno := tokenno ;
      Assert (BooleanOp)
   END ;
   DISPOSE (f)
END PopBooltok ;


(*
   PushBooltok - Push a True and a False exit quad numbers onto the
                 True/False stack.
*)

PROCEDURE PushBooltok (True, False: CARDINAL; tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   Assert (True<=NextQuad) ;
   Assert (False<=NextQuad) ;
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      BooleanOp := TRUE ;
      tokenno := tokno ;
      Annotation := NIL
   END ;
   PushAddress (BoolStack, f) ;
   Annotate ('<q%1d>|<q%2d>||true quad|false quad')
END PushBooltok ;


(*
   PopBool - Pops a True and a False exit quad number from the True/False
             stack.
*)

PROCEDURE PopBool (VAR True, False: CARDINAL) ;
VAR
   tokno: CARDINAL ;
BEGIN
   PopBooltok (True, False, tokno)
END PopBool ;


(*
   PushBool - Push a True and a False exit quad numbers onto the
              True/False stack.
*)

PROCEDURE PushBool (True, False: CARDINAL) ;
BEGIN
   PushBooltok (True, False, UnknownTokenNo)
END PushBool ;


(*
   IsBoolean - returns true is the Stack position pos contains a Boolean
               Exit. False is returned if an Ident is stored.
*)

PROCEDURE IsBoolean (pos: CARDINAL) : BOOLEAN ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.BooleanOp )
END IsBoolean ;


(*
   OperandD - returns possible array dimension associated with the ident
              operand stored on the boolean stack.
*)

PROCEDURE OperandD (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean (pos)) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.Dimension )
END OperandD ;


(*
   OperandA - returns possible array symbol associated with the ident
              operand stored on the boolean stack.
*)

PROCEDURE OperandA (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean (pos)) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.Unbounded )
END OperandA ;


(*
   OperandT - returns the ident operand stored in the true position on the boolean stack.
*)

PROCEDURE OperandT (pos: CARDINAL) : WORD ;
BEGIN
   Assert(NOT IsBoolean (pos)) ;
   RETURN( OperandTno(pos) )
END OperandT ;


(*
   OperandF - returns the ident operand stored in the false position on the boolean stack.
*)

PROCEDURE OperandF (pos: CARDINAL) : WORD ;
BEGIN
   Assert(NOT IsBoolean (pos)) ;
   RETURN( OperandFno(pos) )
END OperandF ;


(*
   OperandRW - returns the rw operand stored on the boolean stack.
*)

PROCEDURE OperandRW (pos: CARDINAL) : WORD ;
VAR
   f: BoolFrame ;
BEGIN
   Assert(pos>0) ;
   Assert(NOT IsBoolean (pos)) ;
   f := PeepAddress(BoolStack, pos) ;
   RETURN( f^.ReadWrite )
END OperandRW ;


(*
   OperandMergeRW - returns the rw operand if not NulSym else it
                    returns True.
*)

PROCEDURE OperandMergeRW (pos: CARDINAL) : WORD ;
BEGIN
   IF OperandRW (pos) = NulSym
   THEN
      RETURN OperandT (pos)
   ELSE
      RETURN OperandRW (pos)
   END
END OperandMergeRW ;


(*
   OperandTok - returns the token associated with pos, on the stack.
*)

PROCEDURE OperandTok (pos: CARDINAL) : WORD ;
BEGIN
   Assert (NOT IsBoolean (pos)) ;
   RETURN OperandTtok (pos)
END OperandTok ;


(*
   BuildCodeOn - generates a quadruple declaring that code should be
                 emmitted from henceforth.

                 The Stack is unnaffected.
*)

PROCEDURE BuildCodeOn ;
BEGIN
   GenQuad(CodeOnOp, NulSym, NulSym, NulSym)
END BuildCodeOn ;


(*
   BuildCodeOff - generates a quadruple declaring that code should not be
                  emmitted from henceforth.

                  The Stack is unnaffected.
*)

PROCEDURE BuildCodeOff ;
BEGIN
   GenQuad(CodeOffOp, NulSym, NulSym, NulSym)
END BuildCodeOff ;


(*
   BuildProfileOn - generates a quadruple declaring that profile timings
                    should be emmitted from henceforth.

                    The Stack is unnaffected.
*)

PROCEDURE BuildProfileOn ;
BEGIN
   GenQuad(ProfileOnOp, NulSym, NulSym, NulSym)
END BuildProfileOn ;


(*
   BuildProfileOn - generates a quadruple declaring that profile timings
                    should be emmitted from henceforth.

                    The Stack is unnaffected.
*)

PROCEDURE BuildProfileOff ;
BEGIN
   GenQuad(ProfileOffOp, NulSym, NulSym, NulSym)
END BuildProfileOff ;


(*
   BuildOptimizeOn - generates a quadruple declaring that optimization
                     should occur from henceforth.

                     The Stack is unnaffected.
*)

PROCEDURE BuildOptimizeOn ;
BEGIN
   GenQuad(OptimizeOnOp, NulSym, NulSym, NulSym)
END BuildOptimizeOn ;


(*
   BuildOptimizeOff - generates a quadruple declaring that optimization
                      should not occur from henceforth.

                      The Stack is unnaffected.
*)

PROCEDURE BuildOptimizeOff ;
BEGIN
   GenQuad (OptimizeOffOp, NulSym, NulSym, NulSym)
END BuildOptimizeOff ;


(*
   BuildAsm - builds an Inline pseudo quadruple operator.
              The inline interface, Sym, is stored as the operand
              to the operator InlineOp.

              The stack is expected to contain:


                        Entry                   Exit
                        =====                   ====

              Ptr ->
                     +--------------+
                     | Sym          |        Empty
                     |--------------|
*)

PROCEDURE BuildAsm (tok: CARDINAL) ;
VAR
   Sym: CARDINAL ;
BEGIN
   PopT (Sym) ;
   GenQuadO (tok, InlineOp, NulSym, NulSym, Sym, FALSE)
END BuildAsm ;


(*
   BuildLineNo - builds a LineNumberOp pseudo quadruple operator.
                 This quadruple indicates which source line has been
                 processed, these quadruples are only generated if we
                 are producing runtime debugging information.

                 The stack is not affected, read or altered in any way.


                        Entry                   Exit
                        =====                   ====

                 Ptr ->                              <- Ptr
*)

PROCEDURE BuildLineNo ;
VAR
   filename: Name ;
   f       : QuadFrame ;
BEGIN
   IF (NextQuad#Head) AND (GenerateLineDebug OR GenerateDebugging) AND FALSE
   THEN
      filename := makekey (string (GetFileName ())) ;
      f := GetQF (NextQuad-1) ;
      IF NOT ((f^.Operator = LineNumberOp) AND (f^.Operand1 = WORD (filename)))
      THEN
         GenQuad (LineNumberOp, WORD (filename), NulSym, WORD (GetLineNo ()))
      END
   END
END BuildLineNo ;


(*
   UseLineNote - uses the line note and returns it to the free list.
*)

PROCEDURE UseLineNote (l: LineNote) ;
VAR
   f: QuadFrame ;
BEGIN
   WITH l^ DO
      f := GetQF (NextQuad-1) ;
      IF (f^.Operator = LineNumberOp) AND (f^.Operand1 = WORD (File))
      THEN
         (* do nothing *)
      ELSE
         IF FALSE
         THEN
            GenQuad (LineNumberOp, WORD (File), NulSym, WORD (Line))
         END
      END ;
      Next := FreeLineList
   END ;
   FreeLineList := l
END UseLineNote ;


(*
   PopLineNo - pops a line note from the line stack.
*)

PROCEDURE PopLineNo () : LineNote ;
VAR
   l: LineNote ;
BEGIN
   l := PopAddress(LineStack) ;
   IF l=NIL
   THEN
      InternalError ('no line note available')
   END ;
   RETURN( l  )
END PopLineNo ;


(*
   InitLineNote - creates a line note and initializes it to
                  contain, file, line.
*)

PROCEDURE InitLineNote (file: Name; line: CARDINAL) : LineNote ;
VAR
   l: LineNote ;
BEGIN
   IF FreeLineList=NIL
   THEN
      NEW(l)
   ELSE
      l := FreeLineList ;
      FreeLineList := FreeLineList^.Next
   END ;
   WITH l^ DO
      File := file ;
      Line := line
   END ;
   RETURN( l )
END InitLineNote ;


(*
   PushLineNote -
*)

PROCEDURE PushLineNote (l: LineNote) ;
BEGIN
   PushAddress(LineStack, l)
END PushLineNote ;


(*
   PushLineNo - pushes the current file and line number to the stack.
*)

PROCEDURE PushLineNo ;
BEGIN
   PushLineNote(InitLineNote(makekey(string(GetFileName())), GetLineNo()))
END PushLineNo ;


(*
   BuildStmtNote - builds a StatementNoteOp pseudo quadruple operator.
                   This quadruple indicates which source line has been
                   processed and it represents the start of a statement
                   sequence.
                   It differs from LineNumberOp in that multiple successive
                   LineNumberOps will be removed and the final one is attached to
                   the next real GCC tree.  Whereas a StatementNoteOp is always left
                   alone.  Depending upon the debugging level it will issue a nop
                   instruction to ensure that the gdb single step will step into
                   this line.  Practically it allows pedalogical debugging to
                   occur when there is syntax sugar such as:


                         END  (* step *)
                      END  (* step *)
                   END ; (* step *)
		   a := 1 ; (* step *)

                   REPEAT (* step *)
		      i := 1  (* step *)

                   The stack is not affected, read or altered in any way.


                        Entry                   Exit
                        =====                   ====

                 Ptr ->                              <- Ptr
*)

PROCEDURE BuildStmtNote (offset: INTEGER) ;
VAR
   tokenno: INTEGER ;
BEGIN
   IF NextQuad#Head
   THEN
      tokenno := offset ;
      INC (tokenno, GetTokenNo ()) ;
      BuildStmtNoteTok (VAL(CARDINAL, tokenno))
   END
END BuildStmtNote ;


(*
   BuildStmtNoteTok - adds a nop (with an assigned tokenno location) to the code.
*)

PROCEDURE BuildStmtNoteTok (tokenno: CARDINAL) ;
VAR
   filename: Name ;
   f       : QuadFrame ;
BEGIN
   f := GetQF (NextQuad-1) ;
   (* no need to have multiple notes at the same position.  *)
   IF (f^.Operator # StatementNoteOp) OR (f^.Operand3 # tokenno)
   THEN
      filename := makekey (string (GetFileName ())) ;
      GenQuad (StatementNoteOp, WORD (filename), NulSym, tokenno)
   END
END BuildStmtNoteTok ;


(*
   AddRecordToList - adds the record held on the top of stack to the
                     list of records and varient fields.
*)

PROCEDURE AddRecordToList ;
VAR
   r: CARDINAL ;
   n: CARDINAL ;
BEGIN
   r := OperandT(1) ;
   Assert(IsRecord(r) OR IsFieldVarient(r)) ;
   (*
      r might be a field varient if the declaration consists of nested
      varients.  However ISO TSIZE can only utilise record types, we store
      a varient field anyway as the next pass would not know whether to
      ignore a varient field.
   *)
   PutItemIntoList (VarientFields, r) ;
   IF DebugVarients
   THEN
      n := NoOfItemsInList(VarientFields) ;
      IF IsRecord(r)
      THEN
         printf2('in list: record %d is %d\n', n, r)
      ELSE
         printf2('in list: varient field %d is %d\n', n, r)
      END
   END
END AddRecordToList ;


(*
   AddVarientToList - adds varient held on the top of stack to the list.
*)

PROCEDURE AddVarientToList ;
VAR
   v, n: CARDINAL ;
BEGIN
   v := OperandT(1) ;
   Assert(IsVarient(v)) ;
   PutItemIntoList(VarientFields, v) ;
   IF DebugVarients
   THEN
      n := NoOfItemsInList(VarientFields) ;
      printf2('in list: varient %d is %d\n', n, v)
   END
END AddVarientToList ;


(*
   AddVarientFieldToList - adds varient field, f, to the list of all varient
                           fields created.
*)

PROCEDURE AddVarientFieldToList (f: CARDINAL) ;
VAR
   n: CARDINAL ;
BEGIN
   Assert(IsFieldVarient(f)) ;
   PutItemIntoList(VarientFields, f) ;
   IF DebugVarients
   THEN
      n := NoOfItemsInList(VarientFields) ;
      printf2('in list: varient field %d is %d\n', n, f)
   END
END AddVarientFieldToList ;


(*
   GetRecordOrField -
*)

PROCEDURE GetRecordOrField () : CARDINAL ;
VAR
   f: CARDINAL ;
BEGIN
   INC(VarientFieldNo) ;
   f := GetItemFromList(VarientFields, VarientFieldNo) ;
   IF DebugVarients
   THEN
      IF IsRecord(f)
      THEN
         printf2('out list: record %d is %d\n', VarientFieldNo, f)
      ELSE
         printf2('out list: varient field %d is %d\n', VarientFieldNo, f)
      END
   END ;
   RETURN( f )
END GetRecordOrField ;


(*
   BeginVarient - begin a varient record.
*)

PROCEDURE BeginVarient ;
VAR
   r, v: CARDINAL ;
BEGIN
   r := GetRecordOrField() ;
   Assert(IsRecord(r) OR IsFieldVarient(r)) ;
   v := GetRecordOrField() ;
   Assert(IsVarient(v)) ;
   BuildRange(InitCaseBounds(PushCase(r, v, NulSym)))
END BeginVarient ;


(*
   EndVarient - end a varient record.
*)

PROCEDURE EndVarient ;
BEGIN
   PopCase
END EndVarient ;


(*
   ElseVarient - associate an ELSE clause with a varient record.
*)

PROCEDURE ElseVarient ;
VAR
   f: CARDINAL ;
BEGIN
   f := GetRecordOrField() ;
   Assert(IsFieldVarient(f)) ;
   ElseCase(f)
END ElseVarient ;



(*
   BeginVarientList - begin an ident list containing ranges belonging to a
                      varient list.
*)

PROCEDURE BeginVarientList ;
VAR
   f: CARDINAL ;
BEGIN
   f := GetRecordOrField() ;
   Assert(IsFieldVarient(f)) ;
   BeginCaseList(f)
END BeginVarientList ;


(*
   EndVarientList - end a range list for a varient field.
*)

PROCEDURE EndVarientList ;
BEGIN
   EndCaseList
END EndVarientList ;


(*
   AddVarientRange - creates a range from the top two contant expressions
                     on the stack which are recorded with the current
                     varient field.  The stack is unaltered.
*)

PROCEDURE AddVarientRange ;
VAR
   r1, r2: CARDINAL ;
BEGIN
   PopT(r2) ;
   PopT(r1) ;
   AddRange(r1, r2, GetTokenNo())
END AddVarientRange ;


(*
   AddVarientEquality - adds the contant expression on the top of the stack
                        to the current varient field being recorded.
                        The stack is unaltered.
*)

PROCEDURE AddVarientEquality ;
VAR
   r1: CARDINAL ;
BEGIN
   PopT(r1) ;
   AddRange(r1, NulSym, GetTokenNo())
END AddVarientEquality ;


(*
   BuildAsmElement - the stack is expected to contain:


                        Entry                      Exit
                        =====                      ====

                 Ptr ->
                        +------------------+
                        | expr | tokpos    |
                        |------------------|
                        | str              |
                        |------------------|
                        | name             |
                        |------------------|       +------------------+
                        | CurrentInterface |       | CurrentInterface |
                        |------------------|       |------------------|
                        | CurrentAsm       |       | CurrentAsm       |
                        |------------------|       |------------------|
                        | n                |       | n                |
                        |------------------|       |------------------|
*)

PROCEDURE BuildAsmElement (input, output: BOOLEAN) ;
CONST
   DebugAsmTokPos = FALSE ;
VAR
   s                   : String ;
   n, str, expr, tokpos,
   CurrentInterface,
   CurrentAsm, name    : CARDINAL ;
BEGIN
   PopTtok (expr, tokpos) ;
   PopT (str) ;
   PopT (name) ;
   PopT (CurrentInterface) ;
   PopT (CurrentAsm) ;
   Assert (IsGnuAsm (CurrentAsm) OR IsGnuAsmVolatile (CurrentAsm)) ;
   PopT (n) ;
   INC (n) ;
   IF CurrentInterface = NulSym
   THEN
      CurrentInterface := MakeRegInterface ()
   END ;
   IF input
   THEN
      PutRegInterface (tokpos, CurrentInterface, n, name, str, expr,
                       NextQuad, 0) ;
      IF DebugAsmTokPos
      THEN
         s := InitString ('input expression') ;
         WarnStringAt (s, tokpos)
      END
   END ;
   IF output
   THEN
      PutRegInterface (tokpos, CurrentInterface, n, name, str, expr,
                       0, NextQuad) ;
      IF DebugAsmTokPos
      THEN
         s := InitString ('output expression') ;
         WarnStringAt (s, tokpos)
      END
   END ;
   PushT (n) ;
   PushT (CurrentAsm) ;
   PushT (CurrentInterface)
END BuildAsmElement ;


(*
   BuildAsmTrash - the stack is expected to contain:


                        Entry                      Exit
                        =====                      ====

                 Ptr ->
                        +------------------+
                        | expr | tokpos    |
                        |------------------|       +------------------+
                        | CurrentInterface |       | CurrentInterface |
                        |------------------|       |------------------|
                        | CurrentAsm       |       | CurrentAsm       |
                        |------------------|       |------------------|
                        | n                |       | n                |
                        |------------------|       |------------------|
*)

PROCEDURE BuildAsmTrash ;
VAR
   n, expr, tokpos,
   CurrentInterface,
   CurrentAsm      : CARDINAL ;
BEGIN
   PopTtok (expr, tokpos) ;
   PopT (CurrentInterface) ;
   PopT (CurrentAsm) ;
   Assert (IsGnuAsm (CurrentAsm) OR IsGnuAsmVolatile (CurrentAsm)) ;
   PopT (n) ;
   INC (n) ;
   IF CurrentInterface = NulSym
   THEN
      CurrentInterface := MakeRegInterface ()
   END ;
   PutRegInterface (tokpos, CurrentInterface, n, NulName, NulSym, expr,
                    0, NextQuad) ;
   PushT (n) ;
   PushT (CurrentAsm) ;
   PushT (CurrentInterface)
END BuildAsmTrash ;


(*
   IncOperandD - increment the dimension number associated with symbol
                 at, pos, on the boolean stack.
*)

(*
PROCEDURE IncOperandD (pos: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PeepAddress(BoolStack, pos) ;
   INC(f^.Dimension)
END IncOperandD ;
*)


(*
   PushTFA - Push True, False, Array, numbers onto the
             True/False stack.  True and False are assumed to
             contain Symbols or Ident etc.
*)

PROCEDURE PushTFA (True, False, Array: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := Array
   END ;
   PushAddress(BoolStack, f)
END PushTFA ;


(*
   PushTFAD - Push True, False, Array, Dim, numbers onto the
              True/False stack.  True and False are assumed to
              contain Symbols or Ident etc.
*)

PROCEDURE PushTFAD (True, False, Array, Dim: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := Array ;
      Dimension := Dim
   END ;
   PushAddress(BoolStack, f)
END PushTFAD ;


(*
   PushTFADtok - Push True, False, Array, Dim, numbers onto the
                 True/False stack.  True and False are assumed to
                 contain Symbols or Ident etc.
*)

PROCEDURE PushTFADtok (True, False, Array, Dim: WORD; tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := Array ;
      Dimension := Dim ;
      tokenno := tokno
   END ;
   PushAddress (BoolStack, f)
END PushTFADtok ;


(*
   PushTFADrwtok - Push True, False, Array, Dim, rw, numbers onto the
                   True/False stack.  True and False are assumed to
                   contain Symbols or Ident etc.
*)

PROCEDURE PushTFADrwtok (True, False, Array, Dim, rw: WORD; Tok: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := Array ;
      Dimension := Dim ;
      ReadWrite := rw ;
      tokenno := Tok
   END ;
   PushAddress (BoolStack, f)
END PushTFADrwtok ;


(*
   PopTFrwtok - Pop a True and False number from the True/False stack.
                True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTFrwtok (VAR True, False, rw: WORD; VAR tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Assert(NOT BooleanOp) ;
      rw := ReadWrite ;
      tokno := tokenno
   END ;
   DISPOSE(f)
END PopTFrwtok ;


(*
   PushTFrwtok - Push an item onto the stack in the T (true) position,
                 it is assummed to be a token and its token location is recorded.
*)

PROCEDURE PushTFrwtok (True, False, rw: WORD; tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      ReadWrite := rw ;
      tokenno := tokno
   END ;
   PushAddress(BoolStack, f)
END PushTFrwtok ;


(*
   PushTFDtok - Push True, False, Dim, numbers onto the
                True/False stack.  True and False are assumed to
                contain Symbols or Ident etc.
*)

PROCEDURE PushTFDtok (True, False, Dim: WORD; Tok: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Dimension := Dim ;
      tokenno := Tok
   END ;
   PushAddress (BoolStack, f)
END PushTFDtok ;


(*
   PopTFDtok - Pop a True, False, Dim number from the True/False stack.
               True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTFDtok (VAR True, False, Dim: WORD; VAR Tok: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Dim := Dimension ;
      Tok := tokenno ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopTFDtok ;


(*
   PushTFDrwtok - Push True, False, Dim, numbers onto the
                  True/False stack.  True and False are assumed to
                  contain Symbols or Ident etc.
*)

PROCEDURE PushTFDrwtok (True, False, Dim, rw: WORD; Tok: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Dimension := Dim ;
      ReadWrite := rw ;
      tokenno := Tok
   END ;
   PushAddress (BoolStack, f)
END PushTFDrwtok ;


(*
   PushTFrw - Push a True and False numbers onto the True/False stack.
              True and False are assumed to contain Symbols or Ident etc.
              It also pushes the higher level symbol which is associated
              with the True symbol.  Eg record variable or array variable.
*)

PROCEDURE PushTFrw (True, False: WORD; rw: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      ReadWrite := rw
   END ;
   PushAddress(BoolStack, f)
END PushTFrw ;


(*
   PopTFrw - Pop a True and False number from the True/False stack.
             True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTFrw (VAR True, False, rw: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Assert(NOT BooleanOp) ;
      rw := ReadWrite
   END ;
   DISPOSE(f)
END PopTFrw ;


(*
   PushTF - Push a True and False numbers onto the True/False stack.
            True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PushTF (True, False: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False
   END ;
   PushAddress(BoolStack, f)
END PushTF ;


(*
   PopTF - Pop a True and False number from the True/False stack.
           True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTF (VAR True, False: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopTF ;


(*
   DupFrame - duplicate the top of stack and push the new frame.
*)

PROCEDURE DupFrame ;
VAR
   f, newf: BoolFrame ;
BEGIN
   f := PopAddress (BoolStack) ;
   PushAddress (BoolStack, f) ;
   newf := newBoolFrame () ;
   newf^ := f^ ;
   PushAddress (BoolStack, newf)
END DupFrame ;


(*
   newBoolFrame - creates a new BoolFrame with all fields initialised to their defaults.
*)

PROCEDURE newBoolFrame () : BoolFrame ;
VAR
   f: BoolFrame ;
BEGIN
   NEW (f) ;
   WITH f^ DO
      TrueExit   := 0 ;
      FalseExit  := 0 ;
      Unbounded  := NulSym ;
      BooleanOp  := FALSE ;
      Dimension  := 0 ;
      ReadWrite  := NulSym ;
      name       := NulSym ;
      Annotation := NIL ;
      tokenno    := UnknownTokenNo
   END ;
   RETURN f
END newBoolFrame ;


(*
   PushTtok - Push an item onto the stack in the T (true) position,
              it is assummed to be a token and its token location is recorded.
*)

PROCEDURE PushTtok (True: WORD; tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   (* PrintTokenNo (tokno) ; *)
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      tokenno := tokno
   END ;
   PushAddress (BoolStack, f)
END PushTtok ;


(*
   PushT - Push an item onto the stack in the T (true) position.
*)

PROCEDURE PushT (True: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True
   END ;
   PushAddress (BoolStack, f)
END PushT ;


(*
   PopT - Pops the T value from the stack.
*)

PROCEDURE PopT (VAR True: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress (BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopT ;


(*
   PopTtok - Pops the T value from the stack and token position.
*)

PROCEDURE PopTtok (VAR True: WORD; VAR tok: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      tok := tokenno ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopTtok ;


(*
   PushTrw - Push an item onto the True/False stack. The False value will be zero.
*)

(*
PROCEDURE PushTrw (True: WORD; rw: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      ReadWrite := rw
   END ;
   PushAddress(BoolStack, f)
END PushTrw ;
*)


(*
   PushTrwtok - Push an item onto the True/False stack. The False value will be zero.
*)

PROCEDURE PushTrwtok (True: WORD; rw: WORD; tok: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      ReadWrite := rw ;
      tokenno := tok
   END ;
   PushAddress(BoolStack, f)
END PushTrwtok ;


(*
   PopTrw - Pop a True field and rw symbol from the stack.
*)

PROCEDURE PopTrw (VAR True, rw: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      Assert(NOT BooleanOp) ;
      rw := ReadWrite
   END ;
   DISPOSE(f)
END PopTrw ;


(*
   PopTrwtok - Pop a True field and rw symbol from the stack.
*)

PROCEDURE PopTrwtok (VAR True, rw: WORD; VAR tok: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      Assert(NOT BooleanOp) ;
      rw := ReadWrite ;
      tok := tokenno
   END ;
   DISPOSE(f)
END PopTrwtok ;


(*
   PushTFn - Push a True and False numbers onto the True/False stack.
             True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PushTFn (True, False, n: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit  := True ;
      FalseExit := False ;
      name      := n
   END ;
   PushAddress(BoolStack, f)
END PushTFn ;


(*
   PushTFntok - Push a True and False numbers onto the True/False stack.
                True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PushTFntok (True, False, n: WORD; tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit  := True ;
      FalseExit := False ;
      name      := n ;
      tokenno   := tokno
   END ;
   PushAddress (BoolStack, f)
END PushTFntok ;


(*
   PopTFn - Pop a True and False number from the True/False stack.
            True and False are assumed to contain Symbols or Ident etc.
*)

PROCEDURE PopTFn (VAR True, False, n: WORD) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      n := name ;
      Assert(NOT BooleanOp)
   END ;
   DISPOSE(f)
END PopTFn ;


(*
   PopNothing - pops the top element on the boolean stack.
*)

PROCEDURE PopNothing ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   DISPOSE(f)
END PopNothing ;


(*
   PopN - pops multiple elements from the BoolStack.
*)

PROCEDURE PopN (n: CARDINAL) ;
BEGIN
   WHILE n>0 DO
      PopNothing ;
      DEC(n)
   END
END PopN ;


(*
   PushTFtok - Push an item onto the stack in the T (true) position,
               it is assummed to be a token and its token location is recorded.
*)

PROCEDURE PushTFtok (True, False: WORD; tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      tokenno := tokno
   END ;
   PushAddress(BoolStack, f)
END PushTFtok ;


(*
   PopTFtok - Pop T/F/tok from the stack.
*)

PROCEDURE PopTFtok (VAR True, False: WORD; VAR tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := PopAddress(BoolStack) ;
   WITH f^ DO
      True := TrueExit ;
      False := FalseExit ;
      tokno := tokenno
   END
END PopTFtok ;


(*
   PushTFAtok - Push T/F/A/tok to the stack.
*)

PROCEDURE PushTFAtok (True, False, Array: WORD; tokno: CARDINAL) ;
VAR
   f: BoolFrame ;
BEGIN
   f := newBoolFrame () ;
   WITH f^ DO
      TrueExit := True ;
      FalseExit := False ;
      Unbounded := Array ;
      tokenno := tokno
   END ;
   PushAddress(BoolStack, f)
END PushTFAtok ;


(*
   Top - returns the no of items held in the stack.
*)

PROCEDURE Top () : CARDINAL ;
BEGIN
   RETURN( NoOfItemsInStackAddress(BoolStack) )
END Top ;


(*
   PushAutoOn - push the auto flag and then set it to TRUE.
                Any call to ident in the parser will result in the token being pushed.
*)

PROCEDURE PushAutoOn ;
BEGIN
   PushWord(AutoStack, IsAutoOn) ;
   IsAutoOn := TRUE
END PushAutoOn ;


(*
   PushAutoOff - push the auto flag and then set it to FALSE.
*)

PROCEDURE PushAutoOff ;
BEGIN
   PushWord(AutoStack, IsAutoOn) ;
   IsAutoOn := FALSE
END PushAutoOff ;


(*
   IsAutoPushOn - returns the value of the current Auto ident push flag.
*)

PROCEDURE IsAutoPushOn () : BOOLEAN ;
BEGIN
   RETURN( IsAutoOn )
END IsAutoPushOn ;


(*
   PopAuto - restores the previous value of the Auto flag.
*)

PROCEDURE PopAuto ;
BEGIN
   IsAutoOn := PopWord(AutoStack)
END PopAuto ;


(*
   PushInConstExpression - push the InConstExpression flag and then set it to TRUE.
*)

PROCEDURE PushInConstExpression ;
BEGIN
   PushWord(ConstExprStack, InConstExpression) ;
   InConstExpression := TRUE
END PushInConstExpression ;


(*
   PopInConstExpression - restores the previous value of the InConstExpression.
*)

PROCEDURE PopInConstExpression ;
BEGIN
   InConstExpression := PopWord(ConstExprStack)
END PopInConstExpression ;


(*
   IsInConstExpression - returns the value of the InConstExpression.
*)

PROCEDURE IsInConstExpression () : BOOLEAN ;
BEGIN
   RETURN( InConstExpression )
END IsInConstExpression ;


(*
   PushInConstParameters - push the InConstParameters flag and then set it to TRUE.
*)

PROCEDURE PushInConstParameters ;
BEGIN
   PushWord (ConstParamStack, InConstParameters) ;
   InConstParameters := TRUE
END PushInConstParameters ;


(*
   PopInConstParameters - restores the previous value of the InConstParameters.
*)

PROCEDURE PopInConstParameters ;
BEGIN
   InConstParameters := PopWord(ConstParamStack)
END PopInConstParameters ;


(*
   IsInConstParameters - returns the value of the InConstParameters.
*)

PROCEDURE IsInConstParameters () : BOOLEAN ;
BEGIN
   RETURN( InConstParameters )
END IsInConstParameters ;


(*
   MustCheckOverflow - returns TRUE if the quadruple should test for overflow.
*)

PROCEDURE MustCheckOverflow (q: CARDINAL) : BOOLEAN ;
VAR
   f: QuadFrame ;
BEGIN
   f := GetQF(q) ;
   RETURN( f^.CheckOverflow )
END MustCheckOverflow ;


(*
   StressStack -
*)

(*
PROCEDURE StressStack ;
CONST
   Maxtries = 1000 ;
VAR
   n, i, j: CARDINAL ;
BEGIN
   PushT(1) ;
   PopT(i) ;
   Assert(i=1) ;
   FOR n := 1 TO Maxtries DO
      FOR i := n TO 1 BY -1 DO
         PushT(i)
      END ;
      FOR i := n TO 1 BY -1 DO
         Assert(OperandT(i)=i)
      END ;
      FOR i := 1 TO n DO
         Assert(OperandT(i)=i)
      END ;
      FOR i := 1 TO n BY 10 DO
         Assert(OperandT(i)=i)
      END ;
      IF (n>1) AND (n MOD 2 = 0)
      THEN
         FOR i := 1 TO n DIV 2 DO
            PopT(j) ;
            Assert(j=i)
         END ;
         FOR i := n DIV 2 TO 1 BY -1 DO
            PushT(i)
         END
      END ;
      FOR i := 1 TO n DO
         PopT(j) ;
         Assert(j=i)
      END
   END
END StressStack ;
*)


(*
   Init - initialize the M2Quads module, all the stacks, all the lists
          and the quads list.
*)

PROCEDURE Init ;
BEGIN
   LogicalOrTok := MakeKey('_LOR') ;
   LogicalAndTok := MakeKey('_LAND') ;
   LogicalXorTok := MakeKey('_LXOR') ;
   LogicalDifferenceTok := MakeKey('_LDIFF') ;
   ArithPlusTok := MakeKey ('_ARITH_+') ;
   QuadArray := InitIndexTuned (1, 1024*1024 DIV 16, 16) ;
   FreeList := 1 ;
   NewQuad(NextQuad) ;
   Assert(NextQuad=1) ;
   BoolStack := InitStackAddress() ;
   ExitStack := InitStackWord() ;
   RepeatStack := InitStackWord() ;
   WhileStack := InitStackWord() ;
   ForStack := InitStackWord() ;
   WithStack := InitStackAddress() ;
   ReturnStack := InitStackWord() ;
   LineStack := InitStackAddress() ;
   PriorityStack := InitStackWord() ;
   TryStack := InitStackWord() ;
   CatchStack := InitStackWord() ;
   ExceptStack := InitStackWord() ;
   ConstructorStack := InitStackAddress() ;
   ConstParamStack := InitStackWord () ;
   ConstExprStack := InitStackWord () ;
   (* StressStack ; *)
   SuppressWith := FALSE ;
   Head := 1 ;
   LastQuadNo := 0 ;
   MustNotCheckBounds := FALSE ;
   InitQuad := 0 ;
   GrowInitialization := 0 ;
   ForInfo := InitIndex (1) ;
   QuadrupleGeneration := TRUE ;
   BuildingHigh := FALSE ;
   BuildingSize := FALSE ;
   AutoStack := InitStackWord() ;
   IsAutoOn := TRUE ;
   InConstExpression := FALSE ;
   InConstParameters := FALSE ;
   FreeLineList := NIL ;
   InitList(VarientFields) ;
   VarientFieldNo := 0 ;
   NoOfQuads := 0 ;
END Init ;


BEGIN
   Init
END M2Quads.
