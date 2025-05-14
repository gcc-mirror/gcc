(* M2GCCDeclare.mod declares Modula-2 types to GCC.

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

IMPLEMENTATION MODULE M2GCCDeclare ;

(*
    Title      : M2GCCDeclare
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Fri Jul 16 20:10:55 1999
    Description: declares Modula-2 types to GCC, it attempts
                 to only declare a type once all subcomponents are known.
*)

FROM SYSTEM IMPORT ADDRESS, ADR, WORD ;
FROM ASCII IMPORT nul ;
FROM Storage IMPORT ALLOCATE ;
FROM M2Debug IMPORT Assert ;
FROM M2Quads IMPORT DisplayQuadRange ;
FROM m2pp IMPORT DumpGimpleFd ;

IMPORT FIO ;

FROM M2Options IMPORT GenerateDebugging, GenerateLineDebug, Iso, Optimizing, WholeProgram,
                      ScaffoldStatic, GetRuntimeModuleOverride ;

FROM M2AsmUtil IMPORT GetFullSymName, GetFullScopeAsmName ;

FROM M2Batch IMPORT MakeDefinitionSource ;
FROM NameKey IMPORT Name, MakeKey, NulName, KeyToCharStar, makekey ;
FROM M2FileName IMPORT CalculateFileName ;
FROM DynamicStrings IMPORT String, string, InitString, KillString, InitStringCharStar, InitStringChar, Mark ;
FROM M2LexBuf IMPORT TokenToLineNo, FindFileNameFromToken, TokenToLocation, UnknownTokenNo, BuiltinTokenNo ;
FROM M2MetaError IMPORT MetaError1, MetaError2, MetaError3 ;
FROM M2Error IMPORT FlushErrors, InternalError ;
FROM M2LangDump IMPORT GetDumpFile ;

FROM M2Printf IMPORT printf0, printf1, printf2, printf3,
                     fprintf0, fprintf1, fprintf2, fprintf3 ;

FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, InBounds,
                     IncludeIndiceIntoIndex, HighIndice,
                     DebugIndex ;

FROM Lists IMPORT List, InitList, IncludeItemIntoList,
                  PutItemIntoList, GetItemFromList,
                  RemoveItemFromList, ForeachItemInListDo,
      	       	  IsItemInList, NoOfItemsInList, KillList ;

FROM Sets IMPORT Set, InitSet, KillSet,
                 IncludeElementIntoSet, ExcludeElementFromSet,
                 NoOfElementsInSet, IsElementInSet, ForeachElementInSetDo,
                 DuplicateSet, EqualSet ;

FROM M2BasicBlock IMPORT BasicBlock, InitBasicBlocks, KillBasicBlocks, ForeachBasicBlockDo ;

FROM SymbolTable IMPORT NulSym,
                        ModeOfAddr, ProcedureKind,
                        GetProcedureKindDesc,
                        GetProcedureParametersDefined,
                        GetMode,
                        GetScope,
                        GetNth, SkipType, GetVarBackEndType,
			GetSType, GetLType, GetDType,
                        MakeType, PutType, GetLowestType,
      	       	     	GetSubrange, PutSubrange, GetArraySubscript,
      	       	     	NoOfParamAny, GetNthParamAny,
                        PushValue, PopValue, PopSize,
                        IsTemporary, IsUnbounded, IsPartialUnbounded,
                        IsEnumeration, IsVar,
      	       	     	IsSubrange, IsPointer, IsRecord, IsArray,
                        IsFieldEnumeration,
                        IsProcedure, IsProcedureNested, IsModule,
                        IsDefImp,
      	       	     	IsSubscript, IsVarient, IsFieldVarient,
      	       	     	IsType, IsProcType, IsSet, IsSetPacked,
                        IsConst, IsConstSet, IsConstructor,
                        IsFieldEnumeration,
                        IsExported, IsImported,
                        IsVarParamAny, IsRecordField, IsUnboundedParam,
                        IsValueSolved,
                        IsDefinitionForC, IsHiddenTypeDeclared,
                        IsInnerModule, IsUnknown,
                        IsProcedureReachable, IsParameter, IsConstLit,
                        IsDummy, IsVarAParam, IsProcedureVariable,
                        IsGnuAsm, IsGnuAsmVolatile, IsObject, IsTuple,
                        IsError, IsHiddenType, IsVarHeap,
                        IsComponent, IsPublic, IsExtern, IsCtor,
                        IsImport, IsImportStatement, IsConstStringKnown,
                        IsUnboundedParamAny,
      	       	     	GetMainModule, GetBaseModule, GetModule, GetLocalSym,
                        PutModuleFinallyFunction,
                        GetProcedureScope, GetProcedureQuads,
                        NoOfParam, IsVarParam, GetNthParam, GetType,
                        IsRecordFieldAVarientTag, IsEmptyFieldVarient,
                        GetVarient, GetUnbounded, PutArrayLarge,
                        IsAModula2Type, UsesVarArgs,
                        GetSymName, GetParent,
                        GetDeclaredMod, GetVarBackEndType,
                        GetProcedureBeginEnd, IsProcedureAnyNoReturn,
                        GetString, GetStringLength, IsConstString,
                        IsConstStringM2, IsConstStringC, IsConstStringM2nul, IsConstStringCnul,
                        GetAlignment, IsDeclaredPacked, PutDeclaredPacked,
                        GetDefaultRecordFieldAlignment, IsDeclaredPackedResolved,
                        GetPackedEquivalent,
                        GetParameterShadowVar,
                        GetUnboundedRecordType,
                        GetModuleCtors, GetProcedureProcType,
                        MakeSubrange, MakeConstVar, MakeConstLit,
                        PutConst,
			ForeachOAFamily, GetOAFamily,
                        IsModuleWithinProcedure, IsVariableSSA,
                        IsVariableAtAddress, IsConstructorConstant,
                        ForeachLocalSymDo,
      	       	     	ForeachProcedureDo, ForeachModuleDo,
                        ForeachInnerModuleDo, ForeachImportedDo,
                        ForeachExportedDo, PrintInitialized,
                        FinalSymbol ;

FROM M2Base IMPORT IsPseudoBaseProcedure, IsPseudoBaseFunction,
                   GetBaseTypeMinMax, MixTypes,
                   Cardinal, Char, Proc, Integer,
                   LongInt, LongCard, ShortCard, ShortInt,
                   Real, LongReal, ShortReal, ZType, RType,
                   CType, Complex, LongComplex, ShortComplex,
                   Boolean, True, False, Nil,
                   IsRealType, IsNeededAtRunTime, IsComplexType ;

FROM M2System IMPORT IsPseudoSystemFunction, IsSystemType, IsRealN,
                     GetSystemTypeMinMax, Address, Word, Byte, Loc,
                     System, IntegerN, CardinalN, WordN, RealN, SetN, ComplexN,
		     CSizeT, CSSizeT, COffT ;

FROM M2Bitset IMPORT Bitset, Bitnum ;
FROM SymbolConversion IMPORT AddModGcc, Mod2Gcc, GccKnowsAbout, Poison, RemoveMod2Gcc ;
FROM M2GenGCC IMPORT ResolveConstantExpressions ;
FROM M2Scope IMPORT ScopeBlock, InitScopeBlock, KillScopeBlock, ForeachScopeBlockDo3 ;

FROM M2ALU IMPORT Addn, Sub, Equ, GreEqu, Gre, Less, PushInt, PushCard, ConvertToType,
                  PushIntegerTree, PopIntegerTree, PopRealTree, ConvertToInt, PopSetTree,
                  PopChar,
                  DivTrunc,
                  IsConstructorDependants, WalkConstructorDependants,
                  PopConstructorTree, PopComplexTree, PutConstructorSolved,
                  ChangeToConstructor, EvaluateValue, TryEvaluateValue ;

FROM M2Batch IMPORT IsSourceSeen, GetModuleFile, IsModuleSeen, LookupModule ;
FROM gcctypes IMPORT location_t, tree ;
FROM m2linemap IMPORT BuiltinsLocation ;

FROM m2decl IMPORT BuildIntegerConstant, BuildStringConstant, BuildCStringConstant,
                   BuildStartFunctionDeclaration,
                   BuildParameterDeclaration, BuildEndFunctionDeclaration,
                   DeclareKnownVariable, GetBitsPerBitset, BuildPtrToTypeString ;

FROM m2type IMPORT MarkFunctionReferenced, BuildStartRecord, BuildStartVarient, BuildStartFunctionType,
                   BuildStartFieldVarient, BuildStartVarient, BuildStartType, BuildStartArrayType,
                   PutArrayType, BuildPointerType, BuildEndType, BuildCharConstant,
                   BuildTypeDeclaration, GetDefaultType, GetBooleanType, GetBooleanTrue,
                   GetBooleanFalse, BuildSubrangeType, GetM2ZType, GetM2RType, GetM2CType,
                   GetM2CardinalType, GetM2IntegerType, GetM2CharType, GetISOLocType, GetIntegerType,
                   GetISOByteType, GetISOWordType, GetByteType, GetWordType, GetProcType, GetPointerType,
                   GetM2LongIntType, GetM2LongCardType, GetM2ShortIntType, GetM2ShortCardType,
                   GetM2LongRealType, GetM2ShortRealType, GetM2RealType, GetBitnumType, GetBitsetType,
                   GetM2ComplexType, GetM2ComplexType, GetM2LongComplexType, GetM2ShortComplexType,
                   GetM2Integer8, GetM2Integer16, GetM2Integer32, GetM2Integer64, GetM2Cardinal8,
                   GetM2Cardinal16, GetM2Cardinal32, GetM2Cardinal64, GetM2Word16, GetM2Word32,
                   GetM2Word64, GetM2Bitset8, GetM2Bitset16, GetM2Bitset32, GetM2Real32, GetM2Real64,
                   GetM2Real96, GetM2Real128, GetM2Complex32, GetM2Complex64, GetM2Complex96,
                   GetM2Complex128, GetCSizeTType, GetCSSizeTType, GetCOffTType,
		   GetPackedBooleanType, BuildConstPointerType,
                   BuildPointerType, BuildEnumerator, BuildStartEnumeration, BuildEndEnumeration,
                   SetAlignment, SetTypePacked, SetDeclPacked, BuildSmallestTypeRange,
                   SetRecordFieldOffset, ChainOn, BuildEndRecord, BuildFieldRecord,
                   BuildEndFieldVarient, BuildArrayIndexType, BuildEndFunctionType,
                   BuildSetType, BuildEndVarient, BuildEndArrayType, InitFunctionTypeParameters,
                   BuildProcTypeParameterDeclaration, DeclareKnownType,
                   ValueOutOfTypeRange, ExceedsTypeRange,
                   GetMaxFrom, GetMinFrom ;

FROM m2convert IMPORT BuildConvert ;

FROM m2expr IMPORT BuildSub, BuildLSL, BuildTBitSize, BuildAdd, BuildDivTrunc, BuildModTrunc,
                   BuildSize, TreeOverflow, AreConstantsEqual, CompareTrees,
                   GetPointerZero, GetIntegerZero, GetIntegerOne ;

FROM m2block IMPORT RememberType, pushGlobalScope, popGlobalScope,
                    pushFunctionScope, popFunctionScope,
                    finishFunctionDecl, RememberConstant, GetGlobalContext ;


TYPE
   StartProcedure = PROCEDURE (location_t, ADDRESS) : tree ;
   ListType       = (fullydeclared, partiallydeclared, niltypedarrays,
                     heldbyalignment, finishedalignment, todolist,
                     tobesolvedbyquads, finishedsetarray) ;
   doDeclareProcedure = PROCEDURE (CARDINAL, CARDINAL) ;

CONST
   Debugging       = FALSE ;
   Progress        = FALSE ;
   EnableSSA       = FALSE ;
   EnableWatch     = TRUE ;
   TraceQuadruples = FALSE ;

TYPE
   Group = POINTER TO RECORD
                         ToBeSolvedByQuads,               (* Constants which must be solved *)
                                                          (* by processing the quadruples.  *)
                         FinishedSetArray,                (* Sets which have had their set  *)
                                                          (* array created.                 *)
                         NilTypedArrays,                  (* Arrays which have NIL as their *)
                                                          (* type.                          *)
                         FullyDeclared,                   (* Those symbols which have been  *)
                                                          (* fully declared.                *)
                         PartiallyDeclared,               (* Those types which have need to *)
                                                          (* be finished (but already       *)
                                                          (* started: records, function     *)
                                                          (* and array type).               *)
                         HeldByAlignment,                 (* Types which have a user        *)
                                                          (* specified alignment constant.  *)
                         FinishedAlignment,               (* Records for which we know      *)
                                                          (* their alignment value.         *)
                         ToDoList            : Set ;      (* Contains a set of all          *)
                                                          (* outstanding types that need to *)
                                                          (* be declared to GCC once        *)
                                                          (* its dependants have            *)
                                                          (* been written.                  *)
                         Next                : Group ;
                      END ;


VAR
   FreeGroup,
   GlobalGroup         : Group ;    (* The global group of all sets.  *)
   VisitedList,
   ChainedList         : Set ;
   HaveInitDefaultTypes: BOOLEAN ;  (* Have we initialized them yet?  *)
   WatchList           : Set ;      (* Set of symbols being watched.  *)
   EnumerationIndex    : Index ;
   action              : IsAction ;
   ConstantResolved,
   enumDeps            : BOOLEAN ;


PROCEDURE mystop ; BEGIN END mystop ;


(* *************************************************** *)
(*
   PrintNum -
*)

PROCEDURE PrintNum (sym: WORD) ;
BEGIN
   printf1 ('%d, ', sym)
END PrintNum ;


(*
   DebugSet -
*)

PROCEDURE DebugSet (a: ARRAY OF CHAR; l: Set) ;
BEGIN
   printf0 (a) ;
   printf0 (' {') ;
   ForeachElementInSetDo (l, PrintNum) ;
   printf0 ('}\n')
END DebugSet ;


(*
   DebugSets -
*)

PROCEDURE DebugSets ;
BEGIN
   DebugSet ('ToDoList', GlobalGroup^.ToDoList) ;
   DebugSet ('HeldByAlignment', GlobalGroup^.HeldByAlignment) ;
   DebugSet ('FinishedAlignment', GlobalGroup^.FinishedAlignment) ;
   DebugSet ('PartiallyDeclared', GlobalGroup^.PartiallyDeclared) ;
   DebugSet ('FullyDeclared', GlobalGroup^.FullyDeclared) ;
   DebugSet ('NilTypedArrays', GlobalGroup^.NilTypedArrays) ;
   DebugSet ('ToBeSolvedByQuads', GlobalGroup^.ToBeSolvedByQuads) ;
   DebugSet ('FinishedSetArray', GlobalGroup^.FinishedSetArray)
END DebugSets ;
(* ************************************************ *)


(*
   DebugNumber -
*)

PROCEDURE DebugNumber (a: ARRAY OF CHAR; s: Set) ;
VAR
   n: CARDINAL ;
BEGIN
   n := NoOfElementsInSet (s) ;
   printf1 (a, n) ;
   FIO.FlushBuffer (FIO.StdOut)
END DebugNumber ;


(*
   DebugSets -
*)

PROCEDURE DebugSetNumbers ;
BEGIN
   DebugNumber ('ToDoList : %d\n', GlobalGroup^.ToDoList) ;
   DebugNumber ('HeldByAlignment : %d\n', GlobalGroup^.HeldByAlignment) ;
   DebugNumber ('PartiallyDeclared : %d\n', GlobalGroup^.PartiallyDeclared) ;
   DebugNumber ('FullyDeclared : %d\n', GlobalGroup^.FullyDeclared) ;
   DebugNumber ('NilTypedArrays : %d\n', GlobalGroup^.NilTypedArrays) ;
   DebugNumber ('ToBeSolvedByQuads : %d\n', GlobalGroup^.ToBeSolvedByQuads) ;
   DebugNumber ('FinishedSetArray : %d\n', GlobalGroup^.FinishedSetArray)
END DebugSetNumbers ;


(*
   AddSymToWatch - adds symbol, sym, to the list of symbols
                   to watch and annotate their movement between
                   lists.
*)

PROCEDURE AddSymToWatch (sym: WORD) ;
BEGIN
   IF (sym # NulSym) AND (NOT IsElementInSet (WatchList, sym))
   THEN
      IncludeElementIntoSet (WatchList, sym) ;
      WalkDependants (sym, AddSymToWatch) ;
      fprintf1 (GetDumpFile (), "%d, ", sym)
   END
END AddSymToWatch ;


(*
   TryFindSymbol -
*)

(*
PROCEDURE TryFindSymbol (module, symname: ARRAY OF CHAR) : CARDINAL ;
VAR
   mn, sn: Name ;
   mod   : CARDINAL ;
BEGIN
   mn := MakeKey(module) ;
   sn := MakeKey(symname) ;
   IF IsModuleSeen(mn)
   THEN
      mod := LookupModule (UnknownTokenNo, mn) ;
      RETURN( GetLocalSym(mod, sn) )
   ELSE
      RETURN( NulSym )
   END
END TryFindSymbol ;
*)


(*
   doInclude -
*)

PROCEDURE doInclude (l: Set; a: ARRAY OF CHAR; sym: CARDINAL) ;
BEGIN
   IF NOT IsElementInSet(l, sym)
   THEN
      fprintf0 (GetDumpFile (), 'rule: ') ;
      WriteRule ;
      fprintf0 (GetDumpFile (), '  ') ;
      fprintf1 (GetDumpFile (), a, sym) ;
      IncludeElementIntoSet (l, sym)
   END
END doInclude ;


(*
   WatchIncludeList - include a symbol onto the set first checking
                      whether it is already on the set and
                      displaying a debug message if the set is
                      changed.
*)

PROCEDURE WatchIncludeList (sym: CARDINAL; lt: ListType) ;
BEGIN
   IF IsElementInSet (WatchList, sym)
   THEN
      CASE lt OF

      tobesolvedbyquads :  doInclude (GlobalGroup^.ToBeSolvedByQuads, "symbol %d -> ToBeSolvedByQuads\n", sym) |
      fullydeclared     :  doInclude (GlobalGroup^.FullyDeclared, "symbol %d -> FullyDeclared\n", sym) |
      partiallydeclared :  doInclude (GlobalGroup^.PartiallyDeclared, "symbol %d -> PartiallyDeclared\n", sym) |
      heldbyalignment   :  doInclude (GlobalGroup^.HeldByAlignment, "symbol %d -> HeldByAlignment\n", sym) |
      finishedalignment :  doInclude (GlobalGroup^.FinishedAlignment, "symbol %d -> FinishedAlignment\n", sym) |
      todolist          :  doInclude (GlobalGroup^.ToDoList, "symbol %d -> ToDoList\n", sym) |
      niltypedarrays    :  doInclude (GlobalGroup^.NilTypedArrays, "symbol %d -> NilTypedArrays\n", sym) |
      finishedsetarray  :  doInclude (GlobalGroup^.FinishedSetArray, "symbol %d -> FinishedSetArray\n", sym)

      ELSE
         InternalError ('unknown list')
      END
   ELSE
      CASE lt OF

      tobesolvedbyquads :  IncludeElementIntoSet (GlobalGroup^.ToBeSolvedByQuads, sym) |
      fullydeclared     :  IncludeElementIntoSet (GlobalGroup^.FullyDeclared, sym) |
      partiallydeclared :  IncludeElementIntoSet (GlobalGroup^.PartiallyDeclared, sym) |
      heldbyalignment   :  IncludeElementIntoSet (GlobalGroup^.HeldByAlignment, sym) |
      finishedalignment :  IncludeElementIntoSet (GlobalGroup^.FinishedAlignment, sym) |
      todolist          :  IncludeElementIntoSet (GlobalGroup^.ToDoList, sym) |
      niltypedarrays    :  IncludeElementIntoSet (GlobalGroup^.NilTypedArrays, sym) |
      finishedsetarray  :  IncludeElementIntoSet (GlobalGroup^.FinishedSetArray, sym)

      ELSE
         InternalError ('unknown list')
      END
   END
END WatchIncludeList ;


(*
   doExclude -
*)

PROCEDURE doExclude (l: Set; a: ARRAY OF CHAR; sym: CARDINAL) ;
BEGIN
   IF IsElementInSet (l, sym)
   THEN
      fprintf0 (GetDumpFile (), 'rule: ') ;
      WriteRule ;
      fprintf0 (GetDumpFile (), '  ') ;
      fprintf1 (GetDumpFile (), a, sym) ;
      ExcludeElementFromSet (l, sym)
   END
END doExclude ;


(*
   WatchRemoveList - remove a symbol onto the list first checking
                     whether it is already on the list and
                     displaying a debug message if the list is
                     changed.
*)

PROCEDURE WatchRemoveList (sym: CARDINAL; lt: ListType) ;
BEGIN
   IF IsElementInSet (WatchList, sym)
   THEN
      CASE lt OF

      tobesolvedbyquads :  doExclude (GlobalGroup^.ToBeSolvedByQuads, "symbol %d off ToBeSolvedByQuads\n", sym) |
      fullydeclared     :  doExclude (GlobalGroup^.FullyDeclared, "symbol %d off FullyDeclared\n", sym) |
      partiallydeclared :  doExclude (GlobalGroup^.PartiallyDeclared, "symbol %d off PartiallyDeclared\n", sym) |
      heldbyalignment   :  doExclude (GlobalGroup^.HeldByAlignment, "symbol %d -> HeldByAlignment\n", sym) |
      finishedalignment :  doExclude (GlobalGroup^.FinishedAlignment, "symbol %d -> FinishedAlignment\n", sym) |
      todolist          :  doExclude (GlobalGroup^.ToDoList, "symbol %d off ToDoList\n", sym) |
      niltypedarrays    :  doExclude (GlobalGroup^.NilTypedArrays, "symbol %d off NilTypedArrays\n", sym) |
      finishedsetarray  :  doExclude (GlobalGroup^.FinishedSetArray, "symbol %d off FinishedSetArray\n", sym) |

      ELSE
         InternalError ('unknown list')
      END
   ELSE
      CASE lt OF

      tobesolvedbyquads :  ExcludeElementFromSet (GlobalGroup^.ToBeSolvedByQuads, sym) |
      fullydeclared     :  ExcludeElementFromSet (GlobalGroup^.FullyDeclared, sym) |
      partiallydeclared :  ExcludeElementFromSet (GlobalGroup^.PartiallyDeclared, sym) |
      heldbyalignment   :  ExcludeElementFromSet (GlobalGroup^.HeldByAlignment, sym) |
      finishedalignment :  ExcludeElementFromSet (GlobalGroup^.FinishedAlignment, sym) |
      todolist          :  ExcludeElementFromSet (GlobalGroup^.ToDoList, sym) |
      niltypedarrays    :  ExcludeElementFromSet (GlobalGroup^.NilTypedArrays, sym) |
      finishedsetarray  :  ExcludeElementFromSet (GlobalGroup^.FinishedSetArray, sym) |

      ELSE
         InternalError ('unknown list')
      END
   END
END WatchRemoveList ;


(*
   NewGroup -
*)

PROCEDURE NewGroup (VAR g: Group) ;
BEGIN
   IF FreeGroup = NIL
   THEN
      NEW (g)
   ELSE
      g := FreeGroup ;
      FreeGroup := FreeGroup^.Next
   END
END NewGroup ;


(*
   DisposeGroup -
*)

PROCEDURE DisposeGroup (VAR g: Group) ;
BEGIN
   g^.Next := FreeGroup ;
   FreeGroup := g ;
   g := NIL
END DisposeGroup ;


(*
   InitGroup - initialize all sets in group and return the group.
*)

PROCEDURE InitGroup () : Group ;
VAR
   g: Group ;
BEGIN
   NewGroup (g) ;
   (* Initialize all sets in group.  *)
   WITH g^ DO
      FinishedSetArray := InitSet (1) ;
      ToDoList := InitSet (1) ;
      FullyDeclared := InitSet (1) ;
      PartiallyDeclared := InitSet (1) ;
      NilTypedArrays := InitSet (1) ;
      HeldByAlignment := InitSet (1) ;
      FinishedAlignment := InitSet (1) ;
      ToBeSolvedByQuads := InitSet (1) ;
      Next := NIL
   END ;
   RETURN g
END InitGroup ;


(*
   KillGroup - delete all sets in group and deallocate g.
*)

PROCEDURE KillGroup (VAR g: Group) ;
BEGIN
   (* Delete all sets in group.  *)
   IF g # NIL
   THEN
      WITH g^ DO
         FinishedSetArray := KillSet (FinishedSetArray) ;
         ToDoList := KillSet (ToDoList) ;
         FullyDeclared := KillSet (FullyDeclared) ;
         PartiallyDeclared := KillSet (PartiallyDeclared) ;
         NilTypedArrays := KillSet (NilTypedArrays) ;
         HeldByAlignment := KillSet (HeldByAlignment) ;
         FinishedAlignment := KillSet (FinishedAlignment) ;
         ToBeSolvedByQuads := KillSet (ToBeSolvedByQuads) ;
         Next := NIL
      END ;
      DisposeGroup (g)
   END
END KillGroup ;


(*
   DupGroup - If g is not NIL then destroy g.
              Return a duplicate of GlobalGroup.
*)

PROCEDURE DupGroup (g: Group) : Group ;
BEGIN
   IF g # NIL
   THEN
      (* Kill old group.  *)
      KillGroup (g)
   END ;
   NewGroup (g) ;
   WITH g^ DO
      (* Copy all sets.  *)
      FinishedSetArray := DuplicateSet (GlobalGroup^.FinishedSetArray) ;
      ToDoList := DuplicateSet (GlobalGroup^.ToDoList) ;
      FullyDeclared := DuplicateSet (GlobalGroup^.FullyDeclared) ;
      PartiallyDeclared := DuplicateSet (GlobalGroup^.PartiallyDeclared) ;
      NilTypedArrays := DuplicateSet (GlobalGroup^.NilTypedArrays) ;
      HeldByAlignment := DuplicateSet (GlobalGroup^.HeldByAlignment) ;
      FinishedAlignment := DuplicateSet (GlobalGroup^.FinishedAlignment) ;
      ToBeSolvedByQuads := DuplicateSet (GlobalGroup^.ToBeSolvedByQuads) ;
      Next := NIL
   END ;
   RETURN g
END DupGroup ;


(*
   EqualGroup - return TRUE if group left = right.
*)

PROCEDURE EqualGroup (left, right: Group) : BOOLEAN ;
BEGIN
   RETURN ((left = right) OR
           (EqualSet (left^.FullyDeclared, right^.FullyDeclared) AND
            EqualSet (left^.PartiallyDeclared, right^.PartiallyDeclared) AND
            EqualSet (left^.NilTypedArrays, right^.NilTypedArrays) AND
            EqualSet (left^.HeldByAlignment, right^.HeldByAlignment) AND
            EqualSet (left^.FinishedAlignment, right^.FinishedAlignment) AND
            EqualSet (left^.ToDoList, right^.ToDoList) AND
            EqualSet (left^.ToBeSolvedByQuads, right^.ToBeSolvedByQuads) AND
            EqualSet (left^.FinishedSetArray, right^.FinishedSetArray)))
END EqualGroup ;


(*
   LookupSet -
*)

PROCEDURE LookupSet (listtype: ListType) : Set ;
BEGIN
   CASE listtype OF

   fullydeclared     : RETURN GlobalGroup^.FullyDeclared |
   partiallydeclared : RETURN GlobalGroup^.PartiallyDeclared |
   niltypedarrays    : RETURN GlobalGroup^.NilTypedArrays |
   heldbyalignment   : RETURN GlobalGroup^.HeldByAlignment |
   finishedalignment : RETURN GlobalGroup^.FinishedAlignment |
   todolist          : RETURN GlobalGroup^.ToDoList |
   tobesolvedbyquads : RETURN GlobalGroup^.ToBeSolvedByQuads |
   finishedsetarray  : RETURN GlobalGroup^.FinishedSetArray

   ELSE
      InternalError ('unknown ListType')
   END ;
   RETURN NIL
END LookupSet ;


(*
   GetEnumList -
*)

PROCEDURE GetEnumList (sym: CARDINAL) : tree ;
BEGIN
   IF InBounds(EnumerationIndex, sym)
   THEN
      RETURN( tree (GetIndice(EnumerationIndex, sym)) )
   ELSE
      RETURN( NIL )
   END
END GetEnumList ;


(*
   PutEnumList -
*)

PROCEDURE PutEnumList (sym: CARDINAL; enumlist: tree) ;
BEGIN
   PutIndice(EnumerationIndex, sym, enumlist)
END PutEnumList ;


(*
   MarkExported - tell GCC to mark all exported procedures in module sym.
*)

PROCEDURE MarkExported (sym: CARDINAL) ;
BEGIN
   IF Optimizing
   THEN
      MarkFunctionReferenced(Mod2Gcc(sym)) ;
      IF IsDefImp(sym) OR IsModule(sym)
      THEN
         ForeachExportedDo(sym, MarkExported)
      END
   END
END MarkExported ;


(*
   Chained - checks to see that, sym, has not already been placed on a chain.
             It returns the symbol, sym.
*)

PROCEDURE Chained (sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsElementInSet(ChainedList, sym)
   THEN
      InternalError ('symbol has already been chained onto a previous list')
   END ;
   IncludeElementIntoSet(ChainedList, sym) ;
   RETURN( sym )
END Chained ;


(*
   DoStartDeclaration - returns a tree representing a symbol which has
                        not yet been finished.  Used when declaring
                        recursive types.
*)

PROCEDURE DoStartDeclaration (sym: CARDINAL; p: StartProcedure) : tree ;
VAR
   location: location_t ;
BEGIN
   IF NOT GccKnowsAbout (sym)
   THEN
      location := TokenToLocation (GetDeclaredMod (sym)) ;
      PreAddModGcc(sym, p (location, KeyToCharStar (GetFullSymName (sym))))
   END ;
   RETURN Mod2Gcc (sym)
END DoStartDeclaration ;


(*
   ArrayComponentsDeclared - returns TRUE if array, sym,
                             subscripts and type are known.
*)

PROCEDURE ArrayComponentsDeclared (sym: CARDINAL) : BOOLEAN ;
VAR
   Subscript      : CARDINAL ;
   Type, High, Low: CARDINAL ;
BEGIN
   Subscript := GetArraySubscript(sym) ;
   Assert(IsSubscript(Subscript)) ;
   Type := GetDType(Subscript) ;
   Low := GetTypeMin(Type) ;
   High := GetTypeMax(Type) ;
   RETURN( IsFullyDeclared(Type) AND
           IsFullyDeclared(Low) AND
           IsFullyDeclared(High) )
END ArrayComponentsDeclared ;


(*
   GetRecordOfVarient -
*)

PROCEDURE GetRecordOfVarient (sym: CARDINAL) : CARDINAL ;
BEGIN
   IF IsVarient(sym) OR IsFieldVarient(sym)
   THEN
      REPEAT
         sym := GetParent(sym)
      UNTIL IsRecord(sym)
   END ;
   RETURN( sym )
END GetRecordOfVarient ;


(*
   CanDeclareRecordKind -
*)

PROCEDURE CanDeclareRecordKind (sym: CARDINAL) : BOOLEAN ;
BEGIN
   sym := GetRecordOfVarient(sym) ;
   RETURN( IsRecord(sym) AND
           ((GetDefaultRecordFieldAlignment(sym)=NulSym) OR
            IsFullyDeclared(GetDefaultRecordFieldAlignment(sym))) )
END CanDeclareRecordKind ;


(*
   DeclareRecordKind - works out whether record, sym, is packed or not.
*)

PROCEDURE DeclareRecordKind (sym: CARDINAL) ;
BEGIN
   IF IsRecord(sym)
   THEN
      DetermineIfRecordPacked(sym)
   END ;
   WatchIncludeList(sym, todolist) ;
   WatchRemoveList(sym, heldbyalignment) ;
   WatchIncludeList(sym, finishedalignment) ;
   IF AllDependantsFullyDeclared(sym)
   THEN
      (* All good and ready to be solved. *)
   END
END DeclareRecordKind ;


(*
   CanDeclareRecord -
*)

PROCEDURE CanDeclareRecord (sym: CARDINAL) : BOOLEAN ;
BEGIN
   TraverseDependants(sym) ;
   IF AllDependantsFullyDeclared(sym)
   THEN
      RETURN TRUE
   ELSE
      WatchIncludeList(sym, finishedalignment) ;
      RETURN FALSE
   END
END CanDeclareRecord ;


(*
   FinishDeclareRecord -
*)

PROCEDURE FinishDeclareRecord (sym: CARDINAL) ;
BEGIN
   DeclareTypeConstFully(sym) ;
   WatchRemoveList(sym, heldbyalignment) ;
   WatchRemoveList(sym, finishedalignment) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END FinishDeclareRecord ;


(*
   CanDeclareTypePartially - return TRUE if we are able to make a
                             gcc partially created type.
*)

PROCEDURE CanDeclareTypePartially (sym: CARDINAL) : BOOLEAN ;
VAR
   type: CARDINAL ;
BEGIN
   IF IsElementInSet(GlobalGroup^.PartiallyDeclared, sym)
   THEN
      RETURN( FALSE )
   ELSIF IsProcType(sym) OR IsRecord(sym) OR IsVarient(sym) OR IsFieldVarient(sym)
   THEN
      RETURN( TRUE )
   ELSIF IsType(sym)
   THEN
      type := GetSType(sym) ;
      IF (type#NulSym) AND IsNilTypedArrays(type)
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END CanDeclareTypePartially ;


(*
   DeclareTypePartially - create the gcc partial type symbol from, sym.
*)

PROCEDURE DeclareTypePartially (sym: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   (* check to see if we have already partially declared the symbol *)
   IF NOT IsElementInSet(GlobalGroup^.PartiallyDeclared, sym)
   THEN
      IF IsRecord(sym)
      THEN
         Assert (NOT IsElementInSet (GlobalGroup^.HeldByAlignment, sym)) ;
         Assert (DoStartDeclaration (sym, BuildStartRecord) # NIL) ;
         WatchIncludeList (sym, heldbyalignment)
      ELSIF IsVarient (sym)
      THEN
         Assert(NOT IsElementInSet(GlobalGroup^.HeldByAlignment, sym)) ;
         Assert (DoStartDeclaration(sym, BuildStartVarient) # NIL) ;
         WatchIncludeList(sym, heldbyalignment)
      ELSIF IsFieldVarient(sym)
      THEN
         Assert(NOT IsElementInSet(GlobalGroup^.HeldByAlignment, sym)) ;
         Assert (DoStartDeclaration(sym, BuildStartFieldVarient) # NIL) ;
         WatchIncludeList(sym, heldbyalignment)
      ELSIF IsProcType(sym)
      THEN
         Assert (DoStartDeclaration(sym, BuildStartFunctionType) # NIL) ;
      ELSIF IsType(sym)
      THEN
         IF NOT GccKnowsAbout(sym)
         THEN
            location := TokenToLocation(GetDeclaredMod(sym)) ;
            PreAddModGcc(sym, BuildStartType(location,
                                             KeyToCharStar(GetFullSymName(sym)),
                                             Mod2Gcc(GetSType(sym))))
         END
      ELSE
         InternalError ('do not know how to create a partial type from this symbol')
      END ;
      WatchIncludeList(sym, partiallydeclared) ;
      TraverseDependants(sym)
   END
END DeclareTypePartially ;


(*
   CanDeclareArrayAsNil -
*)

PROCEDURE CanDeclareArrayAsNil (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsArray(sym) AND ArrayComponentsDeclared(sym) )
END CanDeclareArrayAsNil ;


(*
   DeclareArrayAsNil -
*)

PROCEDURE DeclareArrayAsNil (sym: CARDINAL) ;
BEGIN
   PreAddModGcc(sym, BuildStartArrayType(BuildIndex(GetDeclaredMod(sym), sym), NIL, GetDType(sym))) ;
   WatchIncludeList(sym, niltypedarrays)
END DeclareArrayAsNil ;


(*
   CanDeclareArrayPartially -
*)

PROCEDURE CanDeclareArrayPartially (sym: CARDINAL) : BOOLEAN ;
VAR
   type: CARDINAL ;
BEGIN
   IF IsArray(sym)
   THEN
      type := GetSType(sym) ;
      IF IsPartiallyOrFullyDeclared(type) OR
         (IsPointer(type) AND IsNilTypedArrays(type))
      THEN
         RETURN( TRUE )
      END
   END ;
   RETURN( FALSE )
END CanDeclareArrayPartially ;


(*
   DeclareArrayPartially -
*)

PROCEDURE DeclareArrayPartially (sym: CARDINAL) ;
BEGIN
   Assert(IsArray(sym) AND GccKnowsAbout(sym)) ;
   PutArrayType(Mod2Gcc(sym), Mod2Gcc(GetSType(sym))) ;
   WatchIncludeList(sym, partiallydeclared)
END DeclareArrayPartially ;


(*
   CanDeclarePointerToNilArray -
*)

PROCEDURE CanDeclarePointerToNilArray (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsPointer(sym) AND IsNilTypedArrays(GetSType(sym)) )
END CanDeclarePointerToNilArray ;


(*
   DeclarePointerToNilArray -
*)

PROCEDURE DeclarePointerToNilArray (sym: CARDINAL) ;
BEGIN
   PreAddModGcc(sym, BuildPointerType(Mod2Gcc(GetSType(sym)))) ;
   WatchIncludeList(sym, niltypedarrays)
END DeclarePointerToNilArray ;


(*
   CanPromotePointerFully -
*)

PROCEDURE CanPromotePointerFully (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsPointer(sym) AND IsPartiallyOrFullyDeclared(GetSType(sym)) )
END CanPromotePointerFully ;


(*
   PromotePointerFully -
*)

PROCEDURE PromotePointerFully (sym: CARDINAL) ;
BEGIN
   WatchIncludeList(sym, fullydeclared)
END PromotePointerFully ;


(*
   CompletelyResolved - returns TRUE if a symbols has been completely resolved
                        and is not partically declared (such as a record).
*)

PROCEDURE CompletelyResolved (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(GlobalGroup^.FullyDeclared, sym) )
END CompletelyResolved ;


(*
   IsTypeQ - returns TRUE if all q(dependants) of, sym,
             return TRUE.
*)

PROCEDURE IsTypeQ (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   IF IsVar(sym)
   THEN
      RETURN( IsVarDependants(sym, q) )
   ELSIF IsEnumeration(sym)
   THEN
      RETURN( IsEnumerationDependants(sym, q) )
   ELSIF IsFieldEnumeration(sym)
   THEN
      RETURN( TRUE )
   ELSIF IsSubrange(sym)
   THEN
      RETURN( IsSubrangeDependants(sym, q) )
   ELSIF IsPointer(sym)
   THEN
      RETURN( IsPointerDependants(sym, q) )
   ELSIF IsRecord(sym)
   THEN
      RETURN( IsRecordDependants(sym, q) )
   ELSIF IsRecordField(sym)
   THEN
      RETURN( IsRecordFieldDependants(sym, q) )
   ELSIF IsVarient(sym)
   THEN
      RETURN( IsVarientDependants(sym, q) )
   ELSIF IsFieldVarient(sym)
   THEN
      RETURN( IsVarientFieldDependants(sym, q) )
   ELSIF IsArray(sym)
   THEN
      RETURN( IsArrayDependants(sym, q) )
   ELSIF IsProcType(sym)
   THEN
      RETURN( IsProcTypeDependants(sym, q) )
   ELSIF IsUnbounded(sym)
   THEN
      RETURN( IsUnboundedDependants(sym, q) )
   ELSIF IsPartialUnbounded(sym)
   THEN
      InternalError ('should not be declaring a partial unbounded symbol')
   ELSIF IsSet(sym)
   THEN
      RETURN( IsSetDependants(sym, q) )
   ELSIF IsType(sym)
   THEN
      RETURN( IsTypeDependants(sym, q) )
   ELSIF IsConst(sym)
   THEN
      RETURN( IsConstDependants(sym, q) )
   ELSIF IsConstructor(sym) OR IsConstSet(sym)
   THEN
      (* sym can be a constructor, but at present we have not resolved whether
         all dependants are constants.
       *)
      RETURN( IsConstructorDependants(sym, q) )
   ELSIF IsProcedure(sym)
   THEN
      RETURN( IsProcedureDependants(sym, q) )
   ELSE
      RETURN( TRUE )
   END
END IsTypeQ ;


(*
   IsNilTypedArrays - returns TRUE if, sym, is dependant upon a NIL typed array
*)

PROCEDURE IsNilTypedArrays (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(GlobalGroup^.NilTypedArrays, sym) )
END IsNilTypedArrays ;


(*
   IsFullyDeclared - returns TRUE if, sym, is fully declared.
*)

PROCEDURE IsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(GlobalGroup^.FullyDeclared, sym) )
END IsFullyDeclared ;


(*
   AllDependantsFullyDeclared - returns TRUE if all dependants of,
                                sym, are declared.
*)

PROCEDURE AllDependantsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsTypeQ(sym, IsFullyDeclared) )
END AllDependantsFullyDeclared ;


(*
   NotAllDependantsFullyDeclared - returns TRUE if any dependants of,
                                   sym, are not declared.
*)

PROCEDURE NotAllDependantsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( NOT IsTypeQ(sym, IsFullyDeclared) )
END NotAllDependantsFullyDeclared ;


(*
   IsPartiallyDeclared - returns TRUE if, sym, is partially declared.
*)

PROCEDURE IsPartiallyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(GlobalGroup^.PartiallyDeclared, sym) )
END IsPartiallyDeclared ;


(*
   AllDependantsPartiallyDeclared - returns TRUE if all dependants of,
                                    sym, are partially declared.
*)

PROCEDURE AllDependantsPartiallyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsTypeQ(sym, IsPartiallyDeclared) )
END AllDependantsPartiallyDeclared ;


(*
   NotAllDependantsPartiallyDeclared - returns TRUE if any dependants of,
                                       sym, are not partially declared.
*)

PROCEDURE NotAllDependantsPartiallyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( NOT IsTypeQ(sym, IsPartiallyDeclared) )
END NotAllDependantsPartiallyDeclared ;


(*
   IsPartiallyOrFullyDeclared - returns TRUE if, sym, is partially or fully declared.
*)

PROCEDURE IsPartiallyOrFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsElementInSet(GlobalGroup^.PartiallyDeclared, sym) OR
           IsElementInSet(GlobalGroup^.FullyDeclared, sym) )
END IsPartiallyOrFullyDeclared ;


(*
   AllDependantsPartiallyOrFullyDeclared - returns TRUE if all dependants of,
                                           sym, are partially or fully declared.
*)

PROCEDURE AllDependantsPartiallyOrFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsTypeQ(sym, IsPartiallyOrFullyDeclared) )
END AllDependantsPartiallyOrFullyDeclared ;


(*
   NotAllDependantsPartiallyOrFullyDeclared - returns TRUE if all dependants of,
                                              sym, are not partially and not fully
                                              declared.
*)

(*
PROCEDURE NotAllDependantsPartiallyOrFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsTypeQ(sym, IsPartiallyOrFullyDeclared) )
END NotAllDependantsPartiallyOrFullyDeclared ;
*)


(*
   TypeConstDependantsFullyDeclared - returns TRUE if sym is a constant or
                                      type and its dependants are fully
                                      declared.
*)

PROCEDURE TypeConstDependantsFullyDeclared (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (NOT IsVar(sym)) AND
           (NOT IsRecord(sym)) AND
           (NOT IsParameter(sym)) AND
           AllDependantsFullyDeclared(sym) )
END TypeConstDependantsFullyDeclared ;


(*
   CanBeDeclaredViaPartialDependants - returns TRUE if this symbol
                                       can be declared by partial
                                       dependants.  Such a symbol must
                                       be a record, proctype or
                                       an array.
*)

PROCEDURE CanBeDeclaredViaPartialDependants (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (IsPointer(sym) OR IsProcType(sym)) AND
           AllDependantsPartiallyOrFullyDeclared(sym) )
END CanBeDeclaredViaPartialDependants ;


(*
   DeclareConstFully - will add, sym, to the fully declared list and
                       also remove it from the to do list.  This is
                       called indirectly from M2GenGCC as it calculates
                       constants during quadruple processing.
*)

PROCEDURE DeclareConstFully (sym: CARDINAL) ;
BEGIN
   WatchIncludeList(sym, fullydeclared) ;
   WatchRemoveList(sym, todolist) ;
   WatchRemoveList(sym, partiallydeclared) ;
   WatchRemoveList(sym, tobesolvedbyquads)
END DeclareConstFully ;


(*
   PutToBeSolvedByQuads - places, sym, to this list and returns,
                          sym.
*)

PROCEDURE PutToBeSolvedByQuads (sym: CARDINAL) ;
BEGIN
   WatchIncludeList(sym, tobesolvedbyquads)
END PutToBeSolvedByQuads ;


(*
   DeclareTypeConstFully - declare the GCC type and add the double
                           book keeping entry.
*)

PROCEDURE DeclareTypeConstFully (sym: CARDINAL) ;
VAR
   t: tree ;
BEGIN
   IF NOT IsElementInSet(GlobalGroup^.ToBeSolvedByQuads, sym)
   THEN
      IF IsModule(sym) OR IsDefImp(sym)
      THEN
         WatchIncludeList(sym, fullydeclared) ;
         WatchRemoveList(sym, partiallydeclared) ;
         WatchRemoveList(sym, todolist)
      ELSIF IsProcedure(sym)
      THEN
         DeclareProcedureToGcc(sym) ;
         WatchIncludeList(sym, fullydeclared) ;
         WatchRemoveList(sym, partiallydeclared) ;
         WatchRemoveList(sym, todolist)
      ELSE
         t := TypeConstFullyDeclared(sym) ;
         IF t#NIL
         THEN
            (* add relationship between gccsym and sym *)
            PreAddModGcc(sym, t) ;
            WatchIncludeList(sym, fullydeclared) ;
            WatchRemoveList(sym, partiallydeclared) ;
            WatchRemoveList(sym, heldbyalignment) ;
            WatchRemoveList(sym, finishedalignment) ;
            WatchRemoveList(sym, todolist)
         END
      END
   END
END DeclareTypeConstFully ;


(*
   DeclareTypeFromPartial - declare the full GCC type from a partial type
                            and add the double book keeping entry.
*)

PROCEDURE DeclareTypeFromPartial (sym: CARDINAL) ;
VAR
   t: tree ;
BEGIN
   t := CompleteDeclarationOf(sym) ;
   IF t=NIL
   THEN
      InternalError ('expecting to be able to create a gcc type')
   ELSE
      AddModGcc(sym, t) ;
      WatchIncludeList(sym, fullydeclared) ;
      WatchRemoveList(sym, partiallydeclared)
   END
END DeclareTypeFromPartial ;


(*
   CanBeDeclaredPartiallyViaPartialDependants - returns TRUE if, sym,
                                                can be partially declared via
                                                another partially declared type.
*)

PROCEDURE CanBeDeclaredPartiallyViaPartialDependants (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsType(sym) AND AllDependantsPartiallyDeclared(sym) )
END CanBeDeclaredPartiallyViaPartialDependants ;


(*
   EmitCircularDependancyError - issue a dependancy error.
*)

PROCEDURE EmitCircularDependancyError (sym: CARDINAL) ;
BEGIN
   MetaError1('circular dependancy error found when trying to resolve {%1Uad}',
              sym)
END EmitCircularDependancyError ;


TYPE
   Rule = (norule, partialtype, arraynil, pointernilarray, arraypartial,
           pointerfully, recordkind, recordfully, typeconstfully,
           pointerfrompartial, typefrompartial, partialfrompartial,
           partialtofully, circulartodo, circularpartial, circularniltyped) ;

VAR
   bodyp          : WalkAction ;
   bodyq          : IsAction ;
   bodyt          : ListType ;
   bodyr          : Rule ;
   recursionCaught,
   oneResolved,
   noMoreWritten  : BOOLEAN ;


(*
   WriteRule - writes out the name of the rule.
*)

PROCEDURE WriteRule ;
BEGIN
   IF Debugging
   THEN
      CASE bodyr OF

      norule            :  printf0('norule') |
      partialtype       :  printf0('partialtype') |
      arraynil          :  printf0('arraynil') |
      pointernilarray   :  printf0('pointernilarray') |
      arraypartial      :  printf0('arraypartial') |
      pointerfully      :  printf0('pointerfully') |
      recordkind        :  printf0('recordkind') |
      recordfully       :  printf0('recordfully') |
      typeconstfully    :  printf0('typeconstfully') |
      pointerfrompartial:  printf0('pointerfrompartial') |
      typefrompartial   :  printf0('typefrompartial') |
      partialfrompartial:  printf0('partialfrompartial') |
      partialtofully    :  printf0('partialtofully') |
      circulartodo      :  printf0('circulartodo') |
      circularpartial   :  printf0('circularpartial') |
      circularniltyped  :  printf0('circularniltyped')

      ELSE
         InternalError ('unknown rule')
      END
   END
END WriteRule ;


(*
   Body -
*)

PROCEDURE Body (sym: CARDINAL) ;
BEGIN
   IF bodyq (sym)
   THEN
      WatchRemoveList (sym, bodyt) ;
      bodyp (sym) ;
      (* The bodyp (sym) procedure function might have replaced sym into the set.  *)
      IF NOT IsElementInSet (LookupSet (bodyt), sym)
      THEN
         noMoreWritten := FALSE ;
         oneResolved := TRUE
      END
   END
END Body ;


(*
   ForeachTryDeclare - while q (of one sym in set t) is true
                          for each symbol in set t,
                             if q (sym)
                             then
                                p (sym)
                             end
                          end
                       end
*)

PROCEDURE ForeachTryDeclare (t: ListType; r: Rule;
                             q: IsAction; p: WalkAction) : BOOLEAN ;
BEGIN
   IF recursionCaught
   THEN
      InternalError ('caught recursive cycle in ForeachTryDeclare')
   END ;
   bodyt := t ;
   bodyq := q ;
   bodyp := p ;
   bodyr := r ;
   recursionCaught := TRUE ;
   oneResolved := FALSE ;
   REPEAT
      noMoreWritten := TRUE ;
      ForeachElementInSetDo (LookupSet (t), Body)
   UNTIL noMoreWritten ;
   bodyr := norule ;
   recursionCaught := FALSE ;
   RETURN( oneResolved )
END ForeachTryDeclare ;


(*
   DeclaredOutandingTypes - writes out any types that have their
                            dependants solved.  It returns TRUE if
                            all outstanding types have been written.
*)

PROCEDURE DeclaredOutstandingTypes (ForceComplete: BOOLEAN) : BOOLEAN ;
VAR
   finished: BOOLEAN ;
   copy    : Group ;
BEGIN
   copy := NIL ;
   finished := FALSE ;
   REPEAT
      IF Progress AND (copy # NIL)
      THEN
         IF NOT EqualGroup (copy, GlobalGroup)
         THEN
            DebugSetNumbers ;
            DebugSets
         END
      END ;
      copy := DupGroup (copy) ;
      IF ForeachTryDeclare (todolist,
                            partialtype,
                            CanDeclareTypePartially,
                            DeclareTypePartially)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare (todolist,
                               arraynil,
                               CanDeclareArrayAsNil,
                               DeclareArrayAsNil)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare (todolist,
                               pointernilarray,
                               CanDeclarePointerToNilArray,
                               DeclarePointerToNilArray)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare (niltypedarrays,
                               arraypartial,
                               CanDeclareArrayPartially,
                               DeclareArrayPartially)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare (niltypedarrays,
                               pointerfully,
                               CanPromotePointerFully,
                               PromotePointerFully)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare (heldbyalignment,
                               recordkind,
                               CanDeclareRecordKind,
                               DeclareRecordKind)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare (finishedalignment,
                               recordfully,
                               CanDeclareRecord,
                               FinishDeclareRecord)
      THEN
         (* continue looping *)
      ELSIF ForeachTryDeclare (todolist,
                               typeconstfully,
                               TypeConstDependantsFullyDeclared,
                               DeclareTypeConstFully)
      THEN
         (* Continue looping.  *)
      ELSIF ForeachTryDeclare (todolist,
                               typefrompartial,
                               CanBeDeclaredViaPartialDependants,
                               DeclareTypeFromPartial)
      THEN
         (* Continue looping.  *)
      ELSIF ForeachTryDeclare (partiallydeclared,
                               partialfrompartial,
                               CanBeDeclaredPartiallyViaPartialDependants,
                               DeclareTypePartially)
      THEN
         (* Continue looping.  *)
      ELSIF ForeachTryDeclare (partiallydeclared,
                               partialtofully,
                               TypeConstDependantsFullyDeclared,
                               DeclareTypeConstFully)
      THEN
         (* Continue looping.  *)
      ELSE
         (* Nothing left to do (and constants are resolved elsewhere).  *)
         finished := TRUE
      END
   UNTIL finished ;
   KillGroup (copy) ;
   IF ForceComplete
   THEN
      IF ForeachTryDeclare (todolist,
                            circulartodo,
                            NotAllDependantsFullyDeclared,
                            EmitCircularDependancyError)
      THEN
      ELSIF ForeachTryDeclare (partiallydeclared,
                               circularpartial,
                               NotAllDependantsPartiallyDeclared,
                               EmitCircularDependancyError)
      THEN
      ELSIF ForeachTryDeclare (niltypedarrays,
                               circularniltyped,
                               NotAllDependantsPartiallyDeclared,
                               EmitCircularDependancyError)
      THEN
      END
   END ;
   RETURN NoOfElementsInSet (GlobalGroup^.ToDoList) = 0
END DeclaredOutstandingTypes ;


(*
   CompleteDeclarationOf - returns the GCC Tree for, sym, if it can
                           be created from partially or fully declared
                           dependents.
*)

PROCEDURE CompleteDeclarationOf (sym: CARDINAL) : tree ;
BEGIN
   IF IsArray(sym)
   THEN
      RETURN( DeclareArray(sym) )
   ELSIF IsProcType(sym)
   THEN
      RETURN( DeclareProcType(sym) )
   ELSIF IsRecordField(sym)
   THEN
      RETURN( DeclareRecordField(sym) )
   ELSIF IsPointer(sym)
   THEN
      RETURN( DeclarePointer(sym) )
   ELSE
      RETURN( NIL )
   END
END CompleteDeclarationOf ;


(*
   DeclareType - here a type has been created via TYPE foo = bar,
                 we must tell GCC about it.
*)

PROCEDURE DeclareType (sym: CARDINAL) : tree ;
VAR
   t       : tree ;
   location: location_t ;
BEGIN
   IF GetSType(sym)=NulSym
   THEN
      MetaError1('base type {%1Ua} not understood', sym) ;
      InternalError ('base type should have been declared')
   ELSE
      IF GetSymName(sym)=NulName
      THEN
         RETURN( tree(Mod2Gcc(GetSType(sym))) )
      ELSE
         location := TokenToLocation(GetDeclaredMod(sym)) ;
         IF GccKnowsAbout(sym)
         THEN
            t := Mod2Gcc(sym)
         ELSE
            (* not partially declared therefore start it *)
            t := BuildStartType(location,
                                KeyToCharStar(GetFullSymName(sym)), Mod2Gcc(GetSType(sym)))
         END ;
         t := BuildEndType(location, t) ;  (* now finish it *)
         RETURN( t )
      END
   END
END DeclareType ;


(*
   DeclareIntegerConstant - declares an integer constant.
*)

(*
PROCEDURE DeclareIntegerConstant (sym: CARDINAL; value: INTEGER) ;
BEGIN
   PreAddModGcc(sym, BuildIntegerConstant(value)) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END DeclareIntegerConstant ;
*)


(*
   DeclareIntegerFromTree - declares an integer constant from a Tree, value.
*)

PROCEDURE DeclareConstantFromTree (sym: CARDINAL; value: tree) ;
BEGIN
   PreAddModGcc(sym, value) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END DeclareConstantFromTree ;


(*
   DeclareCharConstant - declares a character constant.
*)

PROCEDURE DeclareCharConstant (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   location: location_t ;
BEGIN
   Assert (IsConstStringKnown (sym)) ;
   location := TokenToLocation(tokenno) ;
   PreAddModGcc(sym, BuildCharConstant(location, KeyToCharStar(GetString(sym)))) ;
   WatchRemoveList(sym, todolist) ;
   WatchIncludeList(sym, fullydeclared)
END DeclareCharConstant ;


(*
   DeclareStringConstant - declares a string constant the sym will be known.
*)

PROCEDURE DeclareStringConstant (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   symtree : tree ;
BEGIN
   Assert (IsConstStringKnown (sym)) ;
   IF IsConstStringM2nul (sym) OR IsConstStringCnul (sym)
   THEN
      (* in either case the string needs a nul terminator.  If the string
         is a C variant it will already have had any escape characters applied.
         The BuildCStringConstant only adds the nul terminator.  *)
      symtree := BuildCStringConstant (KeyToCharStar (GetString (sym)),
                                       GetStringLength (tokenno, sym))
   ELSE
      symtree := BuildStringConstant (KeyToCharStar (GetString (sym)),
                                      GetStringLength (tokenno, sym))
   END ;
   PreAddModGcc (sym, symtree) ;
   WatchRemoveList (sym, todolist) ;
   WatchIncludeList (sym, fullydeclared)
END DeclareStringConstant ;


(*
   PromoteToString - declare, sym, and then promote it to a string.
                     Note that if sym is a single character we do
                          *not* record it as a string
                          but as a char however we always
                          return a string constant.
*)

PROCEDURE PromoteToString (tokenno: CARDINAL; sym: CARDINAL) : tree ;
VAR
   size: CARDINAL ;
   ch  : CHAR ;
BEGIN
   DeclareConstant (tokenno, sym) ;
   IF IsConst (sym) AND (GetSType (sym) = Char)
   THEN
      PushValue (sym) ;
      ch := PopChar (tokenno) ;
      RETURN BuildCStringConstant (string (InitStringChar (ch)), 1)
   ELSE
      Assert (IsConstStringKnown (sym)) ;
      size := GetStringLength (tokenno, sym) ;
      IF size > 1
      THEN
         (* It will be already be declared as a string, so return it.  *)
         RETURN tree (Mod2Gcc (sym))
      ELSE
         RETURN BuildStringConstant (KeyToCharStar (GetString (sym)),
                                     GetStringLength (tokenno, sym))
      END
   END
END PromoteToString ;


(*
   PromoteToCString - declare, sym, and then promote it to a string.
                      Note that if sym is a single character we do
                          *not* record it as a string
                          but as a char however we always
                          return a string constant.
*)

PROCEDURE PromoteToCString (tokenno: CARDINAL; sym: CARDINAL) : tree ;
VAR
   size: CARDINAL ;
   ch  : CHAR ;
BEGIN
   DeclareConstant (tokenno, sym) ;
   Assert (IsConstStringKnown (sym)) ;
   IF IsConst (sym) AND (GetSType (sym) = Char)
   THEN
      PushValue (sym) ;
      ch := PopChar (tokenno) ;
      RETURN BuildCStringConstant (string (InitStringChar (ch)), 1)
   ELSE
      size := GetStringLength (tokenno, sym) ;
      RETURN BuildCStringConstant (KeyToCharStar (GetString (sym)),
                                   size)
   END
END PromoteToCString ;


(*
   WalkConstructor - walks all dependants of, sym.
*)

PROCEDURE WalkConstructor (sym: CARDINAL; p: WalkAction) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetSType(sym) ;
   IF type#NulSym
   THEN
      WalkDependants(type, p) ;
      WalkConstructorDependants(sym, p)
   END
END WalkConstructor ;


(*
   DeclareConstructor - declares a constructor.
*)

PROCEDURE DeclareConstructor (tokenno: CARDINAL; quad: CARDINAL; sym: CARDINAL) ;
BEGIN
   IF sym=NulSym
   THEN
      InternalError ('trying to declare the NulSym')
   END ;
   IF IsConstructor (sym) AND (NOT GccKnowsAbout (sym))
   THEN
      WalkConstructor (sym, TraverseDependants) ;
      DeclareTypesConstantsProceduresInRange (GetScope (sym), quad, quad) ;
      Assert (IsConstructorDependants (sym, IsFullyDeclared)) ;
      PushValue (sym) ;
      DeclareConstantFromTree (sym, PopConstructorTree (tokenno))
   END
END DeclareConstructor ;


(*
   TryDeclareConstructor - try and declare a constructor.  If, sym, is a
                           constructor try and declare it, if we cannot
                           then enter it into the to do list.
*)

PROCEDURE TryDeclareConstructor (tokenno: CARDINAL; sym: CARDINAL) ;
BEGIN
   IF sym#NulSym
   THEN
      IF IsConstructor(sym) AND (NOT GccKnowsAbout(sym))
      THEN
         WalkConstructor(sym, TraverseDependants) ;
         IF NOT IsElementInSet(GlobalGroup^.ToBeSolvedByQuads, sym)
         THEN
            TryEvaluateValue(sym) ;
            IF IsConstructorDependants(sym, IsFullyDeclared)
            THEN
               PushValue(sym) ;
               DeclareConstantFromTree(sym, PopConstructorTree(tokenno))
            END
         END
      END
   END
END TryDeclareConstructor ;


(*
   WalkConst - walks all dependants of, sym.
*)

PROCEDURE WalkConst (sym: CARDINAL; p: WalkAction) ;
VAR
   type: CARDINAL ;
BEGIN
   Assert (IsConst (sym)) ;
   type := GetSType (sym) ;
   IF type # NulSym
   THEN
      p (type)
   END ;
   IF IsConstSet (sym) OR IsConstructor (sym)
   THEN
      WalkConstructor (sym, p)
   END
END WalkConst ;


(*
   IsConstDependants - returns TRUE if the symbol, sym,
                       q(dependants) all return TRUE.
*)

PROCEDURE IsConstDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   type: CARDINAL ;
BEGIN
   Assert (IsConst (sym)) ;
   type := GetSType (sym) ;
   IF type # NulSym
   THEN
      IF NOT q (type)
      THEN
         RETURN FALSE
      END
   END ;
   IF IsConstSet (sym) OR IsConstructor (sym)
   THEN
      RETURN IsConstructorDependants (sym, q)
   END ;
   RETURN IsValueSolved (sym)
END IsConstDependants ;


(*
   TryDeclareConstant - try and declare a constant.  If, sym, is a
                        constant try and declare it, if we cannot
                        then enter it into the to do list.
*)

PROCEDURE TryDeclareConstant (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   TryDeclareConstructor(tokenno, sym) ;
   IF IsConst(sym)
   THEN
      TraverseDependants(sym) ;
      type := GetSType(sym) ;
      IF (type#NulSym) AND (NOT CompletelyResolved(type))
      THEN
         TraverseDependants(sym) ;
         RETURN
      END ;
      IF IsConstructor(sym) AND (NOT IsConstructorConstant(sym))
      THEN
         TraverseDependants(sym) ;
         RETURN
      END ;
      IF (IsConstructor(sym) OR IsConstSet(sym)) AND (type=NulSym)
      THEN
         TraverseDependants(sym) ;
         RETURN
      END ;
      IF IsElementInSet(GlobalGroup^.ToBeSolvedByQuads, sym)
      THEN
         (* we allow the above rules to be executed even if it is fully declared
            so to ensure that types of compiler builtin constants (BitsetSize
            etc) are fully declared.

            However at this point if, sym, is fully declared we return
         *)
         IF IsFullyDeclared(sym)
         THEN
            RETURN
         END ;
         TraverseDependants(sym)
      ELSE
         TryDeclareConst(tokenno, sym)
      END
   END
END TryDeclareConstant ;


(*
   IsAnyType - return TRUE if sym is any Modula-2 type.
*)

PROCEDURE IsAnyType (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (IsRecord(sym) OR IsType(sym) OR IsRecordField(sym) OR
           IsPointer(sym) OR IsArray(sym) OR IsSet (sym) OR IsEnumeration (sym) OR
           IsPointer (sym))
END IsAnyType ;


(*
   TryDeclareType - try and declare a type.  If sym is a
                    type try and declare it, if we cannot
                    then enter it into the to do list.
*)

PROCEDURE TryDeclareType (type: CARDINAL) ;
BEGIN
   IF (type#NulSym) AND IsAnyType (type)
   THEN
      TraverseDependants (type)
   END
END TryDeclareType ;


(*
   DeclareConstant - checks to see whether, sym, is a constant and
                     declares the constant to gcc.
*)

PROCEDURE DeclareConstant (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   type: CARDINAL ;
   t   : tree ;
BEGIN
   IF IsConst(sym)
   THEN
      TraverseDependants(sym) ;
      type := GetSType(sym) ;
      Assert((type=NulSym) OR CompletelyResolved(type)) ;
      Assert((NOT IsConstructor(sym)) OR IsConstructorConstant(sym)) ;
      Assert((type#NulSym) OR (NOT (IsConstructor(sym) OR IsConstSet(sym)))) ;
      t := DeclareConst(tokenno, sym) ;
      Assert(t#NIL)
   END
END DeclareConstant ;


(*
   DeclareConstString -
*)

PROCEDURE DeclareConstString (tokenno: CARDINAL; sym: CARDINAL) : BOOLEAN ;
VAR
   size: CARDINAL ;
BEGIN
   IF IsConstStringKnown (sym)
   THEN
      size := GetStringLength (tokenno, sym) ;
      IF size = 1
      THEN
         DeclareCharConstant (tokenno, sym)
      ELSE
         DeclareStringConstant (tokenno, sym)
      END ;
      RETURN TRUE
   END ;
   RETURN FALSE
END DeclareConstString ;


(*
   TryDeclareConst - try to declare a const to gcc.  If it cannot
                     declare the symbol it places it into the
                     todolist.
*)

PROCEDURE TryDeclareConst (tokenno: CARDINAL; sym: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   IF NOT GccKnowsAbout(sym)
   THEN
      IF IsConstructor(sym) OR IsConstSet(sym)
      THEN
         WalkConstructorDependants(sym, TraverseDependants) ;
         TryEvaluateValue(sym) ;
         IF NOT IsConstructorDependants(sym, IsFullyDeclared)
         THEN
            TraverseDependants(sym) ;
            RETURN
         END ;
         IF NOT IsConstructorConstant(sym)
         THEN
            RETURN
         END
      END ;
      IF IsConstString(sym) AND IsConstStringKnown (sym)
      THEN
         IF DeclareConstString (tokenno, sym)
         THEN
         END
      ELSIF IsValueSolved(sym)
      THEN
         PushValue(sym) ;
         IF IsConstSet(sym)
         THEN
            DeclareConstantFromTree(sym, PopSetTree(tokenno))
         ELSIF IsConstructor(sym)
         THEN
            DeclareConstantFromTree(sym, PopConstructorTree(tokenno))
         ELSIF IsRealType (GetDType (sym)) OR IsRealN (GetDType (sym))
         THEN
            type := GetDType(sym) ;
            DeclareConstantFromTree(sym, BuildConvert(TokenToLocation(tokenno), Mod2Gcc(type), PopRealTree(), TRUE))
         ELSIF IsComplexType(GetDType(sym))
         THEN
            type := GetDType(sym) ;
            DeclareConstantFromTree(sym, BuildConvert(TokenToLocation(tokenno), Mod2Gcc(type), PopComplexTree(), TRUE))
         ELSE
            IF GetSType(sym)=NulSym
            THEN
               type := ZType
            ELSE
               type := GetDType(sym)
            END ;
            DeclareConstantFromTree(sym, BuildConvert(TokenToLocation(tokenno), Mod2Gcc(type), PopIntegerTree(), TRUE))
         END
      ELSE
         TraverseDependants(sym)
      END
   END
END TryDeclareConst ;


(*
   DeclareConst - declares a const to gcc and returns a Tree.
*)

PROCEDURE DeclareConst (tokenno: CARDINAL; sym: CARDINAL) : tree ;
VAR
   type: CARDINAL ;
BEGIN
   IF GccKnowsAbout(sym)
   THEN
      RETURN( Mod2Gcc(sym) )
   END ;
   IF IsConstructor(sym) OR IsConstSet(sym)
   THEN
      EvaluateValue(sym)
   END ;
   IF IsConstString(sym)
   THEN
      IF DeclareConstString (tokenno, sym)
      THEN
      END
   ELSIF IsValueSolved(sym)
   THEN
      PushValue(sym) ;
      IF IsConstSet(sym)
      THEN
         DeclareConstantFromTree(sym, PopSetTree(tokenno))
      ELSIF IsConstructor(sym)
      THEN
         DeclareConstantFromTree(sym, PopConstructorTree(tokenno))
      ELSIF IsRealType(GetDType(sym))
      THEN
         type := GetDType(sym) ;
         DeclareConstantFromTree(sym, BuildConvert(TokenToLocation(tokenno), Mod2Gcc(type), PopRealTree(), TRUE))
      ELSIF IsComplexType(GetDType(sym))
      THEN
         type := GetDType(sym) ;
         DeclareConstantFromTree(sym, BuildConvert(TokenToLocation(tokenno), Mod2Gcc(type), PopComplexTree(), TRUE))
      ELSE
         IF GetSType(sym)=NulSym
         THEN
            type := ZType
         ELSE
            type := GetDType(sym)
         END ;
         DeclareConstantFromTree(sym, BuildConvert(TokenToLocation(tokenno), Mod2Gcc(type), PopIntegerTree(), TRUE))
      END
   END ;
   IF GccKnowsAbout(sym)
   THEN
      RETURN( Mod2Gcc(sym) )
   ELSE
      RETURN( NIL )
   END
END DeclareConst ;


(*
   DeclareParameters -
*)

PROCEDURE DeclareParameters (sym: CARDINAL) ;
BEGIN
   DeclareUnboundedProcedureParameters(sym)
END DeclareParameters ;


VAR
   unboundedp: WalkAction ;


(*
   WalkFamilyOfUnbounded -
*)

PROCEDURE WalkFamilyOfUnbounded (oaf: CARDINAL <* unused *> ; dim: CARDINAL <* unused *> ; unbounded: CARDINAL) ;
BEGIN
   IF unbounded # NulSym
   THEN
      unboundedp (unbounded)
   END
END WalkFamilyOfUnbounded ;


(*
   WalkAssociatedUnbounded -
*)

PROCEDURE WalkAssociatedUnbounded (sym: CARDINAL; p: WalkAction) ;
VAR
   oaf: CARDINAL ;
   o  : WalkAction ;
BEGIN
   oaf := GetOAFamily(sym) ;
   o := unboundedp ;
   unboundedp := p ;
   ForeachOAFamily (oaf, WalkFamilyOfUnbounded) ;
   unboundedp := o
END WalkAssociatedUnbounded ;


(*
   WalkDependants - walks through all dependants of, Sym,
                    calling, p, for each dependant.
*)

PROCEDURE WalkDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   WalkAssociatedUnbounded(sym, p) ;
   IF IsComponent(sym)
   THEN
      WalkComponentDependants(sym, p)
   ELSIF IsEnumeration(sym)
   THEN
      WalkEnumerationDependants(sym, p)
   ELSIF IsSubrange(sym)
   THEN
      WalkSubrangeDependants(sym, p)
   ELSIF IsPointer(sym)
   THEN
      WalkPointerDependants(sym, p)
   ELSIF IsRecord(sym)
   THEN
      WalkRecordDependants(sym, p)
   ELSIF IsVarient(sym)
   THEN
      WalkVarientDependants(sym, p)
   ELSIF IsRecordField(sym)
   THEN
      WalkRecordFieldDependants(sym, p)
   ELSIF IsFieldVarient(sym)
   THEN
      WalkVarientFieldDependants(sym, p)
   ELSIF IsArray(sym)
   THEN
      WalkArrayDependants(sym, p)
   ELSIF IsProcType(sym)
   THEN
      WalkProcTypeDependants(sym, p)
   ELSIF IsUnbounded(sym)
   THEN
      WalkUnboundedDependants(sym, p)
   ELSIF IsSet(sym)
   THEN
      WalkSetDependants(sym, p)
   ELSIF IsType(sym)
   THEN
      WalkTypeDependants(sym, p)
   ELSIF IsConst(sym)
   THEN
      WalkConst(sym, p)
   ELSIF IsVar(sym)
   THEN
      WalkVarDependants(sym, p)
   ELSIF IsProcedure(sym)
   THEN
      WalkProcedureDependants(sym, p)
   END
END WalkDependants ;


(*
   TraverseDependantsInner -
*)

PROCEDURE TraverseDependantsInner (sym: WORD) ;
BEGIN
   IF (NOT IsElementInSet(GlobalGroup^.FullyDeclared, sym)) AND
      (NOT IsElementInSet(GlobalGroup^.ToDoList, sym))
   THEN
      WatchIncludeList(sym, todolist)
   END ;
   IF NOT IsElementInSet(VisitedList, sym)
   THEN
      IncludeElementIntoSet(VisitedList, sym) ;
      WalkDependants(sym, TraverseDependantsInner)
   END
END TraverseDependantsInner ;


(*
   TraverseDependants - walks, sym, dependants.  But it checks
                        to see that, sym, is not on the
                        FullyDeclared and not on the ToDoList.
*)

PROCEDURE TraverseDependants (sym: WORD) ;
BEGIN
   IF VisitedList=NIL
   THEN
      VisitedList := InitSet(1) ;
      TraverseDependantsInner(sym) ;
      VisitedList := KillSet(VisitedList)
   ELSE
      InternalError ('recursive call to TraverseDependants caught')
   END
END TraverseDependants ;


(*
   WalkTypeInfo - walks type, sym, and its dependants.
*)

PROCEDURE WalkTypeInfo (sym: WORD) ;
BEGIN
   IF IsVarient(sym)
   THEN
      InternalError ('why have we reached here?')
   ELSIF IsVar(sym)
   THEN
      WalkTypeInfo(GetSType(sym)) ;
      IF GetVarBackEndType(sym)#NulSym
      THEN
         WalkTypeInfo(GetVarBackEndType(sym))
      END
   ELSIF IsAModula2Type(sym)
   THEN
      TraverseDependants(sym)
   END
END WalkTypeInfo ;


(*
   DeclareUnboundedProcedureParameters -
*)

PROCEDURE DeclareUnboundedProcedureParameters (sym: WORD) ;
VAR
   param,
   type,
   p, i     : CARDINAL ;
   location : location_t ;
BEGIN
   IF IsProcedure(sym)
   THEN
      p := NoOfParamAny (sym) ;
      i := p ;
      WHILE i>0 DO
         IF IsUnboundedParamAny (sym, i)
         THEN
            param := GetNthParamAny (sym, i) ;
            type := GetSType(param) ;
            TraverseDependants(type) ;
            IF GccKnowsAbout(type)
            THEN
               location := TokenToLocation(GetDeclaredMod(type)) ;
               BuildTypeDeclaration(location, Mod2Gcc(type))
            END
         ELSE
            param := GetNth(sym, i) ;
            type := GetSType(param) ;
            TraverseDependants(type)
         END ;
         DEC(i)
      END
   END
END DeclareUnboundedProcedureParameters ;


(*
   WalkUnboundedProcedureParameters -
*)

PROCEDURE WalkUnboundedProcedureParameters (sym: WORD) ;
VAR
   param,
   type,
   p, i: CARDINAL ;
BEGIN
   IF IsProcedure (sym)
   THEN
      p := NoOfParamAny (sym) ;
      i := p ;
      WHILE i>0 DO
         IF IsUnboundedParamAny (sym, i)
         THEN
            param := GetNthParamAny (sym, i)
         ELSE
            param := GetNth (sym, i)
         END ;
         type := GetSType (param) ;
         WalkTypeInfo (type) ;
         DEC (i)
      END
   END
END WalkUnboundedProcedureParameters ;


(*
   WalkTypesInProcedure - walk all types in procedure, Sym.
*)

PROCEDURE WalkTypesInProcedure (sym: WORD) ;
BEGIN
   ForeachLocalSymDo(sym, TraverseDependants)
END WalkTypesInProcedure ;


(*
   WalkTypesInModule - declare all types in module, Sym, to GCC.
*)

PROCEDURE WalkTypesInModule (sym: WORD) ;
VAR
   n: Name ;
BEGIN
   IF Debugging
   THEN
      n := GetSymName(sym) ;
      printf1('Declaring types in MODULE %a\n', n)
   END ;
   ForeachLocalSymDo(sym, WalkTypeInfo) ;
   ForeachLocalSymDo(sym, WalkUnboundedProcedureParameters) ;
   ForeachInnerModuleDo(sym, WalkTypesInModule)
END WalkTypesInModule ;


(*
   IsRecordFieldDependants - returns TRUE if the record field
                             symbol, sym, p(dependants) all return TRUE.
*)

PROCEDURE IsRecordFieldDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   align: CARDINAL ;
   final: BOOLEAN ;
BEGIN
   final := TRUE ;
   IF NOT q(GetSType(sym))
   THEN
      final := FALSE
   END ;
   align := GetAlignment(sym) ;
   IF (align#NulSym) AND (NOT q(align))
   THEN
      final := FALSE
   END ;
   RETURN( final )
END IsRecordFieldDependants ;


(*
   GetModuleWhereDeclared - returns the module where, Sym, was created.
*)

PROCEDURE GetModuleWhereDeclared (sym: CARDINAL) : CARDINAL ;
VAR
   s: CARDINAL ;
BEGIN
   s := GetScope(sym) ;
   IF (s=NulSym) OR IsDefImp(s) OR
      (IsModule(s) AND (GetScope(s)=NulSym))
   THEN
      RETURN( s )
   ELSE
      RETURN( GetModuleWhereDeclared(s) )
   END
END GetModuleWhereDeclared ;


(*
   IsPseudoProcFunc - returns TRUE if Sym is a pseudo function or procedure.
*)

PROCEDURE IsPseudoProcFunc (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          IsPseudoBaseProcedure(Sym) OR IsPseudoBaseFunction(Sym) OR
          IsPseudoSystemFunction(Sym)
         )
END IsPseudoProcFunc ;


(*
   IsProcedureGccNested - returns TRUE if procedure, sym, will be considered
                          as nested by GCC.
                          This will occur if either its outer defining scope
                          is a procedure or is a module which is inside a
                          procedure.
*)

PROCEDURE IsProcedureGccNested (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          IsProcedureNested(sym) OR
          (IsModule(GetScope(sym)) AND IsModuleWithinProcedure(GetScope(sym)))
         )
END IsProcedureGccNested ;


(*
   IsExternal -
*)

PROCEDURE IsExternal (sym: CARDINAL) : BOOLEAN ;
VAR
   mod: CARDINAL ;
BEGIN
   Assert (NOT IsDefImp (sym)) ;
   IF IsProcedure (sym) AND IsExtern (sym)
   THEN
     RETURN TRUE
   END ;
   mod := GetScope(sym) ;
   REPEAT
      IF mod=NulSym
      THEN
         RETURN( FALSE )
      ELSIF IsDefImp(mod)
      THEN
         RETURN( mod#GetMainModule() )
      END ;
      mod := GetScope(mod)
   UNTIL mod=NulSym ;
   RETURN( FALSE )
END IsExternal ;


(*
   IsExternalToWholeProgram - return TRUE if the symbol, sym, is external to the
                              sources that we have parsed.
*)

PROCEDURE IsExternalToWholeProgram (sym: CARDINAL) : BOOLEAN ;
VAR
   mod: CARDINAL ;
BEGIN
   mod := GetScope(sym) ;
   REPEAT
      IF mod=NulSym
      THEN
         RETURN( FALSE )
      ELSIF IsDefImp(mod)
      THEN
         (* return TRUE if we have no source file.  *)
         RETURN( GetModuleFile(mod)=NIL )
      END ;
      mod := GetScope(mod)
   UNTIL mod=NulSym ;
   RETURN( FALSE )
END IsExternalToWholeProgram ;


(*
   DeclareProcedureToGccWholeProgram -
*)

PROCEDURE DeclareProcedureToGccWholeProgram (Sym: CARDINAL) ;
VAR
   returnType,
   GccParam  : tree ;
   scope,
   Variable,
   p, i      : CARDINAL ;
   b, e      : CARDINAL ;
   begin, end,
   location  : location_t ;
BEGIN
   IF (NOT GccKnowsAbout(Sym)) AND (NOT IsPseudoProcFunc(Sym))
   THEN
      BuildStartFunctionDeclaration(UsesVarArgs(Sym)) ;
      p := NoOfParamAny (Sym) ;
      i := p ;
      WHILE i>0 DO
         (* note we dont use GetNthParamAny as we want the parameter that is seen by the procedure block
            remember that this is treated exactly the same as a variable, just its position on
            the activation record is special (ie a parameter)
         *)
         Variable := GetNth(Sym, i) ;
         location := TokenToLocation(GetDeclaredMod(Variable)) ;
         IF IsUnboundedParamAny (Sym, i)
         THEN
            GccParam := BuildParameterDeclaration(location,
                                                  KeyToCharStar(GetSymName(Variable)),
                                                  Mod2Gcc(GetLType(Variable)),
                                                  FALSE)
         ELSE
            GccParam := BuildParameterDeclaration(location,
                                                  KeyToCharStar(GetSymName(Variable)),
                                                  Mod2Gcc(GetLType(Variable)),
                                                  IsVarParamAny (Sym, i))
         END ;
         PreAddModGcc(Variable, GccParam) ;
         WatchRemoveList(Variable, todolist) ;
         WatchIncludeList(Variable, fullydeclared) ;
         DEC(i)
      END ;
      GetProcedureBeginEnd(Sym, b, e) ;
      begin := TokenToLocation(b) ;
      end := TokenToLocation(e) ;
      scope := GetScope(Sym) ;
      PushBinding(scope) ;
      IF GetSType(Sym)=NulSym
      THEN
         returnType := NIL
      ELSE
         returnType := Mod2Gcc(GetSType(Sym))
      END ;
      PreAddModGcc(Sym, BuildEndFunctionDeclaration(begin, end,
                                                    KeyToCharStar(GetFullSymName(Sym)),
                                                    returnType,
                                                    IsExternalToWholeProgram(Sym),
                                                    IsProcedureGccNested(Sym),
                                                    IsExported(GetModuleWhereDeclared(Sym), Sym),
                                                    IsProcedureAnyNoReturn(Sym))) ;
      PopBinding(scope) ;
      WatchRemoveList(Sym, todolist) ;
      WatchIncludeList(Sym, fullydeclared)
   END
END DeclareProcedureToGccWholeProgram ;


(*
   DeclareProcedureToGccSeparateProgram -
*)

PROCEDURE DeclareProcedureToGccSeparateProgram (Sym: CARDINAL) ;
VAR
   returnType,
   GccParam  : tree ;
   scope,
   Variable,
   p, i      : CARDINAL ;
   b, e      : CARDINAL ;
   begin, end,
   location  : location_t ;
   tok       : CARDINAL ;
BEGIN
   tok := GetDeclaredMod(Sym) ;
   IF (NOT GccKnowsAbout(Sym)) AND (NOT IsPseudoProcFunc(Sym)) AND
      (IsEffectivelyImported(GetMainModule(), Sym) OR
       (GetModuleWhereDeclared (Sym) = GetMainModule()) OR
       IsNeededAtRunTime (tok, Sym) OR
       IsImported (GetBaseModule (), Sym) OR
       IsExported(GetModuleWhereDeclared (Sym), Sym) OR
       IsExtern (Sym))
   THEN
      BuildStartFunctionDeclaration(UsesVarArgs(Sym)) ;
      p := NoOfParamAny (Sym) ;
      i := p ;
      WHILE i>0 DO
         (* Note we dont use GetNthParamAny as we want the parameter that is seen by
            the procedure block remember that this is treated exactly the same as
            a variable, just its position on the activation record is special (ie
            a parameter).  *)
         Variable := GetNth(Sym, i) ;
         location := TokenToLocation(GetDeclaredMod(Variable)) ;
         IF IsUnboundedParamAny (Sym, i)
         THEN
            GccParam := BuildParameterDeclaration(location,
                                                  KeyToCharStar(GetSymName(Variable)),
                                                  Mod2Gcc(GetLType(Variable)),
                                                  FALSE)
         ELSE
            GccParam := BuildParameterDeclaration(location,
                                                  KeyToCharStar(GetSymName(Variable)),
                                                  Mod2Gcc(GetLType(Variable)),
                                                  IsVarParamAny (Sym, i))
         END ;
         PreAddModGcc(Variable, GccParam) ;
         WatchRemoveList(Variable, todolist) ;
         WatchIncludeList(Variable, fullydeclared) ;
         DEC(i)
      END ;
      GetProcedureBeginEnd(Sym, b, e) ;
      begin := TokenToLocation(b) ;
      end := TokenToLocation(e) ;
      scope := GetScope(Sym) ;
      PushBinding(scope) ;
      IF GetSType(Sym)=NulSym
      THEN
         returnType := NIL
      ELSE
         returnType := Mod2Gcc(GetSType(Sym))
      END ;
      PreAddModGcc (Sym, BuildEndFunctionDeclaration (begin, end,
                                                      KeyToCharStar (GetFullSymName (Sym)),
                                                      returnType,
                                                      IsExternal (Sym),  (* Extern relative to the main module.  *)
                                                      IsProcedureGccNested (Sym),
                                                      (* Exported from the module where it was declared.  *)
                                                      IsExported (GetModuleWhereDeclared (Sym), Sym) OR IsExtern (Sym),
                                                      IsProcedureAnyNoReturn(Sym))) ;
      PopBinding(scope) ;
      WatchRemoveList(Sym, todolist) ;
      WatchIncludeList(Sym, fullydeclared)
   END
END DeclareProcedureToGccSeparateProgram ;


(*
   DeclareProcedureToGcc - traverses all parameters and interfaces to gm2gcc.
*)

PROCEDURE DeclareProcedureToGcc (sym: CARDINAL) ;
BEGIN
   IF sym # NulSym
   THEN
      IF WholeProgram
      THEN
         DeclareProcedureToGccWholeProgram (sym)
      ELSE
         DeclareProcedureToGccSeparateProgram (sym)
      END
   END
END DeclareProcedureToGcc ;


(*
   DeclareProcedure - declares procedure, sym, or all procedures inside
                      module sym.
*)

PROCEDURE DeclareProcedure (sym: WORD) ;
BEGIN
   IF IsProcedure(sym)
   THEN
      DeclareProcedureToGcc(sym)
   ELSIF IsModule(sym) OR IsDefImp(sym)
   THEN
      ForeachProcedureDo(sym, DeclareProcedure)
   ELSE
      InternalError ('expecting procedure')
   END
END DeclareProcedure ;


(*
   FoldConstants - a wrapper for ResolveConstantExpressions.
*)

PROCEDURE FoldConstants (bb: BasicBlock) ;
BEGIN
   IF ResolveConstantExpressions (DeclareConstFully, bb)
   THEN
      ConstantResolved := TRUE
   END
END FoldConstants ;


(*
   ActivateWatch - activate a watch for any symbol (lista xor listb).
*)

PROCEDURE ActivateWatch (lista, listb: Set) ;
VAR
   smallest,
   largest  : Set ;
   n, sym   : CARDINAL ;
BEGIN
   IF NoOfElementsInSet (lista) # NoOfElementsInSet (listb)
   THEN
      IF NoOfElementsInSet (lista) > NoOfElementsInSet (listb)
      THEN
         largest := lista ;
         smallest := listb
      ELSE
         largest := listb ;
         smallest := lista
      END ;
      printf0 ("adding the following symbols to the watch list as the declarator has detected an internal bug: ") ;
      sym := 1 ;
      n := FinalSymbol () ;
      WHILE sym <= n DO
         IF (IsElementInSet (largest, sym) AND (NOT IsElementInSet (smallest, sym))) OR
            ((NOT IsElementInSet (largest, sym)) AND IsElementInSet (smallest, sym))
         THEN
            AddSymToWatch (sym) ;
            printf1 ("%d ", sym)
         END ;
         INC (sym)
      END ;
      printf0 ("\n")
   END
END ActivateWatch ;


(*
   DeclareTypesConstantsProceduresInRange -
*)

PROCEDURE DeclareTypesConstantsProceduresInRange (scope, start, end: CARDINAL) ;
CONST
   DebugLoop = 1000 ;
VAR
   copy: Group ;
   loop: CARDINAL ;
   sb  : ScopeBlock ;
   bb  : BasicBlock ;
BEGIN
   IF TraceQuadruples
   THEN
      DisplayQuadRange (scope, start, end)
   END ;
   loop := 0 ;
   copy := NIL ;
   sb := InitScopeBlock (scope) ;
   REPEAT
      (* Throw away any unreachable quad.  *)
      bb := InitBasicBlocks (sb) ;
      KillBasicBlocks (bb) ;
      (* Now iterate over remaining quads in scope attempting to resolve constants.  *)
      copy := DupGroup (copy) ;
      bb := InitBasicBlocks (sb) ;
      ConstantResolved := FALSE ;
      ForeachBasicBlockDo (bb, FoldConstants) ;
      KillBasicBlocks (bb) ;
      (* And now types.  *)
      IF DeclaredOutstandingTypes (FALSE)
      THEN
      END ;
      IF loop = DebugLoop
      THEN
         IF TraceQuadruples
         THEN
            DisplayQuadRange (scope, start, end)
         END ;
         ActivateWatch (copy^.ToDoList, GlobalGroup^.ToDoList) ;
         loop := 0
      END ;
      INC (loop)
   UNTIL (NOT ConstantResolved) AND EqualGroup (copy, GlobalGroup) ;
   KillGroup (copy) ;
   bb := InitBasicBlocks (sb) ;
   KillBasicBlocks (bb) ;
   KillScopeBlock (sb)
END DeclareTypesConstantsProceduresInRange ;


(*
   SkipModuleScope - skips all module scopes for, scope.
                     It returns either NulSym or a procedure sym.
*)

PROCEDURE SkipModuleScope (scope: CARDINAL) : CARDINAL ;
BEGIN
   IF (scope=NulSym) OR IsProcedure(scope)
   THEN
      RETURN( scope )
   ELSE
      RETURN( SkipModuleScope(GetScope(scope)) )
   END
END SkipModuleScope ;


(*
   PushBinding -
*)

PROCEDURE PushBinding (scope: CARDINAL) ;
BEGIN
   scope := SkipModuleScope(scope) ;
   IF scope=NulSym
   THEN
      pushGlobalScope
   ELSE
      pushFunctionScope(Mod2Gcc(scope))
   END
END PushBinding ;


(*
   PopBinding -
*)

PROCEDURE PopBinding (scope: CARDINAL) ;
BEGIN
   scope := SkipModuleScope(scope) ;
   IF scope=NulSym
   THEN
      popGlobalScope
   ELSE
      Assert(IsProcedure(scope)) ;
      finishFunctionDecl(TokenToLocation(GetDeclaredMod(scope)), Mod2Gcc(scope)) ;
      Assert (popFunctionScope () # NIL)
   END
END PopBinding ;


(*
   DeclareTypesConstantsProcedures -
*)

PROCEDURE DeclareTypesConstantsProcedures (scope: CARDINAL) ;
VAR
   copy: Group ;
   sb  : ScopeBlock ;
BEGIN
   IF Debugging
   THEN
      printf0 ("declaring types constants in: ") ; PrintTerse (scope)
   END ;
   copy := NIL ;
   sb := InitScopeBlock (scope) ;
   PushBinding (scope) ;
   REPEAT
      copy := DupGroup (copy) ;
      ForeachScopeBlockDo3 (sb, DeclareTypesConstantsProceduresInRange)
   UNTIL EqualGroup (copy, GlobalGroup) ;
   KillGroup (copy) ;
   PopBinding (scope) ;
   KillScopeBlock (sb)
END DeclareTypesConstantsProcedures ;


(*
   AssertAllTypesDeclared - asserts that all types for variables are declared in, scope.
*)

PROCEDURE AssertAllTypesDeclared (scope: CARDINAL) ;
VAR
   n, Var: CARDINAL ;
   failed: BOOLEAN ;
BEGIN
   failed := FALSE ;
   n := 1 ;
   Var := GetNth(scope, n) ;
   WHILE Var#NulSym DO
      IF NOT AllDependantsFullyDeclared(GetSType(Var))
      THEN
         mystop
      END ;
      IF NOT AllDependantsFullyDeclared(GetSType(Var))
      THEN
         EmitCircularDependancyError(GetSType(Var)) ;
         failed := TRUE
      END ;
      INC(n) ;
      Var := GetNth(scope, n)
   END ;
   IF failed
   THEN
      FlushErrors
   END
END AssertAllTypesDeclared ;


(*
   DeclareModuleInit - declare all the ctor related functions within
                       a module.
*)

PROCEDURE DeclareModuleInit (moduleSym: WORD) ;
VAR
   ctor, init, fini, dep: CARDINAL ;
BEGIN
   GetModuleCtors (moduleSym, ctor, init, fini, dep) ;
   DeclareProcedureToGcc (ctor) ;
   DeclareProcedureToGcc (init) ;
   DeclareProcedureToGcc (fini) ;
   DeclareProcedureToGcc (dep)
END DeclareModuleInit ;


(*
   StartDeclareProcedureScope -
*)

PROCEDURE StartDeclareProcedureScope (scope: CARDINAL) ;
BEGIN
   WalkTypesInProcedure(scope) ;
   DeclareProcedure(scope) ;
   ForeachInnerModuleDo(scope, WalkTypesInModule) ;
   DeclareTypesConstantsProcedures (scope) ;
   ForeachInnerModuleDo(scope, DeclareTypesConstantsProcedures) ;
   DeclareLocalVariables(scope) ;
   ForeachInnerModuleDo(scope, DeclareModuleVariables) ;
   AssertAllTypesDeclared(scope) ;
   ForeachProcedureDo(scope, DeclareProcedure) ;
   ForeachInnerModuleDo(scope, StartDeclareScope)
END StartDeclareProcedureScope ;


(*
   StartDeclareModuleScopeSeparate -
*)

PROCEDURE StartDeclareModuleScopeSeparate (scope: CARDINAL) ;
BEGIN
   IF scope=GetMainModule()
   THEN
      ForeachModuleDo(WalkTypesInModule) ;     (* will populate the TYPE and CONST ToDo list  *)
      DeclareTypesConstantsProcedures(scope) ; (* will resolved TYPEs and CONSTs on the ToDo  *)
                                               (* lists.                                      *)
      ForeachModuleDo(DeclareProcedure) ;
      (*
         now that all types have been resolved it is safe to declare
         variables
      *)
      AssertAllTypesDeclared(scope) ;
      DeclareGlobalVariables(scope) ;
      ForeachImportedDo(scope, DeclareImportedVariables) ;
      (* now it is safe to declare all procedures *)
      ForeachProcedureDo(scope, DeclareProcedure) ;
      ForeachInnerModuleDo(scope, WalkTypesInModule) ;
      ForeachInnerModuleDo(scope, DeclareTypesConstantsProcedures) ;
      ForeachInnerModuleDo(scope, StartDeclareScope) ;
      DeclareModuleInit(scope)
   ELSE
      DeclareTypesConstantsProcedures(scope) ;
      AssertAllTypesDeclared(scope) ;
      ForeachProcedureDo(scope, DeclareProcedure) ;
      DeclareModuleInit(scope) ;
      ForeachInnerModuleDo(scope, StartDeclareScope)
   END
END StartDeclareModuleScopeSeparate ;


(*
   StartDeclareModuleScopeWholeProgram -
*)

PROCEDURE StartDeclareModuleScopeWholeProgram (scope: CARDINAL) ;
BEGIN
   IF IsSourceSeen(scope)
   THEN
      ForeachModuleDo(WalkTypesInModule) ;     (* will populate the TYPE and CONST ToDo list  *)
      DeclareTypesConstantsProcedures(scope) ; (* will resolved TYPEs and CONSTs on the ToDo  *)
                                               (* lists.                                      *)
      ForeachModuleDo(DeclareProcedure) ;
      ForeachModuleDo(DeclareModuleInit) ;
      (*
         now that all types have been resolved it is safe to declare
         variables
      *)
      AssertAllTypesDeclared(scope) ;
      DeclareGlobalVariablesWholeProgram(scope) ;
      ForeachImportedDo(scope, DeclareImportedVariablesWholeProgram) ;
      (* now it is safe to declare all procedures *)
      ForeachProcedureDo(scope, DeclareProcedure) ;
      ForeachInnerModuleDo(scope, WalkTypesInModule) ;
      ForeachInnerModuleDo(scope, DeclareTypesConstantsProcedures) ;
      ForeachInnerModuleDo(scope, StartDeclareScope) ;
      DeclareModuleInit(scope)
   ELSE
      DeclareTypesConstantsProcedures(scope) ;
      AssertAllTypesDeclared(scope) ;
      ForeachProcedureDo(scope, DeclareProcedure) ;
      DeclareModuleInit(scope) ;
      ForeachInnerModuleDo(scope, StartDeclareScope)
   END
END StartDeclareModuleScopeWholeProgram ;


(*
   StartDeclareModuleScope -
*)

PROCEDURE StartDeclareModuleScope (scope: CARDINAL) ;
BEGIN
   IF WholeProgram
   THEN
      StartDeclareModuleScopeWholeProgram(scope)
   ELSE
      StartDeclareModuleScopeSeparate(scope)
   END
END StartDeclareModuleScope ;


(*
   StartDeclareScope - declares types, variables associated with this scope.
*)

PROCEDURE StartDeclareScope (scope: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   IF Debugging
   THEN
      n := GetSymName (scope) ;
      printf1 ('declaring symbols in BLOCK %a\n', n)
   END ;
   IF IsProcedure (scope)
   THEN
      StartDeclareProcedureScope (scope)
   ELSE
      StartDeclareModuleScope (scope)
   END ;
   IF Debugging
   THEN
      n := GetSymName (scope) ;
      printf1('\nEND declaring symbols in BLOCK %a\n', n)
   END
END StartDeclareScope ;


(*
   EndDeclareScope -
*)

PROCEDURE EndDeclareScope ;
BEGIN
   (* no need to do anything *)
END EndDeclareScope ;


(*
   IncludeDumpSymbol - include sym into the watch list and all syms dependants.
*)

PROCEDURE IncludeDumpSymbol (sym: CARDINAL) ;
BEGIN
   IF sym # NulSym
   THEN
      AddSymToWatch (sym)
      (*
      fprintf0 (GetDumpFile (), "\n") ;
      PrintVerbose (sym) ;
      fprintf0 (GetDumpFile (), "\n")
      *)
   END
END IncludeDumpSymbol ;


(*
   DumpResolver - dumps the m2 representation of sym.
*)

PROCEDURE DumpResolver (sym: CARDINAL) ;
BEGIN
   fprintf1 (GetDumpFile (), "dump filtered symbol %d and dependants\n", sym) ;
   PrintVerbose (sym) ;
END DumpResolver ;


(*
   DumpFilteredResolver - dumps the gimple or tree representation of all watched symbols.
*)

PROCEDURE DumpFilteredResolver ;
BEGIN
   ForeachElementInSetDo (WatchList, DumpResolver)
END DumpFilteredResolver ;


(*
   DumpDefinitive - dumps the m2 and m2 gimple representation of sym.
*)

PROCEDURE DumpDefinitive (sym: CARDINAL) ;
VAR
   fd: INTEGER ;
BEGIN
   fprintf1 (GetDumpFile (), "\nm2 symbol synopsis: %d\n", sym) ;
   PrintVerbose (sym) ;
   IF GccKnowsAbout (sym)
   THEN
      fprintf1 (GetDumpFile (), "\nm2 gimple: %d", sym) ;
      FIO.FlushBuffer (GetDumpFile ()) ;
      fd := FIO.GetUnixFileDescriptor (GetDumpFile ()) ;
      DumpGimpleFd (fd, Mod2Gcc (sym))
   ELSE
      fprintf1 (GetDumpFile (), "\nno m2 gimple for %d\n", sym)
   END
END DumpDefinitive ;


(*
   DumpFilteredDefinitive - dumps the gimple or tree representation of all watched symbols.
*)

PROCEDURE DumpFilteredDefinitive ;
BEGIN
   ForeachElementInSetDo (WatchList, DumpDefinitive)
END DumpFilteredDefinitive ;


(*
   PreAddModGcc - adds a relationship between sym and tree.
*)

PROCEDURE PreAddModGcc (sym: CARDINAL; tree: tree) ;
BEGIN
   AddModGcc (sym, tree)
END PreAddModGcc ;


(*
   DeclareDefaultType - declares a default type, sym, with, name.
*)

PROCEDURE DeclareDefaultType (sym: CARDINAL; name: ARRAY OF CHAR; gcctype: tree) ;
VAR
   t        : tree ;
   high, low: CARDINAL ;
   location : location_t ;
BEGIN
   (* DeclareDefaultType will declare a new identifier as a type of, gcctype, if it has not already been
      declared by gccgm2.c *)
   location := BuiltinsLocation () ;
   t := GetDefaultType(location, KeyToCharStar(MakeKey(name)), gcctype) ;
   AddModGcc(sym, t) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, sym) ;
   WalkAssociatedUnbounded(sym, TraverseDependants) ;
   (*
      this is very simplistic and assumes that the caller only uses Subranges, Sets and GCC types.
      We need to declare any constants with the types so that AllDependantsFullyDeclared works.
   *)
   IF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      DeclareConstant(GetDeclaredMod(sym), high) ;
      DeclareConstant(GetDeclaredMod(sym), low)
   ELSIF IsSet(sym)
   THEN
      IF IsSubrange(GetSType(sym))
      THEN
         IF NOT GccKnowsAbout(GetSType(sym))
         THEN
            (* only true for internal types of course *)
            InternalError ('subrange type within the set type must be declared before the set type')
         END ;
         GetSubrange(GetSType(sym), high, low) ;
         DeclareConstant(GetDeclaredMod(sym), high) ;
         DeclareConstant(GetDeclaredMod(sym), low)
      ELSIF IsEnumeration(GetSType(sym))
      THEN
         IF NOT GccKnowsAbout(GetSType(sym))
         THEN
            (* only true for internal types of course *)
            InternalError ('enumeration type within the set type must be declared before the set type')
         END
      END
   END
END DeclareDefaultType ;


(*
   DeclareBoolean - declares the Boolean type together with true and false.
*)

PROCEDURE DeclareBoolean ;
BEGIN
   AddModGcc(Boolean, GetBooleanType()) ;
   AddModGcc(True, GetBooleanTrue()) ;
   AddModGcc(False, GetBooleanFalse()) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, Boolean) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, True) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, False) ;
   WalkAssociatedUnbounded(Boolean, TraverseDependants)
END DeclareBoolean ;


(*
   DeclareFixedSizedType - declares the GNU Modula-2 fixed types
                           (if the back end support such a type).
*)

PROCEDURE DeclareFixedSizedType (name: ARRAY OF CHAR; type: CARDINAL; t: tree) ;
VAR
   location : location_t ;
   typetype,
   low, high: CARDINAL ;
BEGIN
   IF type#NulSym
   THEN
      IF IsSet(type) AND (NOT GccKnowsAbout(GetSType(type)))
      THEN
         typetype := GetSType(type) ;
         GetSubrange(typetype, high, low) ;
         DeclareConstant(GetDeclaredMod(type), high) ;
         DeclareConstant(GetDeclaredMod(type), low) ;
         location := TokenToLocation(GetDeclaredMod(typetype)) ;
         PreAddModGcc(typetype, BuildSubrangeType(location,
                                                  KeyToCharStar(GetFullSymName(typetype)),
                                                  Mod2Gcc(GetSType(typetype)),
                                                  Mod2Gcc(low), Mod2Gcc(high))) ;
         IncludeElementIntoSet(GlobalGroup^.FullyDeclared, typetype) ;
         WalkAssociatedUnbounded(typetype, TraverseDependants)
      END ;
      (* gcc back end supports, type *)
      DeclareDefaultType(type, name, t)
   END
END DeclareFixedSizedType ;


(*
   DeclareDefaultSimpleTypes - declares the simple types.
*)

PROCEDURE DeclareDefaultSimpleTypes ;
BEGIN
   AddModGcc(ZType, GetM2ZType()) ;
   AddModGcc(RType, GetM2RType()) ;
   AddModGcc(CType, GetM2CType()) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, ZType) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, RType) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, CType) ;

   DeclareDefaultType(Cardinal    , "CARDINAL"    , GetM2CardinalType()) ;
   DeclareDefaultType(Integer     , "INTEGER"     , GetM2IntegerType()) ;
   DeclareDefaultType(Char        , "CHAR"        , GetM2CharType()) ;
   DeclareDefaultType(Loc         , "LOC"         , GetISOLocType()) ;

   IF Iso
   THEN
      DeclareDefaultType(Byte     , "BYTE"        , GetISOByteType()) ;
      DeclareDefaultType(Word     , "WORD"        , GetISOWordType())
   ELSE
      DeclareDefaultType(Byte     , "BYTE"        , GetByteType()) ;
      DeclareDefaultType(Word     , "WORD"        , GetWordType())
   END ;

   DeclareDefaultType(Proc        , "PROC"        , GetProcType()) ;
   DeclareDefaultType(Address     , "ADDRESS"     , GetPointerType()) ;
   DeclareDefaultType(LongInt     , "LONGINT"     , GetM2LongIntType()) ;
   DeclareDefaultType(LongCard    , "LONGCARD"    , GetM2LongCardType()) ;
   DeclareDefaultType(ShortInt    , "SHORTINT"    , GetM2ShortIntType()) ;
   DeclareDefaultType(ShortCard   , "SHORTCARD"   , GetM2ShortCardType()) ;
   DeclareDefaultType(ShortReal   , "SHORTREAL"   , GetM2ShortRealType()) ;
   DeclareDefaultType(Real        , "REAL"        , GetM2RealType()) ;
   DeclareDefaultType(LongReal    , "LONGREAL"    , GetM2LongRealType()) ;
   DeclareDefaultType(Bitnum      , "BITNUM"      , GetBitnumType()) ;
   DeclareDefaultType(Bitset      , "BITSET"      , GetBitsetType()) ;
   DeclareDefaultType(Complex     , "COMPLEX"     , GetM2ComplexType()) ;
   DeclareDefaultType(LongComplex , "LONGCOMPLEX" , GetM2LongComplexType()) ;
   DeclareDefaultType(ShortComplex, "SHORTCOMPLEX", GetM2ShortComplexType()) ;
   DeclareDefaultType(CSizeT      , "CSIZE_T"     , GetCSizeTType()) ;
   DeclareDefaultType(CSSizeT     , "CSSIZE_T"    , GetCSSizeTType()) ;
   DeclareDefaultType(COffT       , "COFF_T"      , GetCOffTType()) ;

   DeclareBoolean ;

   DeclareFixedSizedType("INTEGER8"  , IntegerN(8)  , GetM2Integer8()) ;
   DeclareFixedSizedType("INTEGER16" , IntegerN(16) , GetM2Integer16()) ;
   DeclareFixedSizedType("INTEGER32" , IntegerN(32) , GetM2Integer32()) ;
   DeclareFixedSizedType("INTEGER64" , IntegerN(64) , GetM2Integer64()) ;
   DeclareFixedSizedType("CARDINAL8" , CardinalN(8) , GetM2Cardinal8()) ;
   DeclareFixedSizedType("CARDINAL16", CardinalN(16), GetM2Cardinal16()) ;
   DeclareFixedSizedType("CARDINAL32", CardinalN(32), GetM2Cardinal32()) ;
   DeclareFixedSizedType("CARDINAL64", CardinalN(64), GetM2Cardinal64()) ;
   DeclareFixedSizedType("WORD16"    , WordN(16)    , GetM2Word16()) ;
   DeclareFixedSizedType("WORD32"    , WordN(32)    , GetM2Word32()) ;
   DeclareFixedSizedType("WORD64"    , WordN(64)    , GetM2Word64()) ;
   DeclareFixedSizedType("BITSET8"   , SetN(8)      , GetM2Bitset8()) ;
   DeclareFixedSizedType("BITSET16"  , SetN(16)     , GetM2Bitset16()) ;
   DeclareFixedSizedType("BITSET32"  , SetN(32)     , GetM2Bitset32()) ;
   DeclareFixedSizedType("REAL32"    , RealN(32)    , GetM2Real32()) ;
   DeclareFixedSizedType("REAL64"    , RealN(64)    , GetM2Real64()) ;
   DeclareFixedSizedType("REAL96"    , RealN(96)    , GetM2Real96()) ;
   DeclareFixedSizedType("REAL128"   , RealN(128)   , GetM2Real128()) ;
   DeclareFixedSizedType("COMPLEX32" , ComplexN(32) , GetM2Complex32()) ;
   DeclareFixedSizedType("COMPLEX64" , ComplexN(64) , GetM2Complex64()) ;
   DeclareFixedSizedType("COMPLEX96" , ComplexN(96) , GetM2Complex96()) ;
   DeclareFixedSizedType("COMPLEX128", ComplexN(128), GetM2Complex128())
END DeclareDefaultSimpleTypes ;


(*
   DeclarePackedBoolean -
*)

PROCEDURE DeclarePackedBoolean ;
VAR
   e: CARDINAL ;
BEGIN
   e := GetPackedEquivalent(Boolean) ;
   AddModGcc(e, GetPackedBooleanType()) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, e)
END DeclarePackedBoolean ;


(*
   DeclarePackedDefaultSimpleTypes -
*)

PROCEDURE DeclarePackedDefaultSimpleTypes ;
BEGIN
   DeclarePackedBoolean
END DeclarePackedDefaultSimpleTypes ;


(*
   DeclareDefaultTypes - makes default types known to GCC
*)

PROCEDURE DeclareDefaultTypes ;
BEGIN
   IF NOT HaveInitDefaultTypes
   THEN
      HaveInitDefaultTypes := TRUE ;
      pushGlobalScope ;
      DeclareDefaultSimpleTypes ;
      DeclarePackedDefaultSimpleTypes ;
      popGlobalScope
   END
END DeclareDefaultTypes ;


(*
   DeclareDefaultConstants - make default constants known to GCC
*)

PROCEDURE DeclareDefaultConstants ;
BEGIN
   AddModGcc(Nil, GetPointerZero(BuiltinsLocation ())) ;
   IncludeElementIntoSet(GlobalGroup^.FullyDeclared, Nil)
END DeclareDefaultConstants ;


(*
   FindContext - returns the scope where the symbol
                 should be created.

                 Symbols created in a module will
                 return the global context tree, but symbols created
                 in a module which is declared inside
                 a procedure will return the procedure Tree.
*)

PROCEDURE FindContext (sym: CARDINAL) : tree ;
BEGIN
   sym := GetProcedureScope(sym) ;
   IF sym=NulSym
   THEN
      RETURN( GetGlobalContext() )
   ELSE
      RETURN( Mod2Gcc(sym) )
   END
END FindContext ;


(*
   IsEffectivelyImported - returns TRUE if symbol, Sym, was
                           effectively imported into ModSym.
*)

PROCEDURE IsEffectivelyImported (ModSym, sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          IsImported(ModSym, sym) OR
          (IsImported(ModSym, GetModuleWhereDeclared(sym)) AND
           IsExported(GetModuleWhereDeclared(sym), sym))
         )
END IsEffectivelyImported ;


(*
   FindOuterModule - returns the out most module where, sym,
                     was declared.  It returns NulSym if the
                     symbol or the module was declared inside
                     a procedure.
*)

PROCEDURE FindOuterModule (sym: CARDINAL) : CARDINAL ;
BEGIN
   sym := GetScope(sym) ;
   WHILE (NOT IsDefImp(sym)) DO
      IF IsModule(sym)
      THEN
         IF GetScope(sym)=NulSym
         THEN
            RETURN( sym )
         ELSE
            sym := GetScope(sym)
         END
      ELSIF IsProcedure(sym)
      THEN
         sym := GetScope(sym)
      END
   END ;
   RETURN( sym )
END FindOuterModule ;


(*
   DoVariableDeclaration - create a corresponding gcc variable and add the association
                           between the front end symbol var and the gcc tree.
*)

PROCEDURE DoVariableDeclaration (var: CARDINAL; name: ADDRESS;
                                 isImported, isExported,
                                 isTemporary, isGlobal: BOOLEAN;
                                 scope: tree) ;
VAR
   type    : tree ;
   varType : CARDINAL ;
   location: location_t ;
BEGIN
   IF IsComponent (var) OR IsVarHeap (var)
   THEN
      RETURN
   END ;
   IF GetMode (var) = LeftValue
   THEN
      (*
        There are two issues to deal with:

        (i)   LeftValue is really a pointer to GetSType (var), which is built
              here.
        (ii)  Front end might have specified the back end use a particular
              data type, in which case we use the specified type.
              We do not add an extra pointer if this is the case.
      *)
      varType := SkipType (GetVarBackEndType (var)) ;
      IF varType=NulSym
      THEN
         (* We have not explicity told back end the type, so build it.  *)
         varType := GetSType (var) ;
         IF IsVariableAtAddress (var)
         THEN
            type := BuildConstPointerType (Mod2Gcc (varType))
         ELSE
            type := BuildPointerType (Mod2Gcc (varType))
         END
      ELSE
         (* We have been requested to use varType.  *)
         type := Mod2Gcc (varType)
      END ;
      Assert (AllDependantsFullyDeclared (varType))
   ELSE
      type := Mod2Gcc (GetDType (var))
   END ;
   location := TokenToLocation (GetDeclaredMod (var)) ;
   PreAddModGcc (var, DeclareKnownVariable (location,
                                            name, type,
                                            isExported, isImported, isTemporary,
                                            isGlobal, scope, NIL)) ;
   WatchRemoveList (var, todolist) ;
   WatchIncludeList (var, fullydeclared)
END DoVariableDeclaration ;


(*
   IsGlobal - is the variable not in a procedure scope.
*)

PROCEDURE IsGlobal (sym: CARDINAL) : BOOLEAN ;
VAR
   s: CARDINAL ;
BEGIN
   s := GetScope(sym) ;
   WHILE (s#NulSym) AND (NOT IsDefImp (s)) AND (NOT IsModule (s)) DO
      IF IsProcedure (s)
      THEN
         RETURN FALSE
      END ;
      s := GetScope (s)
   END ;
   RETURN TRUE
END IsGlobal ;


(*
   DeclareVariable - declares a global variable to GCC.
*)

PROCEDURE DeclareVariable (ModSym, variable: CARDINAL) ;
VAR
   scope: tree ;
   decl : CARDINAL ;
BEGIN
   IF NOT GccKnowsAbout (variable)
   THEN
      scope := FindContext (ModSym) ;
      decl := FindOuterModule (variable) ;
      Assert (AllDependantsFullyDeclared (GetSType (variable))) ;
      PushBinding (ModSym) ;
      DoVariableDeclaration (variable,
                             KeyToCharStar (GetFullSymName (variable)),
                             (* in Modula-2 we are allowed to import from ourselves, but we do not present this to GCC *)
                             IsEffectivelyImported(ModSym, variable) AND (GetMainModule () # decl),
                             IsExported(ModSym, variable),
                             IsTemporary (variable),
                             IsGlobal (variable),
                             scope) ;
      PopBinding (ModSym)
   END
END DeclareVariable ;


(*
   DeclareVariableWholeProgram - declares a global variable to GCC when using -fm2-whole-program.
*)

PROCEDURE DeclareVariableWholeProgram (mainModule, variable: CARDINAL) ;
VAR
   scope: tree ;
   decl : CARDINAL ;
BEGIN
   IF NOT GccKnowsAbout (variable)
   THEN
      scope := FindContext (mainModule) ;
      decl := FindOuterModule (variable) ;
      Assert (AllDependantsFullyDeclared (GetSType (variable))) ;
      PushBinding (mainModule) ;
      DoVariableDeclaration (variable,
                             KeyToCharStar (GetFullSymName (variable)),
                             (NOT IsSourceSeen (decl)) AND
                             IsEffectivelyImported (mainModule, variable) AND (GetMainModule () # decl),
                             IsExported (mainModule, variable),
                             IsTemporary (variable),
                             IsGlobal (variable),
                             scope) ;
      PopBinding (mainModule)
   END
END DeclareVariableWholeProgram ;


(*
   DeclareGlobalVariablesWholeProgram -
*)

PROCEDURE DeclareGlobalVariablesWholeProgram (ModSym: CARDINAL) ;
VAR
   n, Variable: CARDINAL ;
BEGIN
   n := 1 ;
   Variable := GetNth (ModSym, n) ;
   WHILE Variable # NulSym DO
      DeclareVariableWholeProgram (ModSym, Variable) ;
      INC (n) ;
      Variable := GetNth (ModSym, n)
   END ;
   ForeachInnerModuleDo(ModSym, DeclareGlobalVariablesWholeProgram)
END DeclareGlobalVariablesWholeProgram ;


(*
   DeclareGlobalVariables - lists the Global variables for
                            Module ModSym together with their offset.
*)

PROCEDURE DeclareGlobalVariables (ModSym: CARDINAL) ;
VAR
   n, Variable: CARDINAL ;
BEGIN
   n := 1 ;
   Variable := GetNth (ModSym, n) ;
   WHILE Variable # NulSym DO
      DeclareVariable (ModSym, Variable) ;
      INC (n) ;
      Variable := GetNth (ModSym, n)
   END ;
   ForeachInnerModuleDo (ModSym, DeclareGlobalVariables)
END DeclareGlobalVariables ;


(*
   DeclareImportedVariables - declares all imported variables to GM2.
*)

PROCEDURE DeclareImportedVariables (sym: WORD) ;
BEGIN
   IF IsVar (sym)
   THEN
      DeclareVariable (GetMainModule (), sym)
   ELSIF IsDefImp (sym)
   THEN
      ForeachExportedDo (sym, DeclareImportedVariables)
   END
END DeclareImportedVariables ;


(*
   DeclareImportedVariablesWholeProgram - declares all imported variables.
*)

PROCEDURE DeclareImportedVariablesWholeProgram (sym: WORD) ;
BEGIN
   IF IsVar (sym)
   THEN
      IF NOT IsSourceSeen (FindOuterModule (sym))
      THEN
         (* import is necessary, even for -fm2-whole-program as we
            cannot see the source.  *)
         DeclareVariableWholeProgram (GetMainModule (), sym)
      END
   ELSIF IsDefImp (sym)
   THEN
      ForeachExportedDo (sym, DeclareImportedVariablesWholeProgram)
   END
END DeclareImportedVariablesWholeProgram ;


(*
   DeclareLocalVariable - declare a local variable var.
*)

PROCEDURE DeclareLocalVariable (var: CARDINAL) ;
BEGIN
   Assert (AllDependantsFullyDeclared (var)) ;
   DoVariableDeclaration (var,
                          KeyToCharStar (GetFullSymName (var)),
                          FALSE,  (* local variables cannot be imported *)
                          FALSE,  (* or exported *)
                          IsTemporary (var),
                          FALSE,  (* and are not global *)
                          Mod2Gcc (GetScope (var)))
END DeclareLocalVariable ;


(*
   DeclareLocalVariables - declares Local variables for procedure.
*)

PROCEDURE DeclareLocalVariables (procedure: CARDINAL) ;
VAR
   i, var: CARDINAL ;
BEGIN
   i := NoOfParamAny (procedure) + 1 ;
   var := GetNth (procedure, i) ;
   WHILE var # NulSym DO
      Assert (procedure = GetScope (var)) ;
      DeclareLocalVariable (var) ;
      INC (i) ;
      var := GetNth (procedure, i)
   END
END DeclareLocalVariables ;


(*
   DeclareModuleVariables - declares Module variables for a module
                            which was declared inside a procedure.
*)

PROCEDURE DeclareModuleVariables (sym: CARDINAL) ;
VAR
   scope : tree ;
   i, Var: CARDINAL ;
BEGIN
   i := 1 ;
   scope := Mod2Gcc (GetProcedureScope (sym)) ;
   Var := GetNth (sym, i) ;
   WHILE Var # NulSym DO
      Assert (AllDependantsFullyDeclared (GetSType (Var))) ;
      DoVariableDeclaration (Var,
                             KeyToCharStar (GetFullSymName (Var)),
                             FALSE,   (* inner module variables cannot be imported *)
                             FALSE,   (* or exported (as far as GCC is concerned)  *)
                             IsTemporary (Var),
                             FALSE,   (* and are not global *)
                             scope) ;
      INC (i) ;
      Var := GetNth (sym, i)
   END
END DeclareModuleVariables ;


(*
   DeclareFieldValue -
*)

PROCEDURE DeclareFieldValue (sym: CARDINAL; value: tree; VAR list: tree) : tree ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(sym)) ;
   IF (GetModuleWhereDeclared(sym)=NulSym) OR
      (GetModuleWhereDeclared(sym)=GetMainModule())
   THEN
      RETURN( BuildEnumerator(location, KeyToCharStar(GetSymName(sym)), value, list) )
   ELSE
      RETURN( BuildEnumerator(location, KeyToCharStar(GetFullScopeAsmName(sym)), value, list) )
   END
END DeclareFieldValue ;


(*
   DeclareFieldEnumeration - declares an enumerator within the current enumeration type.
*)

PROCEDURE DeclareFieldEnumeration (sym: WORD) : tree ;
VAR
   type    : CARDINAL ;
   field,
   enumlist: tree ;
BEGIN
   (* add relationship between gccSym and sym *)
   type := GetSType (sym) ;
   enumlist := GetEnumList (type) ;
   PushValue (sym) ;
   field := DeclareFieldValue (sym, PopIntegerTree (), enumlist) ;
   PutEnumList (type, enumlist) ;
   RETURN field
END DeclareFieldEnumeration ;


(*
   DeclareEnumeration - declare an enumerated type.
*)

PROCEDURE DeclareEnumeration (sym: WORD) : tree ;
VAR
   enumlist,
   gccenum : tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation (GetDeclaredMod (sym)) ;
   gccenum := BuildStartEnumeration (location, KeyToCharStar (GetFullSymName (sym)), FALSE) ;
   enumlist := GetEnumList (sym) ;
   RETURN BuildEndEnumeration (location, gccenum, enumlist)
END DeclareEnumeration ;


(*
   DeclareSubrangeNarrow - will return cardinal, integer, or type depending on whether
                           low..high fits in the C data type.
*)

PROCEDURE DeclareSubrangeNarrow (location: location_t;
                                 high, low: CARDINAL; type: tree) : tree ;
VAR
   m2low, m2high,
   lowtree,
   hightree     : tree ;
BEGIN
   (* No zero alignment, therefore the front end will prioritize subranges to match
      unsigned int, int, or ZTYPE assuming the low..high range fits.  *)
   lowtree := Mod2Gcc (low) ;
   hightree := Mod2Gcc (high) ;
   IF CompareTrees (lowtree, GetIntegerZero (location)) >= 0
   THEN
      (* low..high is always positive, can we use unsigned int?  *)
      m2high := GetMaxFrom (location, GetM2CardinalType ()) ;
      IF CompareTrees (hightree, m2high) <= 0
      THEN
         RETURN GetM2CardinalType ()
      END
   ELSE
      (* Must be a signed subrange base, can we use int?  *)
      m2high := GetMaxFrom (location, GetM2IntegerType ()) ;
      m2low := GetMinFrom (location, GetM2IntegerType ()) ;
      IF (CompareTrees (lowtree, m2low) >= 0) AND (CompareTrees (hightree, m2high) <= 0)
      THEN
         RETURN GetM2IntegerType ()
      END
   END ;
   (* Fall back to the ZType.  *)
   RETURN type
END DeclareSubrangeNarrow ;


(*
   DeclareSubrange - declare a subrange type.
*)

PROCEDURE DeclareSubrange (sym: CARDINAL) : tree ;
VAR
   type,
   gccsym   : tree ;
   align,
   high, low: CARDINAL ;
   location: location_t ;
BEGIN
   location := TokenToLocation (GetDeclaredMod (sym)) ;
   GetSubrange (sym, high, low) ;
   align := GetAlignment (sym) ;
   type := Mod2Gcc (GetSType (sym)) ;
   IF align # NulSym
   THEN
      IF AreConstantsEqual (GetIntegerZero (location), Mod2Gcc (align))
      THEN
         type := BuildSmallestTypeRange (location, Mod2Gcc (low), Mod2Gcc (high))
      ELSE
         MetaError1 ('a non-zero alignment in a subrange type {%1Wa} is currently not implemented and will be ignored',
                     sym)
      END
   ELSIF GetSType (sym) = ZType
   THEN
      (* Can we narrow the ZType subrange to CARDINAL or INTEGER?  *)
      type := DeclareSubrangeNarrow (location, high, low, type)
   END ;
   gccsym := BuildSubrangeType (location,
                                KeyToCharStar (GetFullSymName (sym)),
                                type, Mod2Gcc (low), Mod2Gcc (high)) ;
   RETURN gccsym
END DeclareSubrange ;


(*
   IncludeGetNth -
*)

PROCEDURE IncludeGetNth (l: List; sym: CARDINAL) ;
VAR
   i: CARDINAL ;
BEGIN
   fprintf0 (GetDumpFile (), ' ListOfFields [') ;
   i := 1 ;
   WHILE GetNth (sym, i) # NulSym DO
      IF i>1
      THEN
         fprintf0 (GetDumpFile (), ', ')
      END ;
      IncludeItemIntoList (l, GetNth(sym, i)) ;
      PrintTerse (GetNth (sym, i)) ;
      INC (i)
   END ;
   fprintf0 (GetDumpFile (), ']')
END IncludeGetNth ;


(*
   IncludeType -
*)

PROCEDURE IncludeType (l: List; sym: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   t := GetSType(sym) ;
   IF t#NulSym
   THEN
      fprintf0 (GetDumpFile(), ' type [') ;
      PrintTerse(t) ;
      IncludeItemIntoList(l, t) ;
      fprintf0 (GetDumpFile(), ']') ;
      t := GetVarBackEndType(sym) ;
      IF t#NulSym
      THEN
         fprintf0 (GetDumpFile(), ' gcc type [') ;
         PrintTerse(t) ;
         IncludeItemIntoList(l, t) ;
         fprintf0 (GetDumpFile(), ']')
      END
   END
END IncludeType ;


(*
   IncludeSubscript -
*)

PROCEDURE IncludeSubscript (l: List; sym: CARDINAL) ;
VAR
   t: CARDINAL ;
BEGIN
   t := GetArraySubscript(sym) ;
   IF t#NulSym
   THEN
      fprintf0 (GetDumpFile(), ' subrange [') ;
      PrintTerse(t) ;
      IncludeItemIntoList(l, t) ;
      fprintf0 (GetDumpFile(), ']') ;
   END
END IncludeSubscript ;


(*
   PrintLocalSymbol -
*)

PROCEDURE PrintLocalSymbol (sym: CARDINAL) ;
BEGIN
   PrintTerse(sym) ; fprintf0 (GetDumpFile(), ', ')
END PrintLocalSymbol ;


(*
   PrintLocalSymbols -
*)

PROCEDURE PrintLocalSymbols (sym: CARDINAL) ;
BEGIN
   fprintf0 (GetDumpFile(), 'Local Symbols {') ;
   ForeachLocalSymDo(sym, PrintLocalSymbol) ;
   fprintf0 (GetDumpFile(), '}')
END PrintLocalSymbols ;


(*
   IncludeGetVarient -
*)

PROCEDURE IncludeGetVarient (l: List; sym: CARDINAL) ;
BEGIN
   IF GetVarient(sym)#NulSym
   THEN
      fprintf0 (GetDumpFile(), ' Varient [') ;
      PrintTerse(GetVarient(sym)) ;
      fprintf0 (GetDumpFile(), ']') ;
      IncludeItemIntoList(l, GetVarient(sym))
   END
END IncludeGetVarient ;


(*
   IncludeUnbounded - includes the record component of an unbounded type.
*)

PROCEDURE IncludeUnbounded (l: List; sym: CARDINAL) ;
BEGIN
   IF GetUnboundedRecordType(sym)#NulSym
   THEN
      IncludeItemIntoList(l, GetUnboundedRecordType(sym))
   END
END IncludeUnbounded ;


(*
   IncludePartialUnbounded - includes the type component of a partial unbounded symbol.
*)

PROCEDURE IncludePartialUnbounded (l: List; sym: CARDINAL) ;
BEGIN
   IF GetSType(sym)#NulSym
   THEN
      IncludeItemIntoList(l, GetSType(sym))
   END
END IncludePartialUnbounded ;


(*
   PrintDeclared - prints out where, sym, was declared.
*)

PROCEDURE PrintDeclared (sym: CARDINAL) ;
VAR
   filename: String ;
   lineno,
   tokenno : CARDINAL ;
BEGIN
   tokenno := GetDeclaredMod(sym) ;
   filename := FindFileNameFromToken(tokenno, 0) ;
   lineno := TokenToLineNo(tokenno, 0) ;
   fprintf2 (GetDumpFile (), " declared in %s:%d", filename, lineno)
END PrintDeclared ;


(*
   PrintAlignment -
*)

PROCEDURE PrintAlignment (sym: CARDINAL) ;
VAR
   align: CARDINAL ;
BEGIN
   IF IsRecord(sym) OR IsType(sym) OR IsRecordField(sym) OR IsPointer(sym) OR IsArray(sym)
   THEN
      align := GetAlignment(sym) ;
      IF align#NulSym
      THEN
         fprintf1 (GetDumpFile(), " aligned [%d]", align)
      END
   END
END PrintAlignment ;


(*
   IncludeGetParent -
*)

PROCEDURE IncludeGetParent (l: List; sym: CARDINAL) ;
BEGIN
   fprintf0 (GetDumpFile(), ' Parent [') ;
   IncludeItemIntoList(l, GetParent(sym)) ;
   PrintTerse(GetParent(sym)) ;
   fprintf0 (GetDumpFile(), ']')
END IncludeGetParent ;


(*
   PrintDecl -
*)

PROCEDURE PrintDecl (sym: CARDINAL) ;
BEGIN
   IF IsDeclaredPackedResolved(sym)
   THEN
      IF IsDeclaredPacked(sym)
      THEN
         fprintf0 (GetDumpFile(), ' packed')
      ELSE
         fprintf0 (GetDumpFile(), ' unpacked')
      END
   ELSE
      fprintf0 (GetDumpFile(), ' unknown if packed')
   END
END PrintDecl ;


(*
   PrintScope - displays the scope and line number of declaration of symbol, sym.
*)

PROCEDURE PrintScope (sym: CARDINAL) ;
VAR
   name : Name ;
   scope,
   line : CARDINAL ;
BEGIN
   line := TokenToLineNo (GetDeclaredMod (sym), 0) ;
   scope := GetScope (sym) ;
   name := GetSymName (scope) ;
   fprintf3 (GetDumpFile (), ' scope %a:%d %d', name, line, scope)
END PrintScope ;


(*
   PrintKind -
*)

PROCEDURE PrintKind (kind: ProcedureKind) ;
VAR
   s: String ;
BEGIN
   s := GetProcedureKindDesc (kind) ;
   fprintf1 (GetDumpFile (), "%s", s) ;
   s := KillString (s)
END PrintKind ;


(*
   PrintProcedureParameters -
*)

PROCEDURE PrintProcedureParameters (sym: CARDINAL; kind: ProcedureKind) ;
VAR
   typeName,
   paramName: Name ;
   p, i, n,
   type     : CARDINAL ;
BEGIN
   fprintf0 (GetDumpFile (), ' (') ;
   n := NoOfParam (sym, kind) ;
   i := 1 ;
   WHILE i <= n DO
      IF i > 1
      THEN
         fprintf0 (GetDumpFile (), '; ')
      END ;
      IF IsVarParam (sym, kind, i)
      THEN
         fprintf0 (GetDumpFile (), 'VAR ')
      END ;
      p := GetNthParam (sym, kind, i) ;
      paramName := GetSymName (p) ;
      type := GetType (p) ;
      typeName := GetSymName (type) ;
      IF IsUnboundedParam (sym, kind, i)
      THEN
         fprintf2 (GetDumpFile (), '%a: ARRAY OF %a', paramName, typeName)
      ELSE
         fprintf2 (GetDumpFile (), '%a: %a', paramName, typeName)
      END ;
      INC (i)
   END ;
   fprintf0 (GetDumpFile (), ')')
END PrintProcedureParameters ;


(*
   PrintProcedureReturnType -
*)

PROCEDURE PrintProcedureReturnType (sym: CARDINAL) ;
VAR
   typeName: Name ;
BEGIN
   IF GetType (sym) # NulSym
   THEN
      typeName := GetSymName (GetType (sym)) ;
      fprintf1 (GetDumpFile (), ' : %a', typeName)
   END ;
   fprintf0 (GetDumpFile (), ' ;')
END PrintProcedureReturnType ;


(*
   PrintProcedure -
*)

PROCEDURE PrintProcedure (sym: CARDINAL) ;
VAR
   n   : Name ;
   kind: ProcedureKind ;
BEGIN
   n := GetSymName (sym) ;
   fprintf2 (GetDumpFile (), 'sym %d IsProcedure (%a)', sym, n);
   IF IsProcedureReachable(sym)
   THEN
      fprintf0 (GetDumpFile(), ' IsProcedureReachable')
   END ;
   PrintScope (sym) ;
   IF IsExtern (sym)
   THEN
      fprintf0 (GetDumpFile (), ' extern')
   END ;
   IF IsPublic (sym)
   THEN
      fprintf0 (GetDumpFile (), ' public')
   END ;
   IF IsCtor (sym)
   THEN
      fprintf0 (GetDumpFile (), ' ctor')
   END ;
   PrintDeclared (sym) ;
   fprintf0 (GetDumpFile (), '\n') ;
   FOR kind := MIN (ProcedureKind) TO MAX (ProcedureKind) DO
      fprintf0 (GetDumpFile (), 'parameters ') ;
      PrintKind (kind) ;
      IF GetProcedureParametersDefined (sym, kind)
      THEN
         fprintf0 (GetDumpFile (), ' defined') ;
         PrintProcedureParameters (sym, kind) ;
         PrintProcedureReturnType (sym)
      ELSE
         fprintf0 (GetDumpFile (), ' undefined')
      END ;
      fprintf0 (GetDumpFile (), '\n')
   END ;
   fprintf0 (GetDumpFile (), ' Associated proctype: ') ;
   PrintProcType (GetProcedureProcType (sym))
END PrintProcedure ;


(*
   PrintProcTypeParameters -
*)

PROCEDURE PrintProcTypeParameters (sym: CARDINAL) ;
VAR
   typeName : Name ;
   p, i, n,
   type     : CARDINAL ;
BEGIN
   fprintf0 (GetDumpFile (), ' (') ;
   n := NoOfParam (sym, ProperProcedure) ;
   i := 1 ;
   WHILE i <= n DO
      IF i > 1
      THEN
         fprintf0 (GetDumpFile (), '; ')
      END ;
      IF IsVarParam (sym, ProperProcedure, i)
      THEN
         fprintf0 (GetDumpFile (), 'VAR ')
      END ;
      p := GetNthParam (sym, ProperProcedure, i) ;
      type := GetType (p) ;
      typeName := GetSymName (type) ;
      IF IsUnboundedParam (sym, ProperProcedure, i)
      THEN
         fprintf1 (GetDumpFile (), 'ARRAY OF %a', typeName)
      ELSE
         fprintf1 (GetDumpFile (), '%a', typeName)
      END ;
      INC (i)
   END ;
   fprintf0 (GetDumpFile (), ')')
END PrintProcTypeParameters ;


(*
   PrintProcType -
*)

PROCEDURE PrintProcType (sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   n := GetSymName (sym) ;
   fprintf2 (GetDumpFile (), 'sym %d IsProcType (%a)', sym, n);
   PrintScope (sym) ;
   PrintDeclared (sym) ;
   fprintf0 (GetDumpFile (), '\n') ;
   fprintf0 (GetDumpFile (), 'parameters ') ;
   PrintProcTypeParameters (sym) ;
   PrintProcedureReturnType (sym) ;
   fprintf0 (GetDumpFile (), '\n')
END PrintProcType ;


(*
   PrintString -
*)

PROCEDURE PrintString (sym: CARDINAL) ;
VAR
   len    : CARDINAL ;
   tokenno: CARDINAL ;
BEGIN
   IF IsConstStringKnown (sym)
   THEN
      IF IsConstStringM2 (sym)
      THEN
         fprintf0 (GetDumpFile (), 'a Modula-2 string')
      ELSIF IsConstStringC (sym)
      THEN
         fprintf0 (GetDumpFile (), ' a C string')
      ELSIF IsConstStringM2nul (sym)
      THEN
         fprintf0 (GetDumpFile (), ' a nul terminated Modula-2 string')
      ELSIF IsConstStringCnul (sym)
      THEN
         fprintf0 (GetDumpFile (), ' a nul terminated C string')
      END ;
      tokenno := GetDeclaredMod (sym) ;
      len := GetStringLength (tokenno, sym) ;
      fprintf1 (GetDumpFile (), ' length %d', len)
   ELSE
      fprintf0 (GetDumpFile (), 'is not currently known')
   END
END PrintString ;


(*
   PrintVerboseFromList - prints the, i, th element in the list, l.
*)

PROCEDURE PrintVerboseFromList (l: List; i: CARDINAL) ;
VAR
   type,
   low,
   high,
   sym   : CARDINAL ;
   n, n2 : Name ;
BEGIN
   sym := GetItemFromList(l, i) ;
   n := GetSymName(sym) ;
   IF IsError(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsError (%a)', sym, n)
   ELSIF IsDefImp(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsDefImp (%a)', sym, n) ;
      IF IsDefinitionForC(sym)
      THEN
         fprintf0 (GetDumpFile(), 'and IsDefinitionForC')
      END ;
      IF IsHiddenTypeDeclared(sym)
      THEN
         fprintf0 (GetDumpFile(), ' IsHiddenTypeDeclared')
      END ;
      ForeachProcedureDo (sym, PrintProcedure)
   ELSIF IsModule(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsModule (%a)', sym, n) ;
      IF IsModuleWithinProcedure(sym)
      THEN
         fprintf0 (GetDumpFile(), ' and IsModuleWithinProcedure')
      END
   ELSIF IsInnerModule(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsInnerModule (%a)', sym, n)
   ELSIF IsUnknown(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsUnknown (%a)', sym, n)
   ELSIF IsType(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsType (%a)', sym, n) ;
      IncludeType(l, sym) ;
      PrintAlignment(sym)
   ELSIF IsProcedure(sym)
   THEN
      PrintProcedure (sym)
   ELSIF IsParameter(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsParameter (%a)', sym, n) ;
      IF GetParameterShadowVar(sym)=NulSym
      THEN
         fprintf0 (GetDumpFile(), ' no shadow local variable')
      ELSE
         fprintf0 (GetDumpFile(), ' shadow ') ;
         IncludeType(l, GetParameterShadowVar(sym))
         (* PrintVerboseFromList(l, GetParameterShadowVar(sym)) *)
      END ;
      IncludeType(l, sym)
   ELSIF IsPointer(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsPointer (%a)', sym, n) ;
      IncludeType(l, sym) ;
      PrintAlignment(sym)
   ELSIF IsRecord(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsRecord (%a)', sym, n) ;
      PrintLocalSymbols(sym) ;
      IncludeGetNth(l, sym) ;
      PrintAlignment(sym) ;
      PrintDecl(sym)
   ELSIF IsVarient(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsVarient (%a)', sym, n) ;
      PrintDecl(sym) ;
      IncludeGetNth(l, sym) ;
      IncludeGetVarient(l, sym) ;
      IncludeGetParent(l, sym)
   ELSIF IsFieldVarient(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsFieldVarient (%a)', sym, n) ;
      PrintDecl(sym) ;
      IncludeGetNth(l, sym) ;
      IncludeGetVarient(l, sym) ;
      IncludeGetParent(l, sym)
   ELSIF IsFieldEnumeration(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsFieldEnumeration (%a)', sym, n)
   ELSIF IsArray(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsArray (%a)', sym, n) ;
      IncludeSubscript(l, sym) ;
      IncludeType(l, sym) ;
      PrintAlignment(sym)
   ELSIF IsEnumeration(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsEnumeration (%a)', sym, n)
   ELSIF IsSet(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsSet (%a)', sym, n) ;
      IncludeType(l, sym)
   ELSIF IsUnbounded(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsUnbounded (%a)', sym, n) ;
      IncludeUnbounded(l, sym)
   ELSIF IsPartialUnbounded(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsPartialUnbounded (%a)', sym, n) ;
      IncludePartialUnbounded(l, sym)
   ELSIF IsRecordField(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsRecordField (%a)', sym, n) ;
      IF IsRecordFieldAVarientTag(sym)
      THEN
         fprintf0 (GetDumpFile(), ' variant tag')
      END ;
      IncludeType(l, sym) ;
      IncludeGetVarient(l, sym) ;
      IncludeGetParent(l, sym) ;
      PrintAlignment(sym) ;
      PrintDecl(sym)
   ELSIF IsProcType(sym)
   THEN
      PrintProcType (sym)
   ELSIF IsVar(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsVar (%a) declared in ', sym, n) ;
      PrintScope (sym) ;
      fprintf0 (GetDumpFile (), 'mode ') ;
      CASE GetMode(sym) OF

      LeftValue     : fprintf0 (GetDumpFile(), 'l ') |
      RightValue    : fprintf0 (GetDumpFile(), 'r ') |
      ImmediateValue: fprintf0 (GetDumpFile(), 'i ') |
      NoValue       : fprintf0 (GetDumpFile(), 'n ')

      END ;
      IF IsTemporary(sym)
      THEN
         fprintf0 (GetDumpFile(), 'temporary ')
      END ;
      IF IsComponent(sym)
      THEN
         fprintf0 (GetDumpFile(), 'component ')
      END ;
      IF IsVarHeap (sym)
      THEN
         fprintf0 (GetDumpFile(), 'heap ')
      END ;
      fprintf0 (GetDumpFile (), '\n') ;
      PrintInitialized (sym) ;
      IncludeType(l, sym)
   ELSIF IsConst(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsConst (%a)', sym, n) ;
      IF IsConstString(sym)
      THEN
         fprintf1 (GetDumpFile(), '  also IsConstString (%a)', n) ;
         PrintString (sym)
      ELSIF IsConstructor(sym)
      THEN
         fprintf0 (GetDumpFile(), ' constant constructor ') ;
         IncludeType(l, sym)
      ELSIF IsConstSet(sym)
      THEN
         fprintf0 (GetDumpFile(), ' constant constructor set ') ;
         IncludeType(l, sym)
      ELSE
         IncludeType(l, sym)
      END
   ELSIF IsConstructor(sym)
   THEN
      fprintf2 (GetDumpFile(), 'sym %d IsConstructor (non constant) (%a)', sym, n) ;
      IncludeType(l, sym)
   ELSIF IsConstLit(sym)
   THEN
      fprintf2 (GetDumpFile(), 'sym %d IsConstLit (%a)', sym, n)
   ELSIF IsDummy(sym)
   THEN
      fprintf2 (GetDumpFile(), 'sym %d IsDummy (%a)', sym, n)
   ELSIF IsTemporary(sym)
   THEN
      fprintf2 (GetDumpFile(), 'sym %d IsTemporary (%a)', sym, n)
   ELSIF IsVarAParam(sym)
   THEN
      fprintf2 (GetDumpFile(), 'sym %d IsVarAParam (%a)', sym, n)
   ELSIF IsSubscript(sym)
   THEN
      fprintf2 (GetDumpFile(), 'sym %d IsSubscript (%a)', sym, n)
   ELSIF IsSubrange(sym)
   THEN
      GetSubrange(sym, high, low) ;
      fprintf2 (GetDumpFile(), 'sym %d IsSubrange (%a)', sym, n) ;
      IF (low#NulSym) AND (high#NulSym)
      THEN
         type := GetSType(sym) ;
         IF type#NulSym
         THEN
            IncludeType(l, sym) ;
            n := GetSymName(type) ;
            fprintf1 (GetDumpFile(), ' %a', n)
         END ;
         n := GetSymName(low) ;
         n2 := GetSymName(high) ;
         fprintf2 (GetDumpFile (), '[%a..%a]', n, n2)
      END
   ELSIF IsProcedureVariable(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsProcedureVariable (%a)', sym, n)
   ELSIF IsProcedureNested(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsProcedureNested (%a)', sym, n)
   ELSIF IsAModula2Type(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsAModula2Type (%a)', sym, n)
   ELSIF IsObject(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsObject (%a)', sym, n)
   ELSIF IsTuple(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsTuple (%a)', sym, n) ;
      low := GetNth(sym, 1) ;
      high := GetNth(sym, 2) ;
      fprintf2 (GetDumpFile (), '%d, %d\n', low, high)
   ELSIF IsGnuAsm(sym)
   THEN
      IF IsGnuAsmVolatile(sym)
      THEN
         fprintf2 (GetDumpFile (), 'sym %d IsGnuAsmVolatile (%a)', sym, n)
      ELSE
         fprintf2 (GetDumpFile (), 'sym %d IsGnuAsm (%a)', sym, n)
      END
   ELSIF IsComponent(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsComponent (%a) ', sym, n) ;
      i := 1 ;
      REPEAT
         type := GetNth(sym, i) ;
         IF type#NulSym
         THEN
            IncludeItemIntoList(l, type) ;
            n := GetSymName(type) ;
            fprintf2 (GetDumpFile (), "[%a %d] ", n, type) ;
            INC(i)
         END ;
      UNTIL type=NulSym
   END ;

   IF IsHiddenType(sym)
   THEN
      fprintf0 (GetDumpFile(), ' IsHiddenType')
   END ;
   fprintf0 (GetDumpFile(), '\n')
END PrintVerboseFromList ;


(*
   PrintVerbose - prints limited information about a symbol.
*)

PROCEDURE PrintVerbose (sym: CARDINAL) ;
VAR
   l: List ;
   i: CARDINAL ;
BEGIN
   InitList (l) ;
   IncludeItemIntoList (l, sym) ;
   i := 1 ;
   WHILE i<=NoOfItemsInList (l) DO
      PrintVerboseFromList (l, i) ;
      INC (i)
   END ;
   KillList (l)
END PrintVerbose ;


(*
   PrintSym - prints limited information about a symbol.
              This procedure is externally visible.
*)

PROCEDURE PrintSym (sym: CARDINAL) ;
BEGIN
   printf1 ('information about symbol: %d\n', sym) ;
   fprintf0 (GetDumpFile (), '==============================\n') ;
   PrintVerbose (sym)
END PrintSym ;


(* ********************************
(*
   PrintSymbol - prints limited information about a symbol.
*)

PROCEDURE PrintSymbol (sym: CARDINAL) ;
BEGIN
   PrintTerse(sym) ;
   fprintf0 (GetDumpFile(), '\n')
END PrintSymbol ;
  ******************************************* *)

(*
   PrintTerse -
*)

PROCEDURE PrintTerse (sym: CARDINAL) ;
VAR
   n: Name ;
BEGIN
   n := GetSymName(sym) ;
   IF IsError(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsError (%a)', sym, n)
   ELSIF IsDefImp(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsDefImp (%a)', sym, n) ;
      IF IsDefinitionForC(sym)
      THEN
         fprintf0 (GetDumpFile(), 'and IsDefinitionForC')
      END ;
      IF IsHiddenTypeDeclared(sym)
      THEN
         fprintf0 (GetDumpFile(), ' IsHiddenTypeDeclared')
      END
   ELSIF IsModule(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsModule (%a)', sym, n) ;
      IF IsModuleWithinProcedure(sym)
      THEN
         fprintf0 (GetDumpFile(), ' and IsModuleWithinProcedure')
      END
   ELSIF IsInnerModule(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsInnerModule (%a)', sym, n)
   ELSIF IsUnknown(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsUnknown (%a)', sym, n)
   ELSIF IsType(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsType (%a)', sym, n)
   ELSIF IsProcedure(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsProcedure (%a)', sym, n);
      IF IsProcedureReachable(sym)
      THEN
         fprintf0 (GetDumpFile(), ' and IsProcedureReachable')
      END
   ELSIF IsParameter(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsParameter (%a)', sym, n)
   ELSIF IsPointer(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsPointer (%a)', sym, n)
   ELSIF IsRecord(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsRecord (%a)', sym, n)
   ELSIF IsVarient(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsVarient (%a)', sym, n)
   ELSIF IsFieldVarient(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsFieldVarient (%a)', sym, n)
   ELSIF IsFieldEnumeration(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsFieldEnumeration (%a)', sym, n)
   ELSIF IsArray(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsArray (%a)', sym, n)
   ELSIF IsEnumeration(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsEnumeration (%a)', sym, n)
   ELSIF IsSet(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsSet (%a)', sym, n)
   ELSIF IsUnbounded(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsUnbounded (%a)', sym, n)
   ELSIF IsRecordField(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsRecordField (%a)', sym, n)
   ELSIF IsProcType(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsProcType (%a)', sym, n)
   ELSIF IsVar(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsVar (%a)', sym, n)
   ELSIF IsConstString(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsConstString (%a)', sym, n)
   ELSIF IsConst(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsConst (%a)', sym, n)
   ELSIF IsConstLit(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsConstLit (%a)', sym, n)
   ELSIF IsDummy(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsDummy (%a)', sym, n)
   ELSIF IsTemporary(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsTemporary (%a)', sym, n)
   ELSIF IsVarAParam(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsVarAParam (%a)', sym, n)
   ELSIF IsSubscript(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsSubscript (%a)', sym, n)
   ELSIF IsSubrange(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsSubrange (%a)', sym, n)
   ELSIF IsProcedureVariable(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsProcedureVariable (%a)', sym, n)
   ELSIF IsProcedureNested(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsProcedureNested (%a)', sym, n)
   ELSIF IsAModula2Type(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsAModula2Type (%a)', sym, n)
   ELSIF IsGnuAsm(sym)
   THEN
      fprintf2 (GetDumpFile (), 'sym %d IsGnuAsm (%a)', sym, n)
   ELSIF IsImport (sym)
   THEN
      fprintf1 (GetDumpFile(), 'sym %d IsImport', sym)
   ELSIF IsImportStatement (sym)
   THEN
      fprintf1 (GetDumpFile(), 'sym %d IsImportStatement', sym)
   END ;

   IF IsHiddenType(sym)
   THEN
      fprintf0 (GetDumpFile(), ' IsHiddenType')
   END
END PrintTerse ;


(*
   CheckAlignment -
*)

PROCEDURE CheckAlignment (type: tree; sym: CARDINAL) : tree ;
VAR
   align: CARDINAL ;
BEGIN
   align := GetAlignment(sym) ;
   IF align#NulSym
   THEN
      PushInt(0) ;
      PushValue(align) ;
      IF NOT Equ(GetDeclaredMod(sym))
      THEN
         RETURN( SetAlignment(type, Mod2Gcc(GetAlignment(sym))) )
      END
   END ;
   RETURN( type )
END CheckAlignment ;


(*
   CheckPragma -
*)

PROCEDURE CheckPragma (type: tree; sym: CARDINAL) : tree ;
BEGIN
   IF IsDeclaredPacked (sym)
   THEN
      IF IsRecordField (sym) OR IsFieldVarient (sym)
      THEN
         type := SetDeclPacked (type)
      ELSIF IsRecord (sym) OR IsVarient (sym)
      THEN
         type := SetTypePacked (type)
      END
   END ;
   RETURN CheckAlignment (type, sym)
END CheckPragma ;


(*
   IsZero - returns TRUE if symbol, sym, is zero.
*)

PROCEDURE IsZero (sym: CARDINAL) : BOOLEAN ;
BEGIN
   PushIntegerTree(Mod2Gcc(sym)) ;
   PushInt(0) ;
   RETURN( Equ(GetDeclaredMod(sym)) )
END IsZero ;


(*
   SetFieldPacked - sets Varient, VarientField and RecordField symbols
                    as packed.
*)

PROCEDURE SetFieldPacked (field: CARDINAL) ;
BEGIN
   IF IsVarient(field) OR IsFieldVarient(field) OR IsRecordField(field)
   THEN
      PutDeclaredPacked(field, TRUE)
   END
END SetFieldPacked ;


(*
   RecordPacked - indicates that record, sym, and its fields
                  are all packed.
*)

PROCEDURE RecordPacked (sym: CARDINAL) ;
BEGIN
   PutDeclaredPacked(sym, TRUE) ;
   WalkRecordDependants(sym, SetFieldPacked)
END RecordPacked ;


(*
   SetFieldNotPacked - sets Varient, VarientField and RecordField symbols
                       as not packed.
*)

PROCEDURE SetFieldNotPacked (field: CARDINAL) ;
BEGIN
   IF IsVarient(field) OR IsFieldVarient(field) OR IsRecordField(field)
   THEN
      PutDeclaredPacked(field, FALSE)
   END
END SetFieldNotPacked ;


(*
   RecordNotPacked - indicates that record, sym, and its fields
                     are all not packed.
*)

PROCEDURE RecordNotPacked (sym: CARDINAL) ;
BEGIN
   PutDeclaredPacked(sym, FALSE) ;
   WalkRecordDependants(sym, SetFieldNotPacked)
END RecordNotPacked ;


(*
   DetermineIfRecordPacked -
*)

PROCEDURE DetermineIfRecordPacked (sym: CARDINAL) ;
VAR
   defaultAlignment: CARDINAL ;
BEGIN
   defaultAlignment := GetDefaultRecordFieldAlignment(sym) ;
   IF (defaultAlignment#NulSym) AND IsZero(defaultAlignment)
   THEN
      RecordPacked(sym)
   ELSE
      RecordNotPacked(sym)
   END
END DetermineIfRecordPacked ;


(*
   DeclarePackedSubrange -
*)

PROCEDURE DeclarePackedSubrange (equiv, sym: CARDINAL) ;
VAR
   type,
   gccsym   : tree ;
   high, low: CARDINAL ;
   location : location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(sym)) ;
   GetSubrange(sym, high, low) ;
   type := BuildSmallestTypeRange(location, Mod2Gcc(low), Mod2Gcc(high)) ;
   gccsym := BuildSubrangeType(location, KeyToCharStar(GetFullSymName(sym)),
                               type, Mod2Gcc(low), Mod2Gcc(high)) ;
   AddModGcc(equiv, gccsym)
END DeclarePackedSubrange ;


(*
   DeclarePackedSet -
*)

PROCEDURE DeclarePackedSet (equiv, sym: CARDINAL) ;
VAR
   highLimit,
   range,
   gccsym   : tree ;
   type,
   high, low: CARDINAL ;
   location: location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(sym)) ;
   Assert(IsSet(sym)) ;
   type := GetDType(sym) ;
   low := GetTypeMin(type) ;
   high := GetTypeMax(type) ;
   highLimit := BuildSub(location, Mod2Gcc(high), Mod2Gcc(low), FALSE) ;
   (* --fixme-- we need to check that low <= WORDLENGTH.  *)
   highLimit := BuildLSL(location, GetIntegerOne(location), highLimit, FALSE) ;
   range := BuildSmallestTypeRange(location, GetIntegerZero(location), highLimit) ;
   gccsym := BuildSubrangeType(location, KeyToCharStar(GetFullSymName(sym)),
                               range, GetIntegerZero(location), highLimit) ;
   AddModGcc(equiv, gccsym)
END DeclarePackedSet ;


(*
   DeclareFieldEnumeration - declares an enumerator within the current enumeration type.
*)

PROCEDURE DeclarePackedFieldEnumeration (sym: WORD) ;
VAR
   equiv,
   type    : CARDINAL ;
   field,
   enumlist: tree ;
BEGIN
   (* add relationship between gccSym and sym *)
   type := GetSType (sym) ;
   equiv := GetPackedEquivalent (type) ;
   enumlist := GetEnumList (equiv) ;
   PushValue (sym) ;
   field := DeclareFieldValue (sym, PopIntegerTree(), enumlist) ;
   Assert (field # NIL) ;
   PutEnumList (equiv, enumlist)
END DeclarePackedFieldEnumeration ;


(*
   DeclarePackedEnumeration -
*)

PROCEDURE DeclarePackedEnumeration (equiv, sym: CARDINAL) ;
VAR
   enumlist,
   gccenum : tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(sym)) ;
   gccenum := BuildStartEnumeration(location, KeyToCharStar(GetFullSymName(sym)), TRUE) ;
   ForeachLocalSymDo(sym, DeclarePackedFieldEnumeration) ;
   enumlist := GetEnumList(equiv) ;
   gccenum := BuildEndEnumeration(location, gccenum, enumlist) ;
   AddModGcc(equiv, gccenum)
END DeclarePackedEnumeration ;


(*
   DeclarePackedType -
*)

PROCEDURE DeclarePackedType (equiv, sym: CARDINAL) ;
VAR
   type: CARDINAL ;
BEGIN
   type := GetSType(sym) ;
   IF type=NulSym
   THEN
      IF sym=Boolean
      THEN
         AddModGcc(equiv, GetPackedBooleanType())
      ELSE
         AddModGcc(equiv, Mod2Gcc(sym))
      END
   ELSE
      DeclarePackedType(GetPackedEquivalent(type), type) ;
      AddModGcc(equiv, Mod2Gcc(GetPackedEquivalent(type)))
   END
END DeclarePackedType ;


(*
   doDeclareEquivalent -
*)

PROCEDURE doDeclareEquivalent (sym: CARDINAL; p: doDeclareProcedure) : tree ;
VAR
   equiv: CARDINAL ;
BEGIN
   equiv := GetPackedEquivalent(sym) ;
   IF NOT GccKnowsAbout(equiv)
   THEN
      p(equiv, sym) ;
      IncludeElementIntoSet(GlobalGroup^.FullyDeclared, equiv)
   END ;
   RETURN( Mod2Gcc(equiv) )
END doDeclareEquivalent ;


(*
   PossiblyPacked -
*)

PROCEDURE PossiblyPacked (sym: CARDINAL; isPacked: BOOLEAN) : tree ;
BEGIN
   IF isPacked
   THEN
      IF IsSubrange(sym)
      THEN
         RETURN( doDeclareEquivalent(sym, DeclarePackedSubrange) )
      ELSIF IsType(sym)
      THEN
         RETURN( doDeclareEquivalent(sym, DeclarePackedType) )
      ELSIF IsEnumeration(sym)
      THEN
         RETURN( doDeclareEquivalent(sym, DeclarePackedEnumeration) )
      ELSIF IsSet(sym)
      THEN
         RETURN( doDeclareEquivalent(sym, DeclarePackedSet) )
      END
   END ;
   RETURN( Mod2Gcc(sym) )
END PossiblyPacked ;


(*
   GetPackedType - returns a possibly packed type for field.
*)

PROCEDURE GetPackedType (sym: CARDINAL) : tree ;
BEGIN
   IF IsSubrange(sym)
   THEN
      RETURN( doDeclareEquivalent(sym, DeclarePackedSubrange) )
   ELSIF IsType(sym)
   THEN
      RETURN( doDeclareEquivalent(sym, DeclarePackedType) )
   ELSIF IsEnumeration(sym)
   THEN
      RETURN( doDeclareEquivalent(sym, DeclarePackedEnumeration) )
   END ;
   RETURN( Mod2Gcc(sym) )
END GetPackedType ;


(*
   MaybeAlignField - checks to see whether, field, is packed or aligned and it updates
                     the offsets if appropriate.
*)

PROCEDURE MaybeAlignField (field: CARDINAL; VAR byteOffset, bitOffset: tree) : tree ;
VAR
   f, ftype,
   nbits   : tree ;
   location: location_t ;
BEGIN
   f := Mod2Gcc(field) ;
   IF IsDeclaredPacked(field)
   THEN
      location := TokenToLocation(GetDeclaredMod(field)) ;
      f := SetDeclPacked(f) ;
      ftype := GetPackedType(GetSType(field)) ;
      nbits := BuildTBitSize(location, ftype) ;
      f := SetRecordFieldOffset(f, byteOffset, bitOffset, ftype, nbits) ;
      bitOffset := BuildAdd(location, bitOffset, nbits, FALSE) ;
      RETURN( f )
   ELSE
      RETURN( CheckAlignment(f, field) )
   END
END MaybeAlignField ;


(*
   DeclareRecord - declares a record and its fields to gcc.
                   The final gcc record type is returned.
*)

PROCEDURE DeclareRecord (Sym: CARDINAL) : tree ;
VAR
   Field     : CARDINAL ;
   i         : CARDINAL ;
   nbits,
   ftype,
   field,
   byteOffset,
   bitOffset,
   FieldList,
   RecordType: tree ;
   location  : location_t ;
BEGIN
   i := 1 ;
   FieldList := tree(NIL) ;
   RecordType := DoStartDeclaration(Sym, BuildStartRecord) ;
   location := TokenToLocation(GetDeclaredMod(Sym)) ;
   byteOffset := GetIntegerZero(location) ;
   bitOffset := GetIntegerZero(location) ;
   REPEAT
      Field := GetNth(Sym, i) ;
      IF Field#NulSym
      THEN
         IF IsRecordField(Field) AND IsRecordFieldAVarientTag(Field) AND (GetSymName(Field)=NulName)
         THEN
            (* do not include a nameless tag into the C struct *)
         ELSIF IsVarient(Field)
         THEN
            Field := Chained(Field) ;
            field := Mod2Gcc(Field) ;
            IF IsDeclaredPacked(Field)
            THEN
               location := TokenToLocation(GetDeclaredMod(Field)) ;
               field := SetDeclPacked(field) ;
               ftype := GetPackedType(GetSType(Field)) ;
               nbits := BuildTBitSize(location, ftype) ;
               field := SetRecordFieldOffset(field, byteOffset, bitOffset, ftype, nbits) ;
               bitOffset := BuildAdd(location, bitOffset, nbits, FALSE) ;
               byteOffset := BuildAdd(location, byteOffset,
                                      BuildDivTrunc(location, bitOffset, BuildIntegerConstant(8), FALSE),
                                      FALSE) ;
               bitOffset := BuildModTrunc(location, bitOffset, BuildIntegerConstant(8), FALSE)
            END ;
            FieldList := ChainOn(FieldList, field)
         ELSE
            IF Debugging
            THEN
               printf0('chaining ') ; PrintTerse(Field) ; printf0('\n')
            END ;
            FieldList := ChainOn(FieldList, MaybeAlignField(Chained(Field), byteOffset, bitOffset))
         END
      END ;
      INC(i)
   UNTIL Field=NulSym ;
   WatchRemoveList(Sym, partiallydeclared) ;
   WatchRemoveList(Sym, heldbyalignment) ;
   WatchRemoveList(Sym, finishedalignment) ;
   location := TokenToLocation(GetDeclaredMod(Sym)) ;
   RETURN( BuildEndRecord(location, RecordType, FieldList, IsDeclaredPacked(Sym)) )
END DeclareRecord ;


(*
   DeclareRecordField -
*)

PROCEDURE DeclareRecordField (sym: CARDINAL) : tree ;
VAR
   field,
   GccFieldType: tree ;
   location    : location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(sym)) ;
   GccFieldType := PossiblyPacked(GetSType(sym), IsDeclaredPacked(sym)) ;
   field := BuildFieldRecord(location, KeyToCharStar(GetFullSymName(sym)), GccFieldType) ;
   RETURN( field )
END DeclareRecordField ;


(*
   DeclareVarient - declares a record and its fields to gcc.
                    The final gcc record type is returned.
*)

PROCEDURE DeclareVarient (sym: CARDINAL) : tree ;
VAR
   Field       : CARDINAL ;
   i           : CARDINAL ;
   byteOffset,
   bitOffset,
   FieldList,
   VarientType : tree ;
   location    : location_t ;
BEGIN
   i := 1 ;
   FieldList := tree(NIL) ;
   VarientType := DoStartDeclaration(sym, BuildStartVarient) ;
   location := TokenToLocation(GetDeclaredMod(sym)) ;
   byteOffset := GetIntegerZero(location) ;
   bitOffset := GetIntegerZero(location) ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      IF IsRecordField(Field) AND IsRecordFieldAVarientTag(Field) AND (GetSymName(Field)=NulName)
      THEN
         (* do not include a nameless tag into the C struct *)
      ELSE
         IF Debugging
         THEN
            printf0('chaining ') ; PrintTerse(Field) ; printf0('\n')
         END ;
         FieldList := ChainOn(FieldList, MaybeAlignField(Chained(Field), byteOffset, bitOffset))
      END ;
      INC(i)
   END ;
   WatchRemoveList(sym, partiallydeclared) ;
   WatchRemoveList(sym, heldbyalignment) ;
   WatchRemoveList(sym, finishedalignment) ;
   VarientType := BuildEndVarient(location, VarientType, FieldList, IsDeclaredPacked(sym)) ;
   RETURN( VarientType )
END DeclareVarient ;


(*
   DeclareFieldVarient -
*)

PROCEDURE DeclareFieldVarient (sym: CARDINAL) : tree ;
VAR
   i, f        : CARDINAL ;
   VarientList,
   VarientType,
   byteOffset,
   bitOffset,
   GccFieldType: tree ;
   location    : location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(sym)) ;
   i := 1 ;
   VarientList := tree(NIL) ;
   VarientType := DoStartDeclaration(sym, BuildStartFieldVarient) ;
   (* no need to store the [sym, RecordType] tuple as it is stored by DeclareRecord which calls us *)
   byteOffset := GetIntegerZero(location) ;
   bitOffset := GetIntegerZero(location) ;
   WHILE GetNth(sym, i)#NulSym DO
      f := GetNth(sym, i) ;
      IF IsFieldVarient(f) AND IsEmptyFieldVarient(f)
      THEN
         (* do not include empty varient fields (created via 'else end' in variant records *)
      ELSE
         IF Debugging
         THEN
            printf0('chaining ') ; PrintTerse(f) ; printf0('\n')
         END ;
         VarientList := ChainOn(VarientList, MaybeAlignField(Chained(f), byteOffset, bitOffset))
      END ;
      INC(i)
   END ;
   WatchRemoveList(sym, partiallydeclared) ;
   GccFieldType := BuildEndFieldVarient(location, VarientType, VarientList, IsDeclaredPacked(sym)) ;
   RETURN( GccFieldType )
END DeclareFieldVarient ;


(*
   DeclarePointer - declares a pointer type to gcc and returns the Tree.
*)

PROCEDURE DeclarePointer (sym: CARDINAL) : tree ;
BEGIN
   RETURN( BuildPointerType(Mod2Gcc(GetSType(sym))) )
END DeclarePointer ;


(*
   DeclareUnbounded - builds an unbounded type and returns the gcc tree.
*)

PROCEDURE DeclareUnbounded (sym: CARDINAL) : tree ;
VAR
   record: CARDINAL ;
BEGIN
   Assert(IsUnbounded(sym)) ;
   IF GccKnowsAbout(sym)
   THEN
      RETURN( Mod2Gcc(sym) )
   ELSE
      record := GetUnboundedRecordType(sym) ;
      Assert(IsRecord(record)) ;
      Assert(AllDependantsFullyDeclared(record)) ;
      IF (NOT GccKnowsAbout(record))
      THEN
         DeclareTypeConstFully(record) ;
         WatchRemoveList(record, todolist)
      END ;
      RETURN( Mod2Gcc(record) )
   END
END DeclareUnbounded ;


(*
   BuildIndex -
*)

PROCEDURE BuildIndex (tokenno: CARDINAL; array: CARDINAL) : tree ;
VAR
   Subscript: CARDINAL ;
   Type,
   High, Low: CARDINAL ;
   n,
   low, high: tree ;
   location : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   Subscript := GetArraySubscript (array) ;
   Assert (IsSubscript (Subscript)) ;
   Type := GetDType (Subscript) ;
   Low := GetTypeMin (Type) ;
   High := GetTypeMax (Type) ;
   DeclareConstant (tokenno, Low) ;
   DeclareConstant (tokenno, High) ;
   low := Mod2Gcc (Low) ;
   high := Mod2Gcc (High) ;
   IF ExceedsTypeRange (GetIntegerType (), low, high)
   THEN
      location := TokenToLocation (tokenno) ;
      n := BuildConvert (location, GetIntegerType (), BuildSub (location, high, low, FALSE), FALSE) ;
      IF TreeOverflow(n) OR ValueOutOfTypeRange (GetIntegerType (), n)
      THEN
         MetaError3('implementation restriction, array is too large {%1EDM}, the range {%2ad}..{%3ad} exceeds the integer range',
                    array, Low, High) ;
         RETURN BuildArrayIndexType (GetIntegerZero (location), GetIntegerZero (location))
      ELSE
         PutArrayLarge (array) ;
	 RETURN BuildArrayIndexType (GetIntegerZero (location), n)
      END
   ELSE
      low := BuildConvert (location, GetIntegerType (), low, FALSE) ;
      high := BuildConvert (location, GetIntegerType (), high, FALSE) ;
      RETURN BuildArrayIndexType (low, high)
   END
END BuildIndex ;


(*
   DeclareArray - declares an array to gcc and returns the gcc tree.
*)

PROCEDURE DeclareArray (Sym: CARDINAL) : tree ;
VAR
   typeOfArray: CARDINAL ;
   ArrayType,
   GccArray,
   GccIndex   : tree ;
   Subscript  : CARDINAL ;
   tokenno    : CARDINAL ;
   location   : location_t ;
BEGIN
   Assert(IsArray(Sym)) ;

   tokenno := GetDeclaredMod(Sym) ;
   location := TokenToLocation(tokenno) ;

   Subscript := GetArraySubscript(Sym) ;
   typeOfArray := GetDType(Sym) ;
   GccArray := Mod2Gcc(typeOfArray) ;
   GccIndex := BuildIndex(tokenno, Sym) ;

   IF GccKnowsAbout(Sym)
   THEN
      ArrayType := Mod2Gcc(Sym)
   ELSE
      ArrayType := BuildStartArrayType(GccIndex, GccArray, typeOfArray) ;
      PreAddModGcc(Sym, ArrayType)
   END ;

   PreAddModGcc(Subscript, GccArray) ;       (* we save the type of this array as the subscript *)
   PushIntegerTree(BuildSize(location, GccArray, FALSE)) ;  (* and the size of this array so far *)
   PopSize(Subscript) ;

   GccArray := BuildEndArrayType(ArrayType, GccArray, GccIndex, typeOfArray) ;
   Assert(GccArray=ArrayType) ;

   RETURN( GccArray )
END DeclareArray ;


(*
   DeclareProcType - declares a procedure type to gcc and returns the gcc type tree.
*)

PROCEDURE DeclareProcType (Sym: CARDINAL) : tree ;
VAR
   i, p,
   Parameter,
   ReturnType: CARDINAL ;
   func,
   GccParam  : tree ;
   location  : location_t ;
BEGIN
   ReturnType := GetSType(Sym) ;
   func := DoStartDeclaration(Sym, BuildStartFunctionType) ;
   InitFunctionTypeParameters ;
   p := NoOfParamAny (Sym) ;
   i := p ;
   WHILE i > 0 DO
      Parameter := GetNthParamAny (Sym, i) ;
      location := TokenToLocation (GetDeclaredMod (Parameter)) ;
      GccParam := BuildProcTypeParameterDeclaration (location, Mod2Gcc (GetSType (Parameter)), IsVarParamAny (Sym, i)) ;
      PreAddModGcc(Parameter, GccParam) ;
      DEC(i)
   END ;
   IF ReturnType = NulSym
   THEN
      RETURN( BuildEndFunctionType (func, NIL, UsesVarArgs(Sym)) )
   ELSE
      RETURN( BuildEndFunctionType (func, Mod2Gcc(ReturnType), UsesVarArgs(Sym)) )
   END
END DeclareProcType ;


VAR
   MaxEnumerationField,
   MinEnumerationField: CARDINAL ;


(*
   FindMinMaxEnum - finds the minimum and maximum enumeration fields.
*)

PROCEDURE FindMinMaxEnum (field: WORD) ;
BEGIN
   IF MaxEnumerationField=NulSym
   THEN
      MaxEnumerationField := field
   ELSE
      PushValue(field) ;
      PushValue(MaxEnumerationField) ;
      IF Gre(GetDeclaredMod(field))
      THEN
         MaxEnumerationField := field
      END
   END ;
   IF MinEnumerationField=NulSym
   THEN
      MinEnumerationField := field
   ELSE
      PushValue(field) ;
      PushValue(MinEnumerationField) ;
      IF Less(GetDeclaredMod(field))
      THEN
         MinEnumerationField := field
      END
   END
END FindMinMaxEnum ;


(*
   GetTypeMin -
*)

PROCEDURE GetTypeMin (type: CARDINAL) : CARDINAL ;
VAR
   min, max: CARDINAL ;
BEGIN
   IF IsSubrange(type)
   THEN
      GetSubrange(type, max, min) ;
      RETURN( min )
   ELSIF IsSet(type)
   THEN
      RETURN( GetTypeMin(GetSType(type)) )
   ELSIF IsEnumeration(type)
   THEN
      MinEnumerationField := NulSym ;
      MaxEnumerationField := NulSym ;
      ForeachLocalSymDo (type, FindMinMaxEnum) ;
      RETURN( MinEnumerationField )
   ELSIF IsBaseType(type)
   THEN
      GetBaseTypeMinMax(type, min, max) ;
      RETURN( min )
   ELSIF IsSystemType(type)
   THEN
      GetSystemTypeMinMax(type, min, max) ;
      RETURN( min )
   ELSIF GetSType(type)=NulSym
   THEN
      MetaError1('unable to obtain the MIN value for type {%1as}', type) ;
      RETURN NulSym
   ELSE
      RETURN( GetTypeMin(GetSType(type)) )
   END
END GetTypeMin ;


(*
   GetTypeMax -
*)

PROCEDURE GetTypeMax (type: CARDINAL) : CARDINAL ;
VAR
   min, max: CARDINAL ;
BEGIN
   IF IsSubrange(type)
   THEN
      GetSubrange(type, max, min) ;
      RETURN( max )
   ELSIF IsSet(type)
   THEN
      RETURN( GetTypeMax(GetSType(type)) )
   ELSIF IsEnumeration(type)
   THEN
      MinEnumerationField := NulSym ;
      MaxEnumerationField := NulSym ;
      ForeachLocalSymDo (type, FindMinMaxEnum) ;
      RETURN( MaxEnumerationField )
   ELSIF IsBaseType(type)
   THEN
      GetBaseTypeMinMax(type, min, max) ;
      RETURN( max )
   ELSIF IsSystemType(type)
   THEN
      GetSystemTypeMinMax(type, min, max) ;
      RETURN( max )
   ELSIF GetSType(type)=NulSym
   THEN
      MetaError1('unable to obtain the MAX value for type {%1as}', type) ;
      RETURN NulSym
   ELSE
      RETURN( GetTypeMax(GetSType(type)) )
   END
END GetTypeMax ;


(*
   PushNoOfBits - pushes the integer value of the number of bits required
                  to maintain a set of type.
*)

PROCEDURE PushNoOfBits (type: CARDINAL; low, high: CARDINAL) ;
BEGIN
   PushValue(high) ;
   ConvertToType(type) ;
   PushValue(low) ;
   ConvertToType(type) ;
   Sub ;
   ConvertToType(Cardinal)
END PushNoOfBits ;


(*
   DeclareLargeSet - n is the name of the set.
                     type is the subrange type (or simple type)
                     low and high are the limits of the subrange.
*)

PROCEDURE DeclareLargeSet (n: Name; type: CARDINAL; low, high: CARDINAL) : tree ;
VAR
   lowtree,
   hightree,
   BitsInSet,
   RecordType,
   GccField,
   FieldList : tree ;
   bpw       : CARDINAL ;
   location  : location_t ;
BEGIN
   location   := TokenToLocation(GetDeclaredMod(type)) ;
   bpw        := GetBitsPerBitset() ;
   PushValue(low) ;
   lowtree    := PopIntegerTree() ;
   PushValue(high) ;
   hightree   := PopIntegerTree() ;
   FieldList  := tree(NIL) ;
   RecordType := BuildStartRecord(location, KeyToCharStar(n)) ;  (* no problem with recursive types here *)
   PushNoOfBits(type, low, high) ;
   PushCard(1) ;
   Addn ;
   BitsInSet := PopIntegerTree() ;
   PushIntegerTree(BitsInSet) ;
   PushCard(0) ;
   WHILE Gre(GetDeclaredMod(type)) DO
      PushIntegerTree(BitsInSet) ;
      PushCard(bpw-1) ;
      IF GreEqu(GetDeclaredMod(type))
      THEN
         PushIntegerTree(lowtree) ;
         PushCard(bpw-1) ;
         Addn ;
         GccField := BuildFieldRecord(location, NIL, BuildSetType(location, NIL, Mod2Gcc(type), lowtree, PopIntegerTree(), FALSE)) ;
         PushIntegerTree(lowtree) ;
         PushCard(bpw) ;
         Addn ;
         lowtree := PopIntegerTree() ;
         PushIntegerTree(BitsInSet) ;
         PushCard(bpw) ;
         Sub ;
         BitsInSet := PopIntegerTree()
      ELSE
         (* printf2('range is %a..%a\n', GetSymName(low), GetSymName(high)) ; *)
         GccField := BuildFieldRecord(location, NIL, BuildSetType(location, NIL, Mod2Gcc(type), lowtree, hightree, FALSE)) ;
         PushCard(0) ;
         BitsInSet := PopIntegerTree()
      END ;
      FieldList := ChainOn(FieldList, GccField) ;
      PushIntegerTree(BitsInSet) ;
      PushCard(0)
   END ;
   RETURN( BuildEndRecord(location, RecordType, FieldList, FALSE) )
END DeclareLargeSet ;


(*
   DeclareLargeOrSmallSet - works out whether the set will exceed TSIZE(WORD). If it does
                            we manufacture a set using:

                            settype = RECORD
                                         w1: SET OF [...]
                                         w2: SET OF [...]
                                      END

                            We do this as GCC and GDB (stabs) only knows about WORD sized sets.
                            If the set will fit into a WORD then we call gccgm2 directly.
*)

PROCEDURE DeclareLargeOrSmallSet (sym: CARDINAL;
                                  n: Name; type: CARDINAL; low, high: CARDINAL) : tree ;
VAR
   location: location_t ;
   packed  : BOOLEAN ;
BEGIN
   PushNoOfBits(type, low, high) ;
   PushCard(GetBitsPerBitset()) ;
   packed := IsSetPacked (sym) ;
   IF Less(GetDeclaredMod(type))
   THEN
      location := TokenToLocation(GetDeclaredMod(sym)) ;
      (* small set *)
      (* PutSetSmall(sym) ; *)
      RETURN BuildSetType (location, KeyToCharStar(n),
                           Mod2Gcc(type), Mod2Gcc(low), Mod2Gcc(high), packed)
   ELSE
      (* PutSetLarge(sym) ; *)
      RETURN DeclareLargeSet (n, type, low, high)   (* --fixme-- finish packed here as well.  *)
   END
END DeclareLargeOrSmallSet ;


(*
   DeclareSet - declares a set type to gcc and returns a Tree.
*)

PROCEDURE DeclareSet (sym: CARDINAL) : tree ;
VAR
   gccsym   : tree ;
   type,
   high, low: CARDINAL ;
BEGIN
   type := GetDType(sym) ;
   IF IsSubrange(type)
   THEN
      GetSubrange(type, high, low) ;
      gccsym := DeclareLargeOrSmallSet(sym, GetFullSymName(sym), GetSType(type), low, high)
   ELSE
      gccsym := DeclareLargeOrSmallSet(sym, GetFullSymName(sym), type, GetTypeMin(type), GetTypeMax(type))
   END ;
   RETURN( gccsym )
END DeclareSet ;


(*
   CheckResolveSubrange - checks to see whether we can determine
                          the subrange type.  We are able to do
                          this once low, high and the type are known.
*)

PROCEDURE CheckResolveSubrange (sym: CARDINAL) ;
VAR
   tokenno               : CARDINAL;
   size, high, low, type: CARDINAL ;
BEGIN
   GetSubrange(sym, high, low) ;
   tokenno := GetDeclaredMod (sym) ;
   type := GetSType(sym) ;
   IF type=NulSym
   THEN
      IF GccKnowsAbout(low) AND GccKnowsAbout(high)
      THEN
         IF IsConstString (low) AND IsConstStringKnown (low)
         THEN
            size := GetStringLength (tokenno, low) ;
            IF size <= 1
            THEN
               PutSubrange(sym, low, high, Char)
            ELSE
               MetaError1 ('cannot have a subrange of a string type {%1Uad}',
                           sym)
            END
         ELSIF IsFieldEnumeration(low)
         THEN
            IF GetSType(low)=GetSType(high)
            THEN
               PutSubrange(sym, low, high, GetSType(low))
            ELSE
               MetaError1('subrange limits must be of the same type {%1Uad}', sym)
            END
         ELSIF IsValueSolved(low)
         THEN
            IF GetSType(low)=LongReal
            THEN
               MetaError1('cannot have a subrange of a SHORTREAL, REAL or LONGREAL type {%1Uad}', sym)
            ELSE
               PutSubrange(sym, low, high, MixTypes(GetSType(low), GetSType(high), GetDeclaredMod(sym)))
            END
         END
      END
   END
END CheckResolveSubrange ;


(*
   TypeConstFullyDeclared - all, sym, dependents are declared, so create and
                            return the GCC Tree equivalent.
*)

PROCEDURE TypeConstFullyDeclared (sym: CARDINAL) : tree ;
VAR
   t: tree ;
BEGIN
   IF IsEnumeration(sym)
   THEN
      t := DeclareEnumeration(sym)
   ELSIF IsFieldEnumeration(sym)
   THEN
      t := DeclareFieldEnumeration(sym)
   ELSIF IsSubrange(sym)
   THEN
      t := DeclareSubrange(sym)
   ELSIF IsRecord(sym)
   THEN
      t := CheckPragma(DeclareRecord(sym), sym)
   ELSIF IsRecordField(sym)
   THEN
      t := CheckPragma(DeclareRecordField(sym), sym)
   ELSIF IsFieldVarient(sym)
   THEN
      t := DeclareFieldVarient(sym)
   ELSIF IsVarient(sym)
   THEN
      t := DeclareVarient(sym)
   ELSIF IsPointer(sym)
   THEN
      t := CheckAlignment(DeclarePointer(sym), sym)
   ELSIF IsUnbounded(sym)
   THEN
      t := DeclareUnbounded(sym)
   ELSIF IsArray(sym)
   THEN
      t := CheckAlignment(DeclareArray(sym), sym)
   ELSIF IsProcType(sym)
   THEN
      t := DeclareProcType(sym)
   ELSIF IsSet(sym)
   THEN
      t := DeclareSet(sym)
   ELSIF IsConst(sym)
   THEN
      IF IsConstructor(sym)
      THEN
         PushValue(sym) ;
         ChangeToConstructor(GetDeclaredMod(sym), GetSType(sym)) ;
         PopValue(sym) ;
         EvaluateValue(sym) ;
         PutConstructorSolved(sym) ;
      ELSIF IsConstSet(sym)
      THEN
         EvaluateValue(sym)
      END ;
      IF NOT IsValueSolved(sym)
      THEN
         RETURN( NIL )
      END ;
      t := DeclareConst(GetDeclaredMod(sym), sym) ;
      Assert(t#NIL)
   ELSIF IsConstructor(sym)
   THEN
      (* not yet known as a constant *)
      RETURN( NIL )
   ELSE
      t := DeclareType(sym) ;
      IF IsType(sym)
      THEN
         t := CheckAlignment(t, sym)
      END
   END ;
   RETURN RememberType (t)
END TypeConstFullyDeclared ;


(*
   IsBaseType - returns true if a type, Sym, is a base type and
                we use predefined GDB information to represent this
                type.
*)

PROCEDURE IsBaseType (Sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (Sym=Cardinal) OR (Sym=Integer) OR
           (Sym=Char) OR (Sym=Proc) )
END IsBaseType ;


(*
   IsFieldEnumerationDependants - sets enumDeps to FALSE if action(Sym)
                                  is also FALSE.
*)

PROCEDURE IsFieldEnumerationDependants (Sym: WORD) ;
BEGIN
   IF NOT action(Sym)
   THEN
      enumDeps := FALSE
   END
END IsFieldEnumerationDependants ;


(*
   IsEnumerationDependants - returns true if the enumeration
                             p(dependants) all return true.
*)

PROCEDURE IsEnumerationDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   action := q ;
   enumDeps := TRUE ;
   ForeachLocalSymDo (sym, IsFieldEnumerationDependants) ;
   RETURN( enumDeps )
END IsEnumerationDependants ;


(*
   WalkEnumerationDependants - returns walks all dependants of Sym.
*)

PROCEDURE WalkEnumerationDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   ForeachLocalSymDo (sym, p)
END WalkEnumerationDependants ;


(*
   WalkSubrangeDependants - calls p(dependants) for each dependant of, sym.
*)

PROCEDURE WalkSubrangeDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   type, align,
   high, low  : CARDINAL ;
BEGIN
   GetSubrange(sym, high, low) ;
   CheckResolveSubrange (sym) ;
   type := GetSType(sym) ;
   IF type#NulSym
   THEN
      p(type)
   END ;
   (* low and high are not types but constants and they are resolved by M2GenGCC *)
   p(low) ;
   p(high) ;
   align := GetAlignment (sym) ;
   IF align # NulSym
   THEN
      p(align)
   END
END WalkSubrangeDependants ;


(*
   IsSubrangeDependants - returns TRUE if the subrange
                          q(dependants) all return TRUE.
*)

PROCEDURE IsSubrangeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result   : BOOLEAN ;
   align,
   type,
   high, low: CARDINAL ;
BEGIN
   GetSubrange(sym, high, low) ;
   (* low and high are not types but constants and they are resolved by M2GenGCC *)
   CheckResolveSubrange(sym) ;
   result := TRUE ;
   type := GetSType(sym) ;
   IF (type=NulSym) OR (NOT q(type))
   THEN
      result := FALSE
   END ;
   IF NOT q(low)
   THEN
      result := FALSE
   END ;
   IF NOT q(high)
   THEN
      result := FALSE
   END ;
   align := GetAlignment(sym) ;
   IF (align#NulSym) AND (NOT q(align))
   THEN
      result := FALSE
   END ;
   RETURN( result )
END IsSubrangeDependants ;


(*
   WalkComponentDependants -
*)

PROCEDURE WalkComponentDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i   : CARDINAL ;
   type: CARDINAL ;
BEGIN
   (* need to walk record and field *)
   i := 1 ;
   REPEAT
      type := GetNth(sym, i) ;
      IF type#NulSym
      THEN
         IF IsVar(type)
         THEN
            p(GetSType(type))
         ELSE
            p(type)
         END ;
         INC(i)
      END
   UNTIL type=NulSym
END WalkComponentDependants ;


(*
   IsComponentDependants -
*)

PROCEDURE IsComponentDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   i     : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   (* need to check record is completely resolved *)
   result := TRUE ;
   i := 1 ;
   REPEAT
      type := GetNth(sym, i) ;
      IF type#NulSym
      THEN
         IF IsVar(type)
         THEN
            type := GetSType(type)
         END ;
         IF NOT q(type)
         THEN
            result := FALSE
         END ;
         INC(i)
      END
   UNTIL type=NulSym ;
   RETURN( result )
END IsComponentDependants ;


(*
   WalkVarDependants - walks all dependants of sym.
*)

PROCEDURE WalkVarDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   type: CARDINAL ;
BEGIN
   p(GetSType(sym)) ;
   IF IsComponent(sym)
   THEN
      WalkComponentDependants(sym, p)
   END ;
   type := GetVarBackEndType(sym) ;
   IF type#NulSym
   THEN
      p(type)
   END
END WalkVarDependants ;


(*
   IsVarDependants - returns TRUE if the pointer symbol, sym,
                     p(dependants) all return TRUE.
*)

PROCEDURE IsVarDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   type  : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   result := TRUE ;
   IF NOT q(GetSType(sym))
   THEN
      result := FALSE
   END ;
   IF IsComponent(sym)
   THEN
      IF NOT IsComponentDependants(sym, q)
      THEN
         result := FALSE
      END
   END ;
   type := GetVarBackEndType(sym) ;
   IF type#NulSym
   THEN
      IF NOT q(type)
      THEN
         result := FALSE
      END
   END ;
   RETURN( result )
END IsVarDependants ;


(*
   WalkPointerDependants - walks all dependants of sym.
*)

PROCEDURE WalkPointerDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   align: CARDINAL ;
BEGIN
   p(GetSType(sym)) ;
   align := GetAlignment(sym) ;
   IF align#NulSym
   THEN
      p(align)
   END
END WalkPointerDependants ;


(*
   IsPointerDependants - returns TRUE if the pointer symbol, sym,
      	       	         p(dependants) all return TRUE.
*)

PROCEDURE IsPointerDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   align: CARDINAL ;
   final: BOOLEAN ;
BEGIN
   final := TRUE ;
   IF NOT q(GetSType(sym))
   THEN
      final := FALSE
   END ;
   align := GetAlignment (sym) ;
   IF final AND (align # NulSym)
   THEN
      IF NOT q(align)
      THEN
         final := FALSE
      END
   END ;
   RETURN final
END IsPointerDependants ;


(*
   IsRecordAlignment -
*)

PROCEDURE IsRecordAlignment (sym: CARDINAL; q: IsAction) : BOOLEAN ;
BEGIN
   IF GetDefaultRecordFieldAlignment(sym)#NulSym
   THEN
      IF NOT q(GetDefaultRecordFieldAlignment(sym))
      THEN
         RETURN( FALSE )
      END
   END ;
   RETURN( TRUE )
END IsRecordAlignment ;


(*
   IsRecordDependants - returns TRUE if the symbol, sym,
                        q(dependants) all return TRUE.
*)

PROCEDURE IsRecordDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
   i     : CARDINAL ;
   field : CARDINAL ;
BEGIN
   result := IsRecordAlignment(sym, q) ;
   i := 1 ;
   REPEAT
      field := GetNth(sym, i) ;
      IF field#NulSym
      THEN
         IF IsRecordField(field)
         THEN
            IF (NOT IsRecordFieldAVarientTag(field)) OR (GetSymName(field)#NulName)
            THEN
               IF NOT q(field)
               THEN
                  result := FALSE
               END
            END
         ELSIF IsVarient(field)
         THEN
            IF NOT q(field)
            THEN
               result := FALSE
      	    END
      	 ELSIF IsFieldVarient(field)
      	 THEN
            InternalError ('should not see a field varient')
         ELSE
            InternalError ('unknown symbol in record')
      	 END
      END ;
      INC(i)
   UNTIL field=NulSym ;
   RETURN( result )
END IsRecordDependants ;


(*
   WalkRecordAlignment - walks the alignment constant associated with
                         record, sym.
*)

PROCEDURE WalkRecordAlignment (sym: CARDINAL; p: WalkAction) ;
BEGIN
   IF GetDefaultRecordFieldAlignment(sym)#NulSym
   THEN
      p(GetDefaultRecordFieldAlignment(sym))
   END
END WalkRecordAlignment ;


(*
   WalkRecordDependants - walks symbol, sym, dependants.  It only
                          walks the fields if the alignment is
                          unused or fully declared.
*)

PROCEDURE WalkRecordDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   WalkRecordAlignment(sym, p) ;
   WalkRecordDependants2(sym, p)
END WalkRecordDependants ;


(*
   WalkRecordFieldDependants -
*)

PROCEDURE WalkRecordFieldDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   v    : CARDINAL ;
   align: CARDINAL ;
BEGIN
   Assert(IsRecordField(sym)) ;
   p(GetSType(sym)) ;
   v := GetVarient(sym) ;
   IF v#NulSym
   THEN
      p(v)
   END ;
   align := GetAlignment(sym) ;
   IF align#NulSym
   THEN
      p(align)
   END
END WalkRecordFieldDependants ;


(*
   WalkRecordDependants2 - walks the fields of record, sym, calling
                           p on every dependant.
*)

PROCEDURE WalkRecordDependants2 (sym: CARDINAL; p: WalkAction) ;
VAR
   i    : CARDINAL ;
   Field: CARDINAL ;
BEGIN
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      p(Field) ;
      IF IsRecordField(Field)
      THEN
         WalkRecordFieldDependants(Field, p)
      ELSIF IsVarient(Field)
      THEN
         WalkVarientDependants(Field, p)
      ELSIF IsFieldVarient(Field)
      THEN
         InternalError ('should not see a field varient')
      ELSE
         InternalError ('unknown symbol in record')
      END ;
      INC(i)
   END
END WalkRecordDependants2 ;


(*
   IsVarientAlignment -
*)

PROCEDURE IsVarientAlignment (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   align: CARDINAL ;
BEGIN
   sym := GetRecordOfVarient(sym) ;
   align := GetDefaultRecordFieldAlignment(sym) ;
   IF (align#NulSym) AND (NOT q(align))
   THEN
      RETURN( FALSE )
   END ;
   RETURN( TRUE )
END IsVarientAlignment ;


(*
   IsVarientDependants - returns TRUE if the symbol, sym,
                         q(dependants) all return TRUE.
*)

PROCEDURE IsVarientDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
   i     : CARDINAL ;
   Field : CARDINAL ;
BEGIN
   result := IsVarientAlignment(sym, q) ;
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      Assert(IsFieldVarient(Field)) ;
      IF NOT q(Field)
      THEN
         result := FALSE
      END ;
      INC(i)
   END ;
   RETURN( result )
END IsVarientDependants ;


(*
   WalkVarientAlignment -
*)

PROCEDURE WalkVarientAlignment (sym: CARDINAL; p: WalkAction) ;
VAR
   align: CARDINAL ;
BEGIN
   sym := GetRecordOfVarient(sym) ;
   align := GetDefaultRecordFieldAlignment(sym) ;
   IF align#NulSym
   THEN
      p(align)
   END
END WalkVarientAlignment ;


(*
   WalkVarientDependants - walks symbol, sym, dependants.
*)

PROCEDURE WalkVarientDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i    : CARDINAL ;
   v,
   Field: CARDINAL ;
BEGIN
   WalkVarientAlignment(sym, p) ;
   IF GetSType(sym)#NulSym
   THEN
      p(GetSType(sym))
   END ;
   v := GetVarient(sym) ;
   IF v#NulSym
   THEN
      p(v)
   END ;
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      Assert(IsFieldVarient(Field)) ;  (* field varients do _not_ have a type *)
      p(Field) ;
      WalkVarientFieldDependants(Field, p) ;
      INC(i)
   END
END WalkVarientDependants ;


(*
   IsVarientFieldDependants - returns TRUE if the symbol, sym,
                              q(dependants) all return TRUE.
*)

PROCEDURE IsVarientFieldDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   i     : CARDINAL ;
   type,
   Field : CARDINAL ;
   result: BOOLEAN ;
BEGIN
   i := 1 ;
   result := IsVarientAlignment(sym, q) ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      IF NOT q(Field)
      THEN
         result := FALSE
      END ;
      type := GetSType(Field) ;
      IF type#NulSym
      THEN
         IF NOT q(type)
         THEN
            result := FALSE
         END
      END ;
      INC(i)
   END ;
   RETURN( result )
END IsVarientFieldDependants ;


(*
   WalkVarientFieldDependants -
*)

PROCEDURE WalkVarientFieldDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i    : CARDINAL ;
   type,
   Field: CARDINAL ;
BEGIN
   WalkVarientAlignment(sym, p) ;
   i := 1 ;
   WHILE GetNth(sym, i)#NulSym DO
      Field := GetNth(sym, i) ;
      p(Field) ;
      type := GetSType(Field) ;
      IF type#NulSym
      THEN
         p(type)
      END ;
      INC(i)
   END
END WalkVarientFieldDependants ;


(*
   IsArrayDependants - returns TRUE if the symbol, sym,
      	       	       q(dependants) all return TRUE.

*)

PROCEDURE IsArrayDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result   : BOOLEAN ;
   align    : CARDINAL ;
   subscript: CARDINAL ;
   high, low: CARDINAL ;
   type     : CARDINAL ;
BEGIN
   result := TRUE ;
   Assert(IsArray(sym)) ;
   type := GetSType(sym) ;

   IF NOT q(type)
   THEN
      result := FALSE
   END ;
   subscript := GetArraySubscript(sym) ;
   IF subscript#NulSym
   THEN
      Assert(IsSubscript(subscript)) ;
      type := GetSType(subscript) ;
      IF NOT q(type)
      THEN
         result := FALSE
      END ;
      type := SkipType(type) ;
      (* the array might be declared as ARRAY type OF foo *)
      low  := GetTypeMin(type) ;
      high := GetTypeMax(type) ;
      IF NOT q(low)
      THEN
         result := FALSE
      END ;
      IF NOT q(high)
      THEN
         result := FALSE
      END ;
      align := GetAlignment(sym) ;
      IF (align#NulSym) AND (NOT q(align))
      THEN
         result := FALSE
      END
   END ;
   RETURN( result )
END IsArrayDependants ;


(*
   WalkArrayDependants - walks symbol, sym, dependants.
*)

PROCEDURE WalkArrayDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   align    : CARDINAL ;
   subscript: CARDINAL ;
   high, low: CARDINAL ;
   type     : CARDINAL ;
BEGIN
   Assert(IsArray(sym)) ;
   type := GetSType(sym) ;
   p(type) ;
   subscript := GetArraySubscript(sym) ;
   IF subscript#NulSym
   THEN
      Assert(IsSubscript(subscript)) ;
      type := GetSType(subscript) ;
      p(type) ;
      type := SkipType(type) ;
      (* the array might be declared as ARRAY type OF foo *)
      low  := GetTypeMin(type) ;
      high := GetTypeMax(type) ;
      p(low) ;
      p(high) ;
      align := GetAlignment (sym) ;
      IF align#NulSym
      THEN
         p(align)
      END
   END
END WalkArrayDependants ;


(*
   IsSetDependants - returns TRUE if the symbol, sym,
                     q(dependants) all return TRUE.
*)

PROCEDURE IsSetDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result         : BOOLEAN ;
   type, low, high: CARDINAL ;
BEGIN
   result := TRUE ;
   Assert(IsSet(sym)) ;

   type := GetDType(sym) ;
   IF NOT q(type)
   THEN
      result := FALSE
   END ;
   low  := GetTypeMin(type) ;
   high := GetTypeMax(type) ;
   IF NOT q(low)
   THEN
      result := FALSE
   END ;
   IF NOT q(high)
   THEN
      result := FALSE
   END ;
   RETURN( result )
END IsSetDependants ;


(*
   WalkSetDependants - walks dependants, sym.
*)

PROCEDURE WalkSetDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   type, low, high: CARDINAL ;
BEGIN
   Assert(IsSet(sym)) ;

   type := GetDType(sym) ;
   p(type) ;
   low  := GetTypeMin(type) ;
   p(low) ;
   high := GetTypeMax(type) ;
   p(high)
END WalkSetDependants ;


(*
   IsProcTypeDependants -
*)

PROCEDURE IsProcTypeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   i, p, son : CARDINAL ;
   ParamType,
   ReturnType: CARDINAL ;
   result    : BOOLEAN ;
BEGIN
   result := TRUE ;
   Assert(IsProcType(sym)) ;
   i := 1 ;
   ReturnType := GetSType(sym) ;
   p := NoOfParamAny (sym) ;
   WHILE i<=p DO
      son := GetNthParamAny (sym, i) ;
      ParamType := GetSType(son) ;
      IF NOT q(ParamType)
      THEN
         result := FALSE
      END ;
      INC(i)
   END ;
   IF (ReturnType=NulSym) OR q(ReturnType)
   THEN
      RETURN( result )
   ELSE
      RETURN( FALSE )
   END
END IsProcTypeDependants ;


(*
   WalkProcTypeDependants - walks dependants, sym.
*)

PROCEDURE WalkProcTypeDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i, n, son : CARDINAL ;
   ParamType,
   ReturnType: CARDINAL ;
BEGIN
   Assert(IsProcType(sym)) ;
   i := 1 ;
   ReturnType := GetSType(sym) ;
   n := NoOfParamAny (sym) ;
   WHILE i<=n DO
      son := GetNthParamAny (sym, i) ;
      ParamType := GetSType(son) ;
      p(ParamType) ;
      INC(i)
   END ;
   IF ReturnType#NulSym
   THEN
      p(ReturnType)
   END
END WalkProcTypeDependants ;


(*
   IsProcedureDependants -
*)

PROCEDURE IsProcedureDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   i, son    : CARDINAL ;
   type,
   ReturnType: CARDINAL ;
   result    : BOOLEAN ;
BEGIN
   result := TRUE ;
   Assert(IsProcedure(sym)) ;
   i := 1 ;
   ReturnType := GetSType(sym) ;
   WHILE GetNth(sym, i)#NulSym DO
      son := GetNth(sym, i) ;
      type := GetSType(son) ;
      IF NOT q(type)
      THEN
         result := FALSE
      END ;
      INC(i)
   END ;
   IF (ReturnType=NulSym) OR q(ReturnType)
   THEN
      RETURN( result )
   ELSE
      RETURN( FALSE )
   END
END IsProcedureDependants ;


(*
   WalkProcedureDependants - walks dependants, sym.
*)

PROCEDURE WalkProcedureDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   i, son    : CARDINAL ;
   type,
   ReturnType: CARDINAL ;
BEGIN
   Assert(IsProcedure(sym)) ;
   i := 1 ;
   ReturnType := GetSType(sym) ;
   WHILE GetNth(sym, i)#NulSym DO
      son := GetNth(sym, i) ;
      type := GetSType(son) ;
      p(type) ;
      INC(i)
   END ;
   IF ReturnType#NulSym
   THEN
      p(ReturnType)
   END
END WalkProcedureDependants ;


(*
   IsUnboundedDependants - returns TRUE if the symbol, sym,
                           q(dependants) all return TRUE.
*)

PROCEDURE IsUnboundedDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
BEGIN
   result := TRUE ;
   IF NOT q(GetUnboundedRecordType(sym))
   THEN
      result := FALSE
   END ;
   IF NOT q(Cardinal)
   THEN
      result := FALSE
   END ;
   IF NOT q(GetSType(sym))
   THEN
      result := FALSE
   END ;
   RETURN( result )
END IsUnboundedDependants ;


(*
   WalkUnboundedDependants - walks the dependants of, sym.
*)

PROCEDURE WalkUnboundedDependants (sym: CARDINAL; p: WalkAction) ;
BEGIN
   p(GetUnboundedRecordType(sym)) ;
   p(Cardinal) ;
   p(GetSType(sym))
END WalkUnboundedDependants ;


(*
   IsTypeDependants - returns TRUE if all q(dependants) return
                      TRUE.
*)

PROCEDURE IsTypeDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   align: CARDINAL ;
   type : CARDINAL ;
   final: BOOLEAN ;
BEGIN
   type := GetSType(sym) ;
   final := TRUE ;
   IF (type#NulSym) AND (NOT q(type))
   THEN
      final := FALSE
   END ;
   align := GetAlignment(sym) ;
   IF (align#NulSym) AND (NOT q(align))
   THEN
      final := FALSE
   END ;
   RETURN( final )
END IsTypeDependants ;


(*
   WalkTypeDependants - walks all dependants of, sym.
*)

PROCEDURE WalkTypeDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   align: CARDINAL ;
   type : CARDINAL ;
BEGIN
   type := GetSType(sym) ;
   IF type#NulSym
   THEN
      p(type)
   END ;
   align := GetAlignment(sym) ;
   IF align#NulSym
   THEN
      p(align)
   END
END WalkTypeDependants ;


(*
   PoisonSymbols - poisons all gcc symbols from procedure, sym.
                   A debugging aid.
*)

PROCEDURE PoisonSymbols (sym: CARDINAL) ;
BEGIN
   IF IsProcedure(sym)
   THEN
      ForeachLocalSymDo(sym, Poison)
   END
END PoisonSymbols ;


(*
   ConstantKnownAndUsed -
*)

PROCEDURE ConstantKnownAndUsed (sym: CARDINAL; t: tree) ;
BEGIN
   DeclareConstantFromTree(sym, RememberConstant(t))
END ConstantKnownAndUsed ;


(*
   InitDeclarations - initializes default types and the source filename.
*)

PROCEDURE InitDeclarations ;
BEGIN
   DeclareDefaultTypes ;
   DeclareDefaultConstants
END InitDeclarations ;


BEGIN
   FreeGroup := NIL ;
   GlobalGroup := InitGroup () ;
   ChainedList := InitSet(1) ;
   WatchList := InitSet(1) ;
   VisitedList := NIL ;
   EnumerationIndex := InitIndex(1) ;
   HaveInitDefaultTypes := FALSE ;
   recursionCaught := FALSE
END M2GCCDeclare.
