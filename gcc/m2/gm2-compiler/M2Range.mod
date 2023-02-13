(* M2Range.mod exports procedures which maintain the range checking.

Copyright (C) 2008-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Range ;


FROM SymbolTable IMPORT NulSym, GetLowestType, PutReadQuad, RemoveReadQuad,
                        IsVar, IsConst, PushValue, GetSubrange, GetType,
                        IsSubrange, GetSymName, IsTemporary, IsSet,
                        IsRecord, IsPointer, IsArray, IsProcType, IsConstLit,
                        IsAModula2Type, IsUnbounded, IsEnumeration, GetMode,
                        IsConstString, MakeConstLit, SkipType, IsProcedure,
                        IsParameter, GetDeclaredMod, IsVarParam, GetNthParam,
                        ModeOfAddr ;

FROM m2tree IMPORT Tree, debug_tree ;
FROM m2linemap IMPORT ErrorAt, GetFilenameFromLocation, GetColumnNoFromLocation, GetLineNoFromLocation ;

FROM m2type IMPORT GetMinFrom, GetMaxFrom,
                   GetIntegerType, GetTreeType,
                   GetPointerType,
                   AddStatement ;

FROM m2statement IMPORT BuildProcedureCallTree, BuildIfThenElseEnd, BuildIfThenDoEnd ;

FROM m2expr IMPORT CompareTrees, BuildSub, BuildAdd, GetIntegerZero, GetIntegerOne,
                   BuildAddr, BuildIndirect, BuildGreaterThan, BuildLessThan,
                   BuildGreaterThanOrEqual,
                   GetPointerZero, BuildNegate, BuildEqualTo, BuildLessThanOrEqual,
                   IsTrue, IsFalse, TreeOverflow ;

FROM m2convert IMPORT BuildConvert ;
FROM m2statement IMPORT BuildParam ;
FROM m2decl IMPORT BuildStringConstant, BuildIntegerConstant ;
FROM m2builtins IMPORT BuiltInIsfinite ;

FROM M2Debug IMPORT Assert ;
FROM Indexing IMPORT Index, InitIndex, InBounds, PutIndice, GetIndice ;
FROM Storage IMPORT ALLOCATE ;
FROM M2ALU IMPORT PushIntegerTree, PushInt, ConvertToInt, Equ, Gre, Less, GreEqu ;
FROM M2Options IMPORT VariantValueChecking ;

FROM M2Error IMPORT Error, InternalError, ErrorFormat0, ErrorFormat1, ErrorFormat2, FlushErrors,
                    GetAnnounceScope ;

FROM M2ColorString IMPORT quoteOpen, quoteClose ;

FROM M2MetaError IMPORT MetaError1, MetaError2, MetaError3,
                        MetaErrorT0, MetaErrorT1, MetaErrorT2, MetaErrorT3,
                        MetaErrorsT1, MetaErrorsT2, MetaErrorsT3, MetaErrorsT4,
                        MetaErrorStringT1, MetaErrorStringT2, MetaErrorStringT3,
                        MetaString3 ;

FROM M2LexBuf IMPORT UnknownTokenNo, GetTokenNo, FindFileNameFromToken, TokenToLineNo, TokenToColumnNo, TokenToLocation ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM M2GCCDeclare IMPORT TryDeclareConstant, DeclareConstructor ;
FROM M2Quads IMPORT QuadOperator, PutQuad, SubQuad, WriteOperand ;
FROM SymbolConversion IMPORT GccKnowsAbout, Mod2Gcc ;
FROM Lists IMPORT List ;
FROM NameKey IMPORT Name, MakeKey, KeyToCharStar ;
FROM StdIO IMPORT Write ;
FROM DynamicStrings IMPORT String, string, Length, InitString, ConCat, ConCatChar, Mark, InitStringCharStar, KillString ;
FROM M2GenGCC IMPORT GetHighFromUnbounded, StringToChar, LValueToGenericPtr, ZConstToTypedConst ;
FROM M2System IMPORT Address, Word, Loc, Byte, IsWordN, IsRealN, IsComplexN ;
FROM FormatStrings IMPORT Sprintf0, Sprintf1, Sprintf2 ;

FROM M2Check IMPORT ParameterTypeCompatible, ExpressionTypeCompatible ;

FROM M2Base IMPORT Nil, IsRealType, GetBaseTypeMinMax,
                   Cardinal, Integer, ZType, IsComplexType,
                   IsAssignmentCompatible,
                   IsExpressionCompatible,
                   IsParameterCompatible,
                   ExceptionAssign,
                   ExceptionReturn,
                   ExceptionInc, ExceptionDec,
                   ExceptionIncl, ExceptionExcl,
                   ExceptionShift, ExceptionRotate,
                   ExceptionStaticArray, ExceptionDynamicArray,
                   ExceptionForLoopBegin, ExceptionForLoopTo, ExceptionForLoopEnd,
                   ExceptionPointerNil, ExceptionNoReturn, ExceptionCase,
                   ExceptionNonPosDiv, ExceptionNonPosMod,
                   ExceptionZeroDiv, ExceptionZeroRem,
		   ExceptionWholeValue, ExceptionRealValue,
                   ExceptionParameterBounds,
                   ExceptionNo ;

FROM M2CaseList IMPORT CaseBoundsResolved, OverlappingCaseBounds, WriteCase, MissingCaseBounds, TypeCaseBounds ;


TYPE
   TypeOfRange = (assignment, returnassignment, subrangeassignment,
                  inc, dec, incl, excl, shift, rotate,
                  typeexpr, typeassign, typeparam, paramassign,
                  staticarraysubscript,
                  dynamicarraysubscript,
                  forloopbegin, forloopto, forloopend,
                  pointernil, noreturn, noelse,
                  casebounds,
                  wholenonposdiv, wholenonposmod,
                  wholezerodiv, wholezerorem, none) ;

   Range = POINTER TO RECORD
                         type          : TypeOfRange ;
                         des,
                         expr,
                         desLowestType,
                         exprLowestType: CARDINAL ;
                         procedure     : CARDINAL ;
                         paramNo       : CARDINAL ;
                         isLeftValue   : BOOLEAN ;   (* is des an LValue,
                                                        only used in pointernil *)
                         dimension     : CARDINAL ;
                         caseList      : CARDINAL ;
                         tokenNo       : CARDINAL ;
                         errorReported : BOOLEAN ;  (* error message reported yet? *)
                         strict        : BOOLEAN ;  (* is it a comparison expression?  *)
                         isin          : BOOLEAN ;  (* expression created by IN operator?  *)
                      END ;


VAR
   TopOfRange: CARDINAL ;
   RangeIndex: Index ;


(*
   OverlapsRange - returns TRUE if a1..a2 overlaps with b1..b2.
*)

PROCEDURE OverlapsRange (a1, a2, b1, b2: Tree) : BOOLEAN ;
BEGIN
   (* RETURN( ((a1<=b2) AND (a2>=b1)) ) *)
   RETURN( (CompareTrees(a1, b2)<=0) AND (CompareTrees(a2, b1)>=0) )
END OverlapsRange ;


(*
   IsGreater - returns TRUE if a>b.
*)

PROCEDURE IsGreater (a, b: Tree) : BOOLEAN ;
BEGIN
   RETURN( CompareTrees(a, b)>0 )
END IsGreater ;


(*
   IsGreaterOrEqual - returns TRUE if a>=b.
*)

PROCEDURE IsGreaterOrEqual (a, b: Tree) : BOOLEAN ;
BEGIN
   RETURN( CompareTrees(a, b)>=0 )
END IsGreaterOrEqual ;


(*
   IsEqual - returns TRUE if a=b.
*)

PROCEDURE IsEqual (a, b: Tree) : BOOLEAN ;
BEGIN
   RETURN( CompareTrees(a, b)=0 )
END IsEqual ;


(*
   IsGreaterOrEqualConversion - tests whether t>=e.
*)

PROCEDURE IsGreaterOrEqualConversion (location: location_t; l: CARDINAL; d, e: CARDINAL) : BOOLEAN ;
BEGIN
   IF GetType(d)=NulSym
   THEN
      IF GetType(e)=NulSym
      THEN
         RETURN( IsGreaterOrEqual(Mod2Gcc(l), LValueToGenericPtr(location, e)) )
      ELSE
         RETURN( IsGreaterOrEqual(BuildConvert(location, Mod2Gcc(SkipType(GetType(e))), Mod2Gcc(l), FALSE),
                                  LValueToGenericPtr(location, e)) )
      END
   ELSE
      RETURN( IsGreaterOrEqual(BuildConvert(location, Mod2Gcc(SkipType(GetType(d))), Mod2Gcc(l), FALSE),
                               LValueToGenericPtr(location, e)) )
   END
END IsGreaterOrEqualConversion ;


(*
   IsEqualConversion - returns TRUE if a=b.
*)

PROCEDURE IsEqualConversion (l: CARDINAL; d, e: CARDINAL) : BOOLEAN ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(GetDeclaredMod(l)) ;
   IF GetType(d)=NulSym
   THEN
      IF GetType(e)=NulSym
      THEN
         RETURN( IsEqual(Mod2Gcc(l), LValueToGenericPtr(location, e)) )
      ELSE
         RETURN( IsEqual(BuildConvert(location, Mod2Gcc(SkipType(GetType(e))), Mod2Gcc(l), FALSE),
                         LValueToGenericPtr(location, e)) )
      END
   ELSE
      RETURN( IsEqual(BuildConvert(location, Mod2Gcc(SkipType(GetType(d))), Mod2Gcc(l), FALSE),
                      LValueToGenericPtr(location, e)) )
   END
END IsEqualConversion ;


(*
   lookupExceptionHandler -
*)

PROCEDURE lookupExceptionHandler (type: TypeOfRange) : CARDINAL ;
BEGIN
   CASE type OF

   assignment           : RETURN( ExceptionAssign ) |
   returnassignment     : RETURN( ExceptionReturn ) |
   subrangeassignment   : InternalError ('not expecting this case value') |
   inc                  : RETURN( ExceptionInc ) |
   dec                  : RETURN( ExceptionDec ) |
   incl                 : RETURN( ExceptionIncl ) |
   excl                 : RETURN( ExceptionExcl ) |
   shift                : RETURN( ExceptionShift ) |
   rotate               : RETURN( ExceptionRotate ) |
   typeassign           : InternalError ('not expecting this case value') |
   typeparam            : InternalError ('not expecting this case value') |
   typeexpr             : InternalError ('not expecting this case value') |
   paramassign          : RETURN( ExceptionParameterBounds ) |
   staticarraysubscript : RETURN( ExceptionStaticArray ) |
   dynamicarraysubscript: RETURN( ExceptionDynamicArray ) |
   forloopbegin         : RETURN( ExceptionForLoopBegin ) |
   forloopto            : RETURN( ExceptionForLoopTo ) |
   forloopend           : RETURN( ExceptionForLoopEnd ) |
   pointernil           : RETURN( ExceptionPointerNil ) |
   noreturn             : RETURN( ExceptionNoReturn ) |
   noelse               : RETURN( ExceptionCase ) |
   casebounds           : InternalError ('not expecting this case value') |
   wholenonposdiv       : RETURN( ExceptionNonPosDiv ) |
   wholenonposmod       : RETURN( ExceptionNonPosMod ) |
   wholezerodiv         : RETURN( ExceptionZeroDiv ) |
   wholezerorem         : RETURN( ExceptionZeroRem ) |
   none                 : RETURN( ExceptionNo )

   ELSE
      InternalError ('enumeration value unknown')
   END
END lookupExceptionHandler ;


(*
   InitRange - returns a new range item.
*)

PROCEDURE InitRange () : CARDINAL ;
VAR
   r: CARDINAL ;
   p: Range ;
BEGIN
   INC(TopOfRange) ;
   r := TopOfRange ;
   NEW(p) ;
   IF p=NIL
   THEN
      InternalError ('out of memory error')
   ELSE
      WITH p^ DO
         type           := none ;
         des            := NulSym ;
         expr           := NulSym ;
         desLowestType  := NulSym ;
         exprLowestType := NulSym ;
         isLeftValue    := FALSE ;   (* ignored in all cases other *)
         dimension      := 0 ;
         caseList       := 0 ;
         tokenNo        := 0 ;       (* than pointernil            *)
         errorReported  := FALSE
      END ;
      PutIndice(RangeIndex, r, p)
   END ;
   RETURN( r )
END InitRange ;


(*
   reportedError - returns whether this is the first time this error has been
                   reported.
*)

PROCEDURE reportedError (r: CARDINAL) : BOOLEAN ;
VAR
   p: Range ;
BEGIN
   p := GetIndice (RangeIndex, r) ;
   RETURN p^.errorReported
END reportedError ;


(*
   setReported - assigns errorReported to TRUE.
*)

PROCEDURE setReported (r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice (RangeIndex, r) ;
   p^.errorReported := TRUE
END setReported ;


(*
   PutRange - initializes contents of, p, to
              d, e and their lowest types.
              It also fills in the current token no
              and returns, p.
*)

PROCEDURE PutRange (tokno: CARDINAL; p: Range; t: TypeOfRange; d, e: CARDINAL) : Range ;
BEGIN
   WITH p^ DO
      type           := t ;
      des            := d ;
      expr           := e ;
      desLowestType  := GetLowestType (d) ;
      exprLowestType := GetLowestType (e) ;
      tokenNo        := tokno ;
      strict         := FALSE ;
      isin           := FALSE
   END ;
   RETURN p
END PutRange ;


(*
   chooseTokenPos - returns, tokenpos, if it is not the unknown location, otherwise
                    it returns GetTokenNo.
*)

PROCEDURE chooseTokenPos (tokenpos: CARDINAL) : CARDINAL ;
BEGIN
   IF tokenpos = UnknownTokenNo
   THEN
      RETURN GetTokenNo ()
   ELSE
      RETURN tokenpos
   END
END chooseTokenPos ;


(*
   PutRangeNoLow - initializes contents of, p.  It
                   does not set lowest types as they may be
                   unknown at this point.
*)

PROCEDURE PutRangeNoLow (tokpos: CARDINAL; p: Range; t: TypeOfRange; d, e: CARDINAL) : Range ;
BEGIN
   WITH p^ DO
      type           := t ;
      des            := d ;
      expr           := e ;
      desLowestType  := NulSym ;
      exprLowestType := NulSym ;
      isLeftValue    := FALSE ;
      tokenNo        := chooseTokenPos (tokpos) ;
      strict         := FALSE ;
      isin           := FALSE
   END ;
   RETURN p
END PutRangeNoLow ;


(*
   PutRangeExpr - initializes contents of, p.  It
                  does not set lowest types as they may be
                  unknown at this point.
*)

PROCEDURE PutRangeExpr (tokpos: CARDINAL; p: Range; t: TypeOfRange;
                        d, e: CARDINAL; strict, isin: BOOLEAN) : Range ;
BEGIN
   WITH p^ DO
      type           := t ;
      des            := d ;
      expr           := e ;
      desLowestType  := NulSym ;
      exprLowestType := NulSym ;
      isLeftValue    := FALSE ;
      tokenNo        := chooseTokenPos (tokpos) ;
   END ;
   p^.strict := strict ;
   p^.isin := isin ;
   RETURN p
END PutRangeExpr ;


(*
   PutRangePointer - initializes contents of, p, to
                     d, isLeft and their lowest types.
                     It also fills in the current token no
                     and returns, p.
*)

PROCEDURE PutRangePointer (tokpos: CARDINAL;
                           p: Range; d: CARDINAL; isLeft: BOOLEAN) : Range ;
BEGIN
   WITH p^ DO
      type           := pointernil ;
      des            := d ;
      expr           := NulSym ;
      desLowestType  := GetLowestType(GetType(d)) ;
      exprLowestType := NulSym ;
      isLeftValue    := isLeft ;
      tokenNo        := tokpos ;
      strict         := FALSE ;
      isin           := FALSE
   END ;
   RETURN p
END PutRangePointer ;


(*
   PutRangeNoEval - initializes contents of, p, to a non evaluation
                    runtime check such as a no else clause or
                    no return found in function call.
*)

PROCEDURE PutRangeNoEval (p: Range; t: TypeOfRange) : Range ;
BEGIN
   WITH p^ DO
      type    := t ;
      tokenNo := GetTokenNo ()
   END ;
   RETURN p
END PutRangeNoEval ;


(*
   PutRange - initializes contents of, p, to
              d, e and its lowest type.
              It also fills in the current token no
              and returns, p.
*)

PROCEDURE PutRangeUnary (tokno: CARDINAL; p: Range; t: TypeOfRange; d, e: CARDINAL) : Range ;
BEGIN
   WITH p^ DO
      type           := t ;
      des            := d ;
      expr           := e ;
      desLowestType  := GetLowestType(d) ;
      exprLowestType := NulSym ;
      isLeftValue    := FALSE ;
      tokenNo        := chooseTokenPos (tokno) ;
      strict         := FALSE ;
      isin           := FALSE
   END ;
   RETURN( p )
END PutRangeUnary ;


(*
   PutRangeParam - initializes contents of, p, to contain the parameter
                   type checking information.
                   It also fills in the current token no
                   and returns, p.
*)

PROCEDURE PutRangeParam (p: Range; t: TypeOfRange; proc: CARDINAL;
                         i: CARDINAL; formal, actual: CARDINAL) : Range ;
BEGIN
   WITH p^ DO
      type           := t ;
      des            := formal ;
      expr           := actual ;
      desLowestType  := NulSym ;
      exprLowestType := NulSym ;
      procedure      := proc ;
      paramNo        := i ;
      isLeftValue    := FALSE ;
      tokenNo        := GetTokenNo () ;
      strict         := FALSE ;
      isin           := FALSE
   END ;
   RETURN p
END PutRangeParam ;


(*
   PutRangeArraySubscript - initializes contents of, p, to
                            d, e and their lowest types.  It also
                            assigns, dim.
                            It also fills in the current token no
                            and returns, p.
*)

PROCEDURE PutRangeArraySubscript (p: Range; t: TypeOfRange;
                                  d, e: CARDINAL; dim: CARDINAL) : Range ;
BEGIN
   WITH p^ DO
      type           := t ;
      des            := d ;
      expr           := e ;
      desLowestType  := GetLowestType(d) ;
      exprLowestType := GetLowestType(e) ;
      dimension      := dim ;
      tokenNo        := GetTokenNo () ;
      strict         := FALSE ;
      isin           := FALSE
   END ;
   RETURN p
END PutRangeArraySubscript ;


(*
   InitAssignmentRangeCheck - returns a range check node which
                              remembers the information necessary
                              so that a range check for d := e
                              can be generated later on.
*)

PROCEDURE InitAssignmentRangeCheck (tokno: CARDINAL; d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRange (tokno, GetIndice (RangeIndex, r), assignment, d, e) # NIL) ;
   RETURN r
END InitAssignmentRangeCheck ;


(*
   InitReturnRangeCheck - returns a range check node which
                          remembers the information necessary
                          so that a range check for RETURN e
                          from procedure, d, can be generated later on.
*)

PROCEDURE InitReturnRangeCheck (tokno: CARDINAL; d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRange (tokno, GetIndice (RangeIndex, r), returnassignment, d, e) # NIL) ;
   RETURN r
END InitReturnRangeCheck ;


(*
   InitSubrangeRangeCheck - returns a range check node which
                            remembers the information necessary
                            so that a range check for d := e
                            can be generated later on.
*)

PROCEDURE InitSubrangeRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRange (GetTokenNo (), GetIndice (RangeIndex, r), subrangeassignment, d, e) # NIL) ;
   RETURN r
END InitSubrangeRangeCheck ;


(*
   InitStaticArraySubscriptRangeCheck - returns a range check node which
                                        remembers the information necessary
                                        so that a range check for d[e]
                                        can be generated later on.
*)

PROCEDURE InitStaticArraySubscriptRangeCheck (d, e, dim: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeArraySubscript (GetIndice (RangeIndex, r), staticarraysubscript, d, e, dim) # NIL) ;
   RETURN r
END InitStaticArraySubscriptRangeCheck ;


(*
   InitDynamicArraySubscriptRangeCheck - returns a range check node which
                                         remembers the information necessary
                                         so that a range check for d[e]
                                         can be generated later on.
*)

PROCEDURE InitDynamicArraySubscriptRangeCheck (d, e, dim: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeArraySubscript (GetIndice (RangeIndex, r), dynamicarraysubscript, d, e, dim) # NIL) ;
   RETURN r
END InitDynamicArraySubscriptRangeCheck ;


(*
   InitIncRangeCheck - returns a range check node which
                       remembers the information necessary
                       so that a range check for INC(d, e)
                       can be generated later on.
*)

PROCEDURE InitIncRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRange (GetTokenNo (), GetIndice (RangeIndex, r), inc, d, e) # NIL) ;
   RETURN r
END InitIncRangeCheck ;


(*
   InitDecRangeCheck - returns a range check node which
                       remembers the information necessary
                       so that a range check for DEC(d, e)
                       can be generated later on.
*)

PROCEDURE InitDecRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRange (GetTokenNo (), GetIndice (RangeIndex, r), dec, d, e) # NIL) ;
   RETURN r
END InitDecRangeCheck ;


(*
   InitInclCheck - checks to see that bit, e, is type compatible with
                   e and also in range.
*)

PROCEDURE InitInclCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeNoLow (GetTokenNo (), GetIndice (RangeIndex, r), incl, d, e) # NIL) ;
   RETURN r
END InitInclCheck ;


(*
   InitExclCheck - checks to see that bit, e, is type compatible with
                   e and also in range.
*)

PROCEDURE InitExclCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeNoLow (GetTokenNo (), GetIndice (RangeIndex, r), excl, d, e) # NIL) ;
   RETURN r
END InitExclCheck ;


(*
   InitShiftCheck - checks to see that bit, e, is type compatible with
                    d and also in range.
*)

PROCEDURE InitShiftCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeNoLow (GetTokenNo (), GetIndice (RangeIndex, r), shift, d, e) # NIL) ;
   RETURN r
END InitShiftCheck ;


(*
   InitRotateCheck - checks to see that bit, e, is type compatible with
                     d and also in range.
*)

PROCEDURE InitRotateCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeNoLow (GetTokenNo (), GetIndice (RangeIndex, r), rotate, d, e) # NIL) ;
   RETURN r
END InitRotateCheck ;


(*
   InitTypesAssignmentCheck - checks to see that the types of, d, and, e,
                              are assignment compatible.
*)

PROCEDURE InitTypesAssignmentCheck (tokno: CARDINAL; d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeNoLow (tokno, GetIndice (RangeIndex, r), typeassign, d, e) # NIL) ;
   RETURN r
END InitTypesAssignmentCheck ;


(*
   InitTypesParameterCheck - checks to see that the types of, d,
                             and, e, are parameter compatible.
*)

PROCEDURE InitTypesParameterCheck (proc: CARDINAL; i: CARDINAL;
                                   formal, actual: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeParam (GetIndice (RangeIndex, r), typeparam, proc, i, formal, actual) # NIL) ;
   RETURN r
END InitTypesParameterCheck ;


(*
   PutRangeParamAssign - initializes contents of, p, to contain the parameter
                         type checking information.
                         It also fills in the current token no
                         and returns, p.
*)

PROCEDURE PutRangeParamAssign (p: Range; t: TypeOfRange; proc: CARDINAL;
                               i: CARDINAL; formal, actual: CARDINAL) : Range ;
BEGIN
   WITH p^ DO
      type           := t ;
      des            := formal ;
      expr           := actual ;
      desLowestType  := GetLowestType (des) ;
      exprLowestType := GetLowestType (expr) ;
      procedure      := proc ;
      paramNo        := i ;
      dimension      := i ;
      isLeftValue    := FALSE ;
      tokenNo        := GetTokenNo ()
   END ;
   RETURN( p )
END PutRangeParamAssign ;


(*
   InitParameterRangeCheck - checks to see that the types of, d, and, e,
                             are parameter compatible.
*)

PROCEDURE InitParameterRangeCheck (proc: CARDINAL; i: CARDINAL;
                                   formal, actual: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeParamAssign (GetIndice (RangeIndex, r), paramassign, proc, i, formal, actual) # NIL) ;
   RETURN r
END InitParameterRangeCheck ;


(*
   InitTypesExpressionCheck - checks to see that the types of, d, and, e,
                              are expression compatible.
*)

PROCEDURE InitTypesExpressionCheck (tokno: CARDINAL; d, e: CARDINAL; strict, isin: BOOLEAN) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   Assert (PutRangeExpr (tokno, GetIndice (RangeIndex, r), typeexpr, d, e, strict, isin) # NIL) ;
   RETURN r
END InitTypesExpressionCheck ;


(*
   InitForLoopBeginRangeCheck - returns a range check node which
                                remembers the information necessary
                                so that a range check for FOR d := e TO .. DO
                                can be generated later on.
*)

PROCEDURE InitForLoopBeginRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRange (GetTokenNo (), GetIndice (RangeIndex, r), forloopbegin, d, e) # NIL) ;
   RETURN r
END InitForLoopBeginRangeCheck ;


(*
   InitForLoopToRangeCheck - returns a range check node which
                             remembers the information necessary
                             so that a range check for FOR d := e TO .. DO
                             can be generated later on.
*)

PROCEDURE InitForLoopToRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRange (GetTokenNo (), GetIndice (RangeIndex, r), forloopto, d, e) # NIL) ;
   RETURN r
END InitForLoopToRangeCheck ;


(*
   InitForLoopEndRangeCheck - returns a range check node which
                              remembers the information necessary
                              so that a range check for
                              INC or DEC(d, e)
                              can be generated later on.
*)

PROCEDURE InitForLoopEndRangeCheck (d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRange (GetTokenNo (), GetIndice (RangeIndex, r), forloopend, d, e) # NIL) ;
   RETURN r
END InitForLoopEndRangeCheck ;


(*
   InitPointerRangeCheck - creates a pointer # NIL check.
*)

PROCEDURE InitPointerRangeCheck (tokno: CARDINAL;
                                 d: CARDINAL; isLeft: BOOLEAN) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangePointer (tokno, GetIndice (RangeIndex, r), d, isLeft) # NIL) ;
   RETURN r
END InitPointerRangeCheck ;


(*
   InitNoReturnRangeCheck - creates a check held in the function
                            to detect the absence of a RETURN
                            statement at runtime.
*)

PROCEDURE InitNoReturnRangeCheck () : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeNoEval (GetIndice(RangeIndex, r), noreturn) # NIL) ;
   RETURN r
END InitNoReturnRangeCheck ;


(*
   InitNoElseRangeCheck - creates a check held at the end of
                          a CASE statement without an ELSE
                          clause to detect its absence
                          at runtime.
*)

PROCEDURE InitNoElseRangeCheck () : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeNoEval (GetIndice (RangeIndex, r), noelse) # NIL) ;
   RETURN r
END InitNoElseRangeCheck ;


(*
   InitWholeNonPosDivCheck - creates a check expression for non positive
                             or zero 2nd operand to division.
*)

PROCEDURE InitWholeNonPosDivCheck (tokno: CARDINAL; d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeUnary (tokno, GetIndice (RangeIndex, r), wholenonposdiv, d, e) # NIL) ;
   RETURN r
END InitWholeNonPosDivCheck ;


(*
   InitWholeNonPosModCheck - creates a check expression for non positive
                             or zero 2nd operand to modulus.
*)

PROCEDURE InitWholeNonPosModCheck (tokno: CARDINAL; d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeUnary (tokno, GetIndice (RangeIndex, r), wholenonposmod, d, e) # NIL) ;
   RETURN r
END InitWholeNonPosModCheck ;


(*
   InitWholeZeroDivisionCheck - creates a check expression for zero 2nd
                                operand for division.
*)

PROCEDURE InitWholeZeroDivisionCheck (tokno: CARDINAL; d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeUnary (tokno, GetIndice (RangeIndex, r), wholezerodiv, d, e) # NIL) ;
   RETURN r
END InitWholeZeroDivisionCheck ;


(*
   InitWholeZeroRemainderCheck - creates a check expression for zero 2nd
                                 operand for remainder.
*)

PROCEDURE InitWholeZeroRemainderCheck (tokno: CARDINAL; d, e: CARDINAL) : CARDINAL ;
VAR
   r: CARDINAL ;
BEGIN
   r := InitRange () ;
   Assert (PutRangeUnary (tokno, GetIndice (RangeIndex, r), wholezerorem, d, e) # NIL) ;
   RETURN r
END InitWholeZeroRemainderCheck ;


(*
   FoldNil - attempts to fold the pointer against nil comparison.
*)

PROCEDURE FoldNil (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant (tokenno, des) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF GccKnowsAbout (des) AND IsConst (des)
      THEN
         PushValue (des) ;
         PushValue (Nil) ;
         IF Equ (tokenno)
         THEN
            MetaErrorT1 (tokenNo,
                         'attempting to dereference a pointer {%1Wa} whose value will be NIL',
                         des) ;
            PutQuad (q, ErrorOp, NulSym, NulSym, r)
         ELSE
            SubQuad (q)
         END
      END
   END
END FoldNil ;


(*
   GetMinMax - returns TRUE if we know the max and min of m2type.
*)

PROCEDURE GetMinMax (tokenno: CARDINAL; type: CARDINAL; VAR min, max: Tree) : BOOLEAN ;
VAR
   minC, maxC: CARDINAL ;
   location  : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   Assert (IsAModula2Type (type)) ;
   IF GccKnowsAbout(type) AND (NOT IsPointer(type)) AND
      (NOT IsArray(type)) AND (NOT IsRecord(type)) AND
      (NOT IsRecord(type)) AND (NOT IsUnbounded(type)) AND
      (NOT IsProcType(type)) AND (NOT IsRealType(type)) AND
      (NOT IsRealN(type)) AND (NOT IsComplexType(type)) AND
      (NOT IsComplexN(type)) AND
      (type#Address) AND (NOT IsSet(type)) AND
      (type#Word) AND (type#Loc) AND (type#Byte) AND (NOT IsWordN(type))
   THEN
      IF IsSubrange(type)
      THEN
         GetSubrange(type, maxC, minC) ;
         max := Mod2Gcc(maxC) ;
         min := Mod2Gcc(minC)
      ELSIF IsEnumeration(type)
      THEN
         GetBaseTypeMinMax(type, minC, maxC) ;
         max := Mod2Gcc(maxC) ;
         min := Mod2Gcc(minC)
      ELSE
         max := GetMaxFrom(location, Mod2Gcc(type)) ;
         min := GetMinFrom(location, Mod2Gcc(type))
      END ;
      max := BuildConvert (location, Mod2Gcc(type), max, FALSE) ;
      Assert (NOT TreeOverflow (max)) ;
      min := BuildConvert (location, Mod2Gcc(type), min, FALSE) ;
      Assert (NOT TreeOverflow (min)) ;
      RETURN TRUE
   ELSE
      RETURN FALSE
   END
END GetMinMax ;


(*
   OutOfRange - returns TRUE if expr lies outside min..max.
*)

PROCEDURE OutOfRange (tokenno: CARDINAL;
                      min: Tree;
                      expr: CARDINAL;
                      max: Tree;
                      type: CARDINAL) : BOOLEAN ;
BEGIN
   IF TreeOverflow (min)
   THEN
      WriteString ("overflow detected in min\n"); WriteLn ;
      debug_tree (min)
   END ;
   IF TreeOverflow (max)
   THEN
      WriteString ("overflow detected in max\n"); WriteLn ;
      debug_tree (max)
   END ;
   IF TreeOverflow (max)
   THEN
      WriteString ("overflow detected in expr\n"); WriteLn ;
      debug_tree (StringToChar (Mod2Gcc (expr), type, expr));
   END ;
   PushIntegerTree (StringToChar (Mod2Gcc (expr), type, expr)) ;
   PushIntegerTree (min) ;
   IF Less (tokenno)
   THEN
      RETURN TRUE
   END ;
   PushIntegerTree (StringToChar (Mod2Gcc (expr), type, expr)) ;
   PushIntegerTree (max) ;
   IF Gre (tokenno)
   THEN
      RETURN TRUE
   END ;
   RETURN FALSE
END OutOfRange ;


(*
   HandlerExists -
*)

PROCEDURE HandlerExists (r: CARDINAL) : BOOLEAN ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           : RETURN( ExceptionAssign#NulSym ) |
      returnassignment     : RETURN( ExceptionReturn#NulSym ) |
      subrangeassignment   : InternalError ('not expecting this case value') |
      inc                  : RETURN( ExceptionInc#NulSym ) |
      dec                  : RETURN( ExceptionDec#NulSym ) |
      incl                 : RETURN( ExceptionIncl#NulSym ) |
      excl                 : RETURN( ExceptionExcl#NulSym ) |
      shift                : RETURN( ExceptionShift#NulSym ) |
      rotate               : RETURN( ExceptionRotate#NulSym ) |
      typeassign           : RETURN( FALSE ) |
      typeparam            : RETURN( FALSE ) |
      typeexpr             : RETURN( FALSE ) |
      paramassign          : RETURN( ExceptionParameterBounds#NulSym ) |
      staticarraysubscript : RETURN( ExceptionStaticArray#NulSym ) |
      dynamicarraysubscript: RETURN( ExceptionDynamicArray#NulSym ) |
      forloopbegin         : RETURN( ExceptionForLoopBegin#NulSym ) |
      forloopto            : RETURN( ExceptionForLoopTo#NulSym ) |
      forloopend           : RETURN( ExceptionForLoopEnd#NulSym ) |
      pointernil           : RETURN( ExceptionPointerNil#NulSym ) |
      noreturn             : RETURN( ExceptionNoReturn#NulSym ) |
      noelse               : RETURN( ExceptionCase#NulSym ) |
      casebounds           : RETURN( FALSE ) |
      wholenonposdiv       : RETURN( ExceptionNonPosDiv#NulSym ) |
      wholenonposmod       : RETURN( ExceptionNonPosMod#NulSym ) |
      wholezerodiv         : RETURN( ExceptionZeroDiv#NulSym ) |
      wholezerorem         : RETURN( ExceptionZeroRem#NulSym ) |
      none                 : RETURN( FALSE )

      ELSE
         InternalError ('enumeration value unknown')
      END
   END
END HandlerExists ;


(*
   FoldAssignment -
*)

PROCEDURE FoldAssignment (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   min, max: Tree ;
BEGIN
   p := GetIndice (RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant (tokenNo, expr) ;
      IF desLowestType # NulSym
      THEN
         IF GccKnowsAbout (expr) AND IsConst (expr) AND
            GetMinMax (tokenno, desLowestType, min, max)
         THEN
            IF OutOfRange (tokenno, min, expr, max, desLowestType)
            THEN
               MetaErrorT2 (tokenNo,
                            'attempting to assign a value {%2Wa} to a designator {%1a} which will exceed the range of type {%1tad}',
                            des, expr) ;
               PutQuad (q, ErrorOp, NulSym, NulSym, r)
            ELSE
               SubQuad (q)
            END
         END
      END
   END
END FoldAssignment ;


(*
   FoldParameterAssign -
*)

PROCEDURE FoldParameterAssign (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant (tokenNo, expr) ;
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(tokenno, desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, expr, max, desLowestType)
            THEN
               (* this is safer to treat as an error, rather than a warning
                  otherwise the paramater might be widened
                  (if it is a constant).  *)
               MetaErrorT3(tokenNo,
                           'the {%3EN} actual parameter {%2a} will exceed the range of formal parameter type {%1tad}',
                           des, expr, dimension) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               SubQuad(q)
            END
         END
      END
   END
END FoldParameterAssign ;


(*
   FoldReturn - do we know this is reachable, if so generate an error message.
*)

PROCEDURE FoldReturn (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant (tokenNo, expr) ;
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(tokenno, desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, expr, max, desLowestType)
            THEN
               MetaErrorT2(tokenNo,
                           'attempting to return {%2Wa} from a procedure function {%1a} which will exceed exceed the range of type {%1tad}',
                           des, expr) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               SubQuad(q)
            END
         END
      END
   END
END FoldReturn ;


(*
   FoldInc -
*)

PROCEDURE FoldInc (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p          : Range ;
   t, min, max: Tree ;
   location   : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(tokenno, desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, GetIntegerZero(location), expr, max, desLowestType)
            THEN
               MetaErrorT2(tokenNo,
                           'operand to INC {%2Wa} exceeds the range of type {%1ts} of the designator {%1a}',
                           des, expr) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSIF GccKnowsAbout(des) AND IsConst(des) AND GccKnowsAbout(desLowestType)
            THEN
               t := BuildSub(location,
                             max,
                             BuildConvert(location, Mod2Gcc(desLowestType), Mod2Gcc(expr), FALSE),
                             FALSE) ;
               PushIntegerTree(Mod2Gcc(des)) ;
               PushIntegerTree(t) ;
               IF Gre(tokenNo)
               THEN
                  MetaErrorT1(tokenNo,
                              'the designator to INC {%1Wa} will exceed the range of type {%1ts}',
                              des) ;
                  PutQuad(q, ErrorOp, NulSym, NulSym, r)
               ELSE
                  (* range check is unnecessary *)
                  SubQuad(q)
               END
            END
         END
      END
   END
END FoldInc ;


(*
   FoldDec -
*)

PROCEDURE FoldDec (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p          : Range ;
   t, min, max: Tree ;
   location   : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(tokenno, desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, GetIntegerZero(location), expr, max, desLowestType)
            THEN
               MetaErrorT2(tokenNo,
                           'operand to DEC {%2Wa} exceeds the range of type {%1ts} of the designator {%1a}',
                           des, expr) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSIF GccKnowsAbout(des) AND IsConst(des) AND GccKnowsAbout(desLowestType)
            THEN
               t := BuildSub(location,
                             BuildConvert(location, Mod2Gcc(desLowestType), Mod2Gcc(expr), FALSE),
                             min,
                             FALSE) ;
               PushIntegerTree(Mod2Gcc(des)) ;
               PushIntegerTree(t) ;
               IF Less(tokenNo)
               THEN
                  MetaErrorT1(tokenNo,
                              'the designator to DEC {%1Wa} will exceed the range of type {%1ts}',
                              des) ;
                  PutQuad(q, ErrorOp, NulSym, NulSym, r)
               ELSE
                  (* range check is unnecessary *)
                  SubQuad(q)
               END
            END
         END
      END
   END
END FoldDec ;


(*
   CheckSetAndBit - returns TRUE if des is a set type and expr is compatible with des.
*)

PROCEDURE CheckSetAndBit (tokenno: CARDINAL;
                          des, expr: CARDINAL;
                          name: ARRAY OF CHAR) : BOOLEAN ;
VAR
   s: String ;
BEGIN
   IF IsSet(des)
   THEN
      IF IsExpressionCompatible(GetType(des), GetType(expr))
      THEN
         RETURN( TRUE )
      ELSE
         s := ConCat(ConCat(InitString('operands to '),
                            Mark(InitString(name))),
                     Mark(InitString(' {%1Etsd:{%2tsd:{%1tsd} and {%2tsd}}} are incompatible'))) ;
         MetaErrorStringT2(tokenno, s, des, expr) ;
         FlushErrors
      END
   ELSE
      s := ConCat(ConCat(InitString('first operand to '),
                         Mark(InitString(name))),
                  Mark(InitString(' is not a set {%1Etasd}'))) ;
      MetaErrorStringT1(tokenno, s, des) ;
      FlushErrors
   END ;
   RETURN( FALSE )
END CheckSetAndBit ;


(*
   CheckSet - returns TRUE if des is a set type and expr is compatible with INTEGER.
*)

PROCEDURE CheckSet (tokenno: CARDINAL;
                    des, expr: CARDINAL;
                    name: ARRAY OF CHAR) : BOOLEAN ;
VAR
   s: String ;
BEGIN
   IF IsSet(des)
   THEN
      IF IsParameterCompatible(Integer, GetType(expr))
      THEN
         RETURN( TRUE )
      ELSE
         s := ConCat(ConCat(InitString('operands to '),
                            Mark(InitString(name))),
                     Mark(InitString(' {%1Etsd:{%2tsd:{%1tsd} and {%2tsd}}} are incompatible'))) ;
         MetaErrorStringT2(tokenno, s, des, expr) ;
         FlushErrors
      END
   ELSE
      s := ConCat(ConCat(InitString('first operand to '),
                         Mark(InitString(name))),
                  Mark(InitString(' is not a set {%1Etasd}'))) ;
      MetaErrorStringT1(tokenno, s, des) ;
      FlushErrors
   END ;
   RETURN( FALSE )
END CheckSet ;


(*
   FoldIncl - folds an INCL statement if the operands are constant.
*)

PROCEDURE FoldIncl (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      desLowestType := SkipType(GetType(des)) ;
      IF desLowestType#NulSym
      THEN
         IF CheckSetAndBit(tokenno, desLowestType, expr, "INCL")
         THEN
            IF GccKnowsAbout(expr) AND IsConst(expr) AND
               GetMinMax(tokenno, desLowestType, min, max)
            THEN
               IF OutOfRange(tokenno, min, expr, max, desLowestType)
               THEN
                  MetaErrorT2(tokenNo,
                              'operand to INCL {%2Wa} exceeds the range of type {%1tasa}',
                              des, expr) ;
                  PutQuad(q, ErrorOp, NulSym, NulSym, r)
               ELSE
                  (* range check is unnecessary *)
                  SubQuad(q)
               END
            END
         END
      END
   END
END FoldIncl ;


(*
   FoldExcl - folds an EXCL statement if the operands are constant.
*)

PROCEDURE FoldExcl (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      desLowestType := SkipType(GetType(des)) ;
      IF desLowestType#NulSym
      THEN
         IF CheckSetAndBit(tokenno, desLowestType, expr, "EXCL")
         THEN
            IF GccKnowsAbout(expr) AND IsConst(expr) AND
               GetMinMax(tokenno, desLowestType, min, max)
            THEN
               IF OutOfRange(tokenno, min, expr, max, desLowestType)
               THEN
                  MetaErrorT2(tokenNo,
                              'operand to EXCL {%2Wa} exceeds the range of type {%1tasa}',
                              des, expr) ;
                  PutQuad(q, ErrorOp, NulSym, NulSym, r)
               ELSE
                  (* range check is unnecessary *)
                  SubQuad(q)
               END
            END
         END
      END
   END
END FoldExcl ;


(*
   FoldShift - folds an SHIFT test statement if the operands are constant.
*)

PROCEDURE FoldShift (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   ofType  : CARDINAL ;
   p       : Range ;
   shiftMin,
   shiftMax,
   min, max: Tree ;
   location   : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      desLowestType := SkipType(GetType(des)) ;
      IF desLowestType#NulSym
      THEN
         IF CheckSet(tokenno, desLowestType, expr, "SHIFT")
         THEN
            ofType := SkipType(GetType(desLowestType)) ;
            IF GccKnowsAbout(ofType) AND
               GccKnowsAbout(expr) AND IsConst(expr) AND
               GetMinMax(tokenno, ofType, min, max)
            THEN
               min := BuildConvert(location, GetIntegerType(), min, FALSE) ;
               max := BuildConvert(location, GetIntegerType(), max, FALSE) ;
               shiftMax := BuildAdd(location, BuildSub(location, max, min, FALSE),
                                    GetIntegerOne(location),
                                    FALSE) ;
               shiftMin := BuildNegate(location, shiftMax, FALSE) ;
               IF OutOfRange(tokenno, shiftMin, expr, shiftMax, desLowestType)
               THEN
                  MetaErrorT2(tokenNo,
                              'operand to SHIFT {%2Wa} exceeds the range of type {%1tasa}',
                              des, expr) ;
                  PutQuad(q, ErrorOp, NulSym, NulSym, r)
               ELSE
                  (* range check is unnecessary *)
                  SubQuad(q)
               END
            END
         END
      END
   END
END FoldShift ;


(*
   FoldRotate - folds a ROTATE test statement if the operands are constant.
*)

PROCEDURE FoldRotate (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   ofType   : CARDINAL ;
   p        : Range ;
   rotateMin,
   rotateMax,
   min, max : Tree ;
   location   : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      desLowestType := SkipType(GetType(des)) ;
      IF desLowestType#NulSym
      THEN
         IF CheckSet(tokenno, desLowestType, expr, "ROTATE")
         THEN
            ofType := SkipType(GetType(desLowestType)) ;
            IF GccKnowsAbout(ofType) AND
               GccKnowsAbout(expr) AND IsConst(expr) AND
               GetMinMax(tokenno, ofType, min, max)
            THEN
               min := BuildConvert(location, GetIntegerType(), min, FALSE) ;
               max := BuildConvert(location, GetIntegerType(), max, FALSE) ;
               rotateMax := BuildAdd(location,
                                     BuildSub(location, max, min, FALSE),
                                     GetIntegerOne(location),
                                     FALSE) ;
               rotateMin := BuildNegate(location, rotateMax, FALSE) ;
               IF OutOfRange(tokenno, rotateMin, expr, rotateMax, desLowestType)
               THEN
                  MetaErrorT2(tokenNo,
                              'operand to ROTATE {%2Wa} exceeds the range of type {%1tasa}',
                              des, expr) ;
                  PutQuad(q, ErrorOp, NulSym, NulSym, r)
               ELSE
                  (* range check is unnecessary *)
                  SubQuad(q)
               END
            END
         END
      END
   END
END FoldRotate ;


(*
   FoldTypeAssign -
*)

PROCEDURE FoldTypeAssign (q: CARDINAL; tokenNo: CARDINAL; des, expr: CARDINAL; r: CARDINAL) ;
VAR
   exprType: CARDINAL ;
BEGIN
   IF IsProcedure(expr)
   THEN
      exprType := expr
   ELSE
      exprType := GetType(expr)
   END ;

   IF IsAssignmentCompatible (GetType(des), exprType)
   THEN
      SubQuad(q)
   ELSE
      IF NOT reportedError (r)
      THEN
         IF IsProcedure (des)
         THEN
            MetaErrorsT2 (tokenNo,
                          'the return type {%1Etad} declared in procedure {%1Da}',
                          'is incompatible with the returned expression {%2ad}}',
                          des, expr) ;
         ELSE
            MetaErrorT3 (tokenNo,
                         'assignment designator {%1Ea} {%1ta:of type {%1ta}} {%1d:is a {%1d}} and expression {%2a} {%3ad:of type {%3ad}} are incompatible',
                         des, expr, exprType)
         END ;
         setReported (r) ;
         FlushErrors
      END
   END
END FoldTypeAssign ;


(*
   FoldTypeParam -
*)

PROCEDURE FoldTypeParam (q: CARDINAL; tokenNo: CARDINAL; formal, actual, procedure: CARDINAL; paramNo: CARDINAL) ;
BEGIN
   IF ParameterTypeCompatible (tokenNo,
                               '{%4EN} type failure between actual {%3ad} and the {%2ad}',
                               procedure, formal, actual, paramNo, IsVarParam (procedure, paramNo))
   THEN
      SubQuad(q)
   END
END FoldTypeParam ;


(*
   FoldTypeExpr -
*)

PROCEDURE FoldTypeExpr (q: CARDINAL; tokenNo: CARDINAL; left, right: CARDINAL; strict, isin: BOOLEAN; r: CARDINAL) ;
BEGIN
   IF (left # NulSym) AND (right # NulSym) AND (NOT reportedError (r))
   THEN
      IF ExpressionTypeCompatible (tokenNo,
                                   'expression of type {%1Etad} is incompatible with type {%2tad}',
                                   left, right, strict, isin)
      THEN
         SubQuad(q) ;
         setReported (r)
      END
   END
END FoldTypeExpr ;


(*
   CodeTypeAssign -
*)

PROCEDURE CodeTypeAssign (tokenNo: CARDINAL; des, expr: CARDINAL; r: CARDINAL) ;
VAR
   exprType: CARDINAL ;
BEGIN
   IF IsProcedure(expr)
   THEN
      exprType := expr
   ELSE
      exprType := GetType(expr)
   END ;
   IF NOT IsAssignmentCompatible(GetType(des), exprType)
   THEN
      IF NOT reportedError (r)
      THEN
         IF IsProcedure(des)
         THEN
            MetaErrorsT2(tokenNo,
                         'the return type {%1Etad} declared in procedure {%1Da}',
                         'is incompatible with the returned expression {%2EUa} {%2tad:of type {%2tad}}',
                         des, expr) ;
         ELSE
            MetaErrorT2(tokenNo,
                        'assignment designator {%1Ea} {%1ta:of type {%1ta}} {%1d:is a {%1d}} and expression {%2a} {%2tad:of type {%2tad}} are incompatible',
                        des, expr)
         END ;
         setReported (r)
      END
      (* FlushErrors *)
   END
END CodeTypeAssign ;


(*
   CodeTypeParam -
*)

PROCEDURE CodeTypeParam (tokenNo: CARDINAL; formal, actual, procedure: CARDINAL; paramNo: CARDINAL) ;
BEGIN
   IF NOT ParameterTypeCompatible (tokenNo,
                                   '{%4EN} type failure between actual {%3ad} and the formal {%2ad}',
                                   procedure, formal, actual, paramNo, IsVarParam (procedure, paramNo))
   THEN
   END
END CodeTypeParam ;


(*
   CodeTypeExpr -
*)

PROCEDURE CodeTypeExpr (tokenNo: CARDINAL; left, right: CARDINAL; strict, isin: BOOLEAN; r: CARDINAL) ;
BEGIN
   IF NOT reportedError (r)
   THEN
      IF ExpressionTypeCompatible (tokenNo,
                                   'expression of type {%1Etad} is incompatible with type {%2tad}',
                                   left, right, strict, isin)
      THEN
         setReported (r)
      END
   END
END CodeTypeExpr ;


(*
   FoldTypeCheck - folds a type check.  This is a no-op and it used
                   for checking types which are resolved post pass 3.
*)

PROCEDURE FoldTypeCheck (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      (* TryDeclareConstructor(q, expr) ; *)
      IF (GccKnowsAbout(des) OR (IsParameter(des) AND GccKnowsAbout(GetType(des)))) AND
          GccKnowsAbout(expr)
      THEN
         CASE type OF

         typeassign:  FoldTypeAssign(q, tokenNo, des, expr, r) |
         typeparam:   FoldTypeParam(q, tokenNo, des, expr, procedure, paramNo) |
         typeexpr:    FoldTypeExpr(q, tokenNo, des, expr, strict, isin, r)

         ELSE
            InternalError ('not expecting to reach this point')
         END
      END
   END
END FoldTypeCheck ;


(*
   CodeTypeCheck - folds a type check.  This is a no-op and it used
                   for checking types which are resolved post pass 3.
                   It does assume that both, des, and, expr, have been
                   resolved at this point.
*)

PROCEDURE CodeTypeCheck (tokenno: CARDINAL; r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      (* TryDeclareConstructor(0, expr) ; *)
      IF (GccKnowsAbout(des) OR (IsParameter(des) AND GccKnowsAbout(GetType(des)))) AND
          GccKnowsAbout(expr)
      THEN
         CASE type OF

         typeassign:  CodeTypeAssign(tokenNo, des, expr, r) |
         typeparam:   CodeTypeParam(tokenNo, des, expr, procedure, paramNo) |
         typeexpr:    CodeTypeExpr(tokenNo, des, expr, strict, isin, r)

         ELSE
            InternalError ('not expecting to reach this point')
         END
      ELSE
         InternalError ('expecting des and expr to be resolved')
      END
   END
END CodeTypeCheck ;


(*
   FoldForLoopBegin -
*)

PROCEDURE FoldForLoopBegin (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(tokenno, desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, expr, max, desLowestType)
            THEN
               MetaErrorT2(tokenNo,
                           'attempting to assign a value {%2Wa} to a FOR loop designator {%1a} which will exceed the range of type {%1tad}',
                           des, expr) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               SubQuad(q)
            END
         END
      END
   END
END FoldForLoopBegin ;


(*
   FoldForLoopTo -
*)

PROCEDURE FoldForLoopTo (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   min, max: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr) AND
            GetMinMax(tokenno, desLowestType, min, max)
         THEN
            IF OutOfRange(tokenno, min, expr, max, desLowestType)
            THEN
               MetaErrorT2(tokenNo,
                           'final value in FOR loop will exceed type range {%1Wtasa} of designator {%2a}',
                           des, expr) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               SubQuad(q)
            END
         END
      END
   END
END FoldForLoopTo ;


(*
   FoldStaticArraySubscript -
*)

PROCEDURE FoldStaticArraySubscript (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   min, max: Tree ;
BEGIN
   p := GetIndice (RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant (tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant (tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout (expr) AND IsConst (expr) AND
            GetMinMax (tokenno, desLowestType, min, max)
         THEN
            IF OutOfRange (tokenno, min, expr, max, desLowestType)
            THEN
               MetaErrorT3 (tokenNo,
                           'index {%2Wa} out of range found while attempting to access an element of a static array {%1a} in the {%3N} array subscript',
                            des, expr, dimension) ;
               PutQuad (q, ErrorOp, NulSym, NulSym, r)
            ELSE
               (* range check is unnecessary *)
               SubQuad (q)
            END
         END
      END
   END
END FoldStaticArraySubscript ;


(*
   FoldDynamicArraySubscript -
*)

PROCEDURE FoldDynamicArraySubscript (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p       : Range ;
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND IsConst(expr)
         THEN
            IF IsGreater(GetIntegerZero(location), BuildConvert(location, GetIntegerType(), Mod2Gcc(expr), FALSE))
            THEN
               MetaErrorT3(tokenNo,
                           'index {%2Wa} out of range found while attempting to access an element of a dynamic array {%1a} in the {%3N} array subscript',
                           des, expr, dimension) ;
               PutQuad(q, ErrorOp, NulSym, NulSym, r)
            ELSE
               (* cannot fold high bounds, so leave that for the runtime *)
            END
         END
      END
   END
END FoldDynamicArraySubscript ;


(*
   FoldCaseBounds -
*)

PROCEDURE FoldCaseBounds (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      IF CaseBoundsResolved(tokenno, caseList)
      THEN
         IF TypeCaseBounds (caseList)
         THEN
            (* nothing to do *)
         END ;
         IF OverlappingCaseBounds(caseList)
         THEN
            PutQuad(q, ErrorOp, NulSym, NulSym, r) ;
            IF VariantValueChecking AND MissingCaseBounds(tokenno, caseList)
            THEN
               (* nothing to do *)
            END
         ELSIF VariantValueChecking AND MissingCaseBounds(tokenno, caseList)
         THEN
            PutQuad(q, ErrorOp, NulSym, NulSym, r)
         ELSE
            SubQuad(q)
         END
      END
   END
END FoldCaseBounds ;


(*
   CodeCaseBounds - attempts to resolve whether the case bounds are legal.
                    This should resolve at compile time as all case bounds
                    must be constants.  We introduce a CodeCaseBounds as it
                    might be possible that constants have just been declared
                    during the code generation of this function.
*)

PROCEDURE CodeCaseBounds (tokenno: CARDINAL; caseList: CARDINAL) ;
BEGIN
   IF CaseBoundsResolved (tokenno, caseList)
   THEN
      IF TypeCaseBounds (caseList)
      THEN
         (* nothing to do *)
      END ;
      IF OverlappingCaseBounds (caseList)
      THEN
         (* nothing to do *)
      END ;
      IF MissingCaseBounds (tokenno, caseList)
      THEN
         (* nothing to do *)
      END
   ELSE
      MetaErrorT0 (tokenno, '{%E}the CASE statement ranges must be constants')
   END
END CodeCaseBounds ;


(*
   MakeAndDeclareConstLit - creates a constant of value and declares it to GCC.
*)

PROCEDURE MakeAndDeclareConstLit (tokenno: CARDINAL; value: Name; type: CARDINAL) : CARDINAL ;
VAR
   constant: CARDINAL ;
BEGIN
   constant := MakeConstLit (tokenno, value, type) ;
   TryDeclareConstant (tokenno, constant) ;  (* use quad tokenno, rather than the range tokenNo *)
   Assert (GccKnowsAbout (constant)) ;
   RETURN constant
END MakeAndDeclareConstLit ;


(*
   FoldNonPosDiv - attempts to fold the bound checking for a divide expression.
*)

PROCEDURE FoldNonPosDiv (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p   : Range ;
   zero: CARDINAL ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF GccKnowsAbout(expr) AND IsConst(expr)
      THEN
         zero := MakeAndDeclareConstLit (tokenno, MakeKey ('0'), ZType) ;
         IF IsGreaterOrEqualConversion (TokenToLocation (tokenno), zero, des, expr)
         THEN
            MetaErrorT2 (tokenNo,
                         'the divisor {%2Wa} in this division expression is less than or equal to zero, this will cause an exception to be raised before the result is assigned to the designator {%1a}',
                         des, expr) ;
            PutQuad (q, ErrorOp, NulSym, NulSym, r)
         END
      END
   END
END FoldNonPosDiv ;


(*
   FoldNonPosMod - attempts to fold the bound checking for a modulus expression.
*)

PROCEDURE FoldNonPosMod (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p   : Range ;
   zero: CARDINAL ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF GccKnowsAbout(expr) AND IsConst(expr)
      THEN
         zero := MakeAndDeclareConstLit (tokenno, MakeKey('0'), ZType) ;
         IF IsGreaterOrEqualConversion (TokenToLocation(tokenno), zero, des, expr)
         THEN
            MetaErrorT2 (tokenNo,
                         'the divisor {%2Wa} in this modulus expression is less than or equal to zero, this will cause an exception to be raised before the result is assigned to the designator {%1a}',
                         des, expr) ;
            PutQuad (q, ErrorOp, NulSym, NulSym, r)
         END
      END
   END
END FoldNonPosMod ;


(*
   FoldZeroDiv -
*)

PROCEDURE FoldZeroDiv (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p   : Range ;
   zero: CARDINAL ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF GccKnowsAbout(expr) AND IsConst(expr)
      THEN
         zero := MakeAndDeclareConstLit (tokenno, MakeKey ('0'), ZType) ;
         IF IsEqualConversion (zero, des, expr)
         THEN
            MetaErrorT2 (tokenNo,
                        'the divisor {%2Wa} in this division expression is equal to zero, this will cause an exception to be raised before the result is assigned to the designator {%1a}',
                         des, expr) ;
            PutQuad (q, ErrorOp, NulSym, NulSym, r)
         END
      END
   END
END FoldZeroDiv ;


(*
   FoldZeroRem -
*)

PROCEDURE FoldZeroRem (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p   : Range ;
   zero: CARDINAL ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF GccKnowsAbout(expr) AND IsConst(expr)
      THEN
         zero := MakeAndDeclareConstLit (tokenno, MakeKey ('0'), ZType) ;
         IF IsEqualConversion (zero, des, expr)
         THEN
            MetaErrorT2 (tokenNo,
                        'the divisor {%2Wa} in this remainder expression is equal to zero, this will cause an exception to be raised before the result is assigned to the designator {%1a}',
                         des, expr) ;
            PutQuad (q, ErrorOp, NulSym, NulSym, r)
         END
      END
   END
END FoldZeroRem ;


(*
   FoldRangeCheck - attempts to resolve the range check, r.
                    If it evaluates to true then
                       it is replaced by an ErrorOp
                    elsif it evaluates to false then
                       it is removed
                    else
                       it is left alone
*)

PROCEDURE FoldRangeCheck (tokenno: CARDINAL; q: CARDINAL; r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           :  FoldAssignment(tokenno, q, r) |
      returnassignment     :  FoldReturn(tokenno, q, r) |
(*      subrangeassignment   :  |  unused currently *)
      inc                  :  FoldInc(tokenno, q, r) |
      dec                  :  FoldDec(tokenno, q, r) |
      incl                 :  FoldIncl(tokenno, q, r) |
      excl                 :  FoldExcl(tokenno, q, r) |
      shift                :  FoldShift(tokenno, q, r) |
      rotate               :  FoldRotate(tokenno, q, r) |
      typeassign           :  FoldTypeCheck(tokenno, q, r) |
      typeparam            :  FoldTypeCheck(tokenno, q, r) |
      typeexpr             :  FoldTypeCheck(tokenno, q, r) |
      paramassign          :  FoldParameterAssign(tokenno, q, r) |
      staticarraysubscript :  FoldStaticArraySubscript(tokenno, q, r) |
      dynamicarraysubscript:  FoldDynamicArraySubscript(tokenno, q, r) |
      forloopbegin         :  FoldForLoopBegin(tokenno, q, r) |
      forloopto            :  FoldForLoopTo(tokenno, q, r) |
      forloopend           :  RETURN (* unable to fold anything at this point, des, will be variable *) |
      pointernil           :  FoldNil(tokenno, q, r) |
      noreturn             :  RETURN (* nothing to fold *) |
      noelse               :  RETURN (* nothing to fold *) |
      casebounds           :  FoldCaseBounds(tokenno, q, r) |
      wholenonposdiv       :  FoldNonPosDiv(tokenno, q, r) |
      wholenonposmod       :  FoldNonPosMod(tokenno, q, r) |
      wholezerodiv         :  FoldZeroDiv(tokenno, q, r) |
      wholezerorem         :  FoldZeroRem(tokenno, q, r) |
      none                 :  SubQuad(q)

      ELSE
         InternalError ('unexpected case')
      END
   END
END FoldRangeCheck ;


(*
   DeReferenceLValue - returns a Tree which is either ModGcc(expr)
                       or Mod2Gcc ( *expr) depending whether, expr,
                       is an LValue.
*)

PROCEDURE DeReferenceLValue (tokenno: CARDINAL; expr: CARDINAL) : Tree ;
VAR
   e       : Tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   e := Mod2Gcc(expr) ;
   IF GetMode(expr)=LeftValue
   THEN
      e := BuildIndirect(location, e, Mod2Gcc(GetType(expr)))
   END ;
   RETURN( e )
END DeReferenceLValue ;


(*
   BuildStringParam - builds a C style string parameter which will be passed
                      as an ADDRESS type.
*)

PROCEDURE BuildStringParam (tokenno: CARDINAL; s: String) ;
BEGIN
   BuildStringParamLoc (TokenToLocation(tokenno), s)
END BuildStringParam ;


(*
   BuildStringParamLoc - builds a C style string parameter which will be passed
                         as an ADDRESS type.
*)

PROCEDURE BuildStringParamLoc (location: location_t; s: String) ;
BEGIN
   BuildParam (location,
               BuildConvert (location, Mod2Gcc (Address),
                             BuildAddr (location, BuildStringConstant (string(s), Length(s)),
                                        FALSE), FALSE))
END BuildStringParamLoc ;


(*
   CodeErrorCheck - returns a Tree calling the approprate exception handler.
*)

PROCEDURE CodeErrorCheck (r: CARDINAL; function, message: String) : Tree ;
VAR
   filename: String ;
   line,
   column  : CARDINAL ;
   p       : Range ;
   f       : Tree ;
   location: location_t ;
BEGIN
   IF HandlerExists (r)
   THEN
      IF message = NIL
      THEN
         message := GetRangeErrorMessage (r)
      END ;
      message := FillInParameters (r, message) ;
      p := GetIndice (RangeIndex, r) ;
      WITH p^ DO
         filename := FindFileNameFromToken (tokenNo, 0) ;
         line := TokenToLineNo (tokenNo, 0) ;
         column := TokenToColumnNo (tokenNo, 0) ;
         location := TokenToLocation (tokenNo) ;
         f := Mod2Gcc (lookupExceptionHandler (type)) ;
         BuildStringParam (tokenNo, message) ;
         BuildStringParam (tokenNo, function) ;
         BuildParam (location, BuildIntegerConstant (column)) ;
         BuildParam (location, BuildIntegerConstant (line)) ;
         BuildStringParam (tokenNo, filename) ;
         RETURN BuildProcedureCallTree (location, f, NIL)
      END
   ELSE
      RETURN NIL
   END
END CodeErrorCheck ;


(*
   IssueWarning - issue a warning.  The compiler knows that this basic block can be reached
                  and we are in scope, function.
*)

PROCEDURE IssueWarning (function: String; r: CARDINAL) ;
VAR
   p: Range ;
   s: String ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           : s := InitString('if the assignment is ever executed then the designator {%1Wa} will exceed the type range {%1ts:of {%1ts}}') |
      returnassignment     : s := InitString('if the value {%2Wa} is returned from procedure function {%1Wa} then it will exceed the type range {%1ts:of {%1ts}}') |
      subrangeassignment   : InternalError ('not expecting this case value') |
      inc                  : s := InitString('if the INC is ever executed the expression {%2Wa} will cause an overflow error for the designator {%1a} as it exceeds the type range {%1ts:of {%1ts}}') |
      dec                  : s := InitString('if the DEC is ever executed the expression {%2Wa} will cause an underflow error for the designator {%1a} as it exceeds the type range {%1ts:of {%1ts}}') |
      incl                 : s := InitString('the expression {%2Wa} given in the INCL exceeds the type range {%1ts} of the designator {%1a}') |
      excl                 : s := InitString('the expression {%2Wa} given in the EXCL exceeds the type range {%1ts} of the designator {%1a}') |
      shift                : s := InitString('the expression {%2Wa} given in the second parameter to SHIFT exceeds the type range {%1ts} of the first parameter {%1a}') |
      rotate               : s := InitString('the expression {%2Wa} given in the second parameter to ROTATE exceeds the type range {%1ts} of the first parameter {%1a}') |
      typeassign           : s := InitString('') |
      typeparam            : s := InitString('') |
      typeexpr             : s := InitString('') |
      paramassign          : s := InitString('if this call is executed then the actual parameter {%2Wa} will be out of range of the {%3N} formal parameter {%1a}') |
      staticarraysubscript : s := InitString('if this access to the static array {%1Wa:{%2a:{%1a}[{%2a}]}} is ever made then the index will be out of bounds in the {%3N} array subscript') |
      dynamicarraysubscript: s := InitString('if this access to the dynamic array {%1Wa:{%2a:{%1a}[{%2a}]}} is ever made then the index will be out of bounds in the {%3N} array subscript') |
      forloopbegin         : s := InitString('if the assignment in this FOR loop is ever executed then the designator {%1Wa} will be exceed the type range {%1ts:of {%1ts}}') |
      forloopto            : s := InitString('the final value {%2Wa} in this FOR loop will be out of bounds {%1ts:of type {%1ts}} if ever executed') |
      forloopend           : s := InitString('the FOR loop will cause the designator {%1Wa} to be out of bounds when the BY value {%2a} is added') |
      pointernil           : s := InitString('if this pointer value {%1Wa} is ever dereferenced it will cause an exception') |
      noreturn             : s := InitString('{%1W:}this function will exit without executing a RETURN statement') |
      noelse               : s := InitString('{%1W:}this CASE statement does not have an ELSE statement') |
      casebounds           : s := InitString('{%1W:}this CASE statement has overlapping ranges') |
      wholenonposdiv       : s := InitString('this division expression {%2Wa} will cause an exception as this divisor is less than or equal to zero') |
      wholenonposmod       : s := InitString('this modulus expression {%2Wa} will cause an exception as this divisor is less than or equal to zero') |
      wholezerodiv         : s := InitString('this division expression {%2Wa} will cause an exception as the divisor is zero') |
      wholezerorem         : s := InitString('this remainder expression {%2Wa} will cause an exception as the divisor is zero') |
      none                 : InternalError ('unexpected value')

      ELSE
         InternalError ('enumeration value unknown')
      END ;
      s := ConCat (s, Mark (InitString (' in ('))) ;
      s := ConCat (s, function) ;
      s := ConCatChar (s, ')') ;
      MetaErrorStringT3 (tokenNo, s, des, expr, dimension) ;
      (* FlushErrors *)
   END
END IssueWarning ;


(*
   CodeErrorCheckLoc - generate a runtime error message positioned at location
                       and in function.  If function is NIL then the error scope
                       is used.
*)

PROCEDURE CodeErrorCheckLoc (location: location_t;
                             function, message: ADDRESS; func: CARDINAL) : Tree ;
VAR
   scope,
   errorMessage: String ;
   t           : Tree ;
   filename    : String ;
   line,
   column      : CARDINAL ;
BEGIN
   IF func = NulSym
   THEN
      RETURN NIL
   ELSE
      t := Mod2Gcc (func) ;
      IF t # NIL
      THEN
         filename := InitStringCharStar (GetFilenameFromLocation (location)) ;
         Assert (message # NIL) ;
         errorMessage := InitStringCharStar (message) ;
         column := GetColumnNoFromLocation (location) ;
         line := GetLineNoFromLocation (location) ;
         BuildStringParamLoc (location, errorMessage) ;
         IF function = NIL
         THEN
            scope := GetAnnounceScope (filename, NIL)
         ELSE
            scope := quoteOpen (InitString ('')) ;
            scope := ConCat (scope, Mark (InitStringCharStar (function))) ;
            scope := ConCat (InitString ("procedure "), quoteClose (scope))
         END ;
         BuildStringParamLoc (location, scope) ;
         BuildParam (location, BuildIntegerConstant (column)) ;
         BuildParam (location, BuildIntegerConstant (line)) ;
         BuildStringParamLoc (location, filename) ;
         t := BuildProcedureCallTree (location, t, NIL) ;
         (*
         filename := KillString (filename) ;
         scope := KillString (scope) ;
         errorMessage := KillString (errorMessage)
         *)
      END ;
      RETURN t
   END
END CodeErrorCheckLoc ;


(*
   IssueWarningLoc -
*)

PROCEDURE IssueWarningLoc (location: location_t; message: ADDRESS) ;
VAR
   s: String ;
BEGIN
   s := InitString ("numerical overflow detected when performing ") ;
   s := ConCat (s, Mark (InitStringCharStar (message))) ;
   ErrorAt (location, string (s)) ;
   s := KillString (s)
END IssueWarningLoc ;


(*
   BuildIfCallWholeHandlerLoc - return a Tree containing a runtime test whether, condition, is true.
*)

PROCEDURE BuildIfCallWholeHandlerLoc (location: location_t; condition: Tree;
                                      scope, message: ADDRESS) : Tree ;
BEGIN
   RETURN BuildIfCallHandlerLoc (location, condition, scope, message, ExceptionWholeValue)
END BuildIfCallWholeHandlerLoc ;


(*
   BuildIfCallRealHandlerLoc - return a Tree containing a runtime test whether, condition, is true.
*)

PROCEDURE BuildIfCallRealHandlerLoc (location: location_t; condition: Tree;
                                     scope, message: ADDRESS) : Tree ;
BEGIN
   RETURN BuildIfCallHandlerLoc (location, condition, scope, message, ExceptionRealValue)
END BuildIfCallRealHandlerLoc ;


(*
   BuildIfCallHandlerLoc - return a Tree containing a runtime test whether, condition, is true.
*)

PROCEDURE BuildIfCallHandlerLoc (location: location_t; condition: Tree;
                                 scope, message: ADDRESS; func: CARDINAL) : Tree ;
BEGIN
   IF IsTrue (condition)
   THEN
      IssueWarningLoc (location, message)
   END ;
   RETURN BuildIfThenDoEnd (condition, CodeErrorCheckLoc (location, scope, message, func))
END BuildIfCallHandlerLoc ;


(*
   BuildIfCallHandler -
*)

PROCEDURE BuildIfCallHandler (condition: Tree; r: CARDINAL;
                              function, message: String; warning: BOOLEAN) : Tree ;
BEGIN
   IF warning AND IsTrue (condition)
   THEN
      IssueWarning (function, r)
   END ;
   RETURN BuildIfThenDoEnd (condition, CodeErrorCheck (r, function, message))
END BuildIfCallHandler ;


(*
   RangeCheckReal -
*)

PROCEDURE RangeCheckReal (p: Range; r: CARDINAL; function, message: String) ;
VAR
   e,
   condition: Tree ;
   location : location_t ;
BEGIN
   WITH p^ DO
      location := TokenToLocation (tokenNo) ;
      e := DeReferenceLValue (tokenNo, expr) ;
      condition := BuildEqualTo (location,
                                 BuiltInIsfinite (location, e),
                                 GetIntegerZero (location)) ;
      AddStatement (location, BuildIfCallHandler (condition, r, function, message, TRUE)) ;
   END
END RangeCheckReal ;


(*
   RangeCheckOrdinal -
*)

PROCEDURE RangeCheckOrdinal (p: Range; r: CARDINAL; function, message: String) ;
VAR
   condition,
   desMin, desMax,
   exprMin, exprMax: Tree ;
   location        : location_t ;
BEGIN
   WITH p^ DO
      location := TokenToLocation(tokenNo) ;
      IF GetMinMax(tokenNo, exprLowestType, exprMin, exprMax) AND
         GetMinMax(tokenNo, desLowestType, desMin, desMax)
      THEN
         IF OverlapsRange(desMin, desMax, exprMin, exprMax)
         THEN
            IF IsGreater(desMin, exprMin)
            THEN
               condition := BuildLessThan(location, DeReferenceLValue(tokenNo, expr), BuildConvert(location, Mod2Gcc(exprLowestType), desMin, FALSE)) ;
               AddStatement(location, BuildIfCallHandler(condition, r, function, message, TRUE))
            END ;
            IF IsGreater(exprMax, desMax)
            THEN
               condition := BuildGreaterThan(location, DeReferenceLValue(tokenNo, expr), BuildConvert(location, Mod2Gcc(exprLowestType), desMax, FALSE)) ;
               AddStatement(location, BuildIfCallHandler(condition, r, function, message, TRUE))
            END
         ELSE
            MetaErrorStringT3 (tokenNo, message, des, expr, paramNo)
         END
      END
   END
END RangeCheckOrdinal ;


(*
   DoCodeAssignmentExprType -
*)

PROCEDURE DoCodeAssignmentExprType (p: Range;
                                    r: CARDINAL; function, message: String) ;
BEGIN
   WITH p^ DO
      IF GccKnowsAbout(desLowestType) AND
         GccKnowsAbout(exprLowestType)
      THEN
         IF IsRealType(desLowestType) AND IsRealType(exprLowestType)
         THEN
            RangeCheckReal (p, r, function, message)
         ELSE
            RangeCheckOrdinal (p, r, function, message)
         END
      ELSE
         InternalError ('should have resolved these types')
      END
   END
END DoCodeAssignmentExprType ;


(*
   DoCodeAssignmentWithoutExprType -
*)

PROCEDURE DoCodeAssignmentWithoutExprType (p: Range;
                                           r: CARDINAL; function, message: String) ;
VAR
   condition,
   desMin, desMax: Tree ;
   location      : location_t ;
BEGIN
   WITH p^ DO
      location := TokenToLocation(tokenNo) ;
      IF GccKnowsAbout(desLowestType)
      THEN
         IF GetMinMax(tokenNo, desLowestType, desMin, desMax)
         THEN
            condition := BuildLessThan(location,
                                       BuildConvert(location, Mod2Gcc(desLowestType),
                                                    DeReferenceLValue(tokenNo, expr), FALSE),
                                       desMin) ;
            AddStatement(location, BuildIfCallHandler(condition, r, function, message, TRUE)) ;
            condition := BuildGreaterThan(location,
                                          BuildConvert(location, Mod2Gcc(desLowestType),
                                                       DeReferenceLValue(tokenNo, expr), FALSE),
                                          desMax) ;
            AddStatement(location, BuildIfCallHandler(condition, r, function, message, TRUE))
         END
      ELSE
         InternalError ('should have resolved this type')
      END
   END
END DoCodeAssignmentWithoutExprType ;


(*
   DoCodeAssignment -
*)

PROCEDURE DoCodeAssignment (tokenno: CARDINAL; r: CARDINAL;
                            function, message: String) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenNo, des) ;
      TryDeclareConstant(tokenNo, expr) ;
      DeclareConstructor(tokenno, 0, expr) ;
      IF desLowestType#NulSym
      THEN
         Assert(GccKnowsAbout(expr)) ;
         IF exprLowestType=NulSym
         THEN
            DoCodeAssignmentWithoutExprType (p, r, function, message)
         ELSE
            DoCodeAssignmentExprType (p, r, function, message)
         END
      END
   END
END DoCodeAssignment ;


(*
   CodeAssignment -
*)

PROCEDURE CodeAssignment (tokenno: CARDINAL;
                          r: CARDINAL; function, message: String) ;
BEGIN
   DoCodeAssignment (tokenno, r, function, message)
END CodeAssignment ;


(*
   CodeParameterAssign -
*)

PROCEDURE CodeParameterAssign (tokenno: CARDINAL;
                               r: CARDINAL; function, message: String) ;
BEGIN
   DoCodeAssignment (tokenno, r, function, message)
END CodeParameterAssign ;


(*
   CodeReturn -
*)

PROCEDURE CodeReturn (tokenno: CARDINAL;
                      r: CARDINAL; function, message: String) ;
BEGIN
   DoCodeAssignment (tokenno, r, function, message)
END CodeReturn ;


(*
   IfOutsideLimitsDo -
*)

PROCEDURE IfOutsideLimitsDo (tokenno: CARDINAL; min, expr, max: Tree; r: CARDINAL;
                             function, message: String) ;
VAR
   condition: Tree ;
   location : location_t ;
BEGIN
   location := TokenToLocation (tokenno) ;
   condition := BuildGreaterThan (location, min, expr) ;
   AddStatement (location, BuildIfThenDoEnd (condition, CodeErrorCheck (r, function, message))) ;
   condition := BuildLessThan (location, max, expr) ;
   AddStatement (location, BuildIfThenDoEnd (condition, CodeErrorCheck (r, function, message)))
END IfOutsideLimitsDo ;


(*
   CodeInc -
*)

PROCEDURE CodeInc (tokenno: CARDINAL;
                   r: CARDINAL; function, message: String) ;
VAR
   p             : Range ;
   t, condition,
   e,
   desMin, desMax: Tree ;
   location      : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenNo, des) ;
      TryDeclareConstant(tokenNo, expr) ;
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND GccKnowsAbout(desLowestType)
         THEN
            IF GetMinMax(tokenno, desLowestType, desMin, desMax)
            THEN
               e := BuildConvert(location, GetTreeType(desMin), DeReferenceLValue(tokenno, expr), FALSE) ;
               IfOutsideLimitsDo(tokenNo,
                                 BuildConvert(location, GetTreeType(desMin), GetIntegerZero(location), FALSE),
                                 e, desMax, r, function, message) ;
               t := BuildSub(location,
                             desMax,
                             BuildConvert(location, Mod2Gcc(desLowestType), e, FALSE),
                             FALSE) ;
               condition := BuildGreaterThan(location, Mod2Gcc(des), t) ;
               AddStatement (location, BuildIfThenDoEnd (condition, CodeErrorCheck (r, function, message)))
            END
         ELSE
            InternalError ('should have resolved these types')
         END
      END
   END
END CodeInc ;


(*
   CodeDec -
*)

PROCEDURE CodeDec (tokenno: CARDINAL;
                   r: CARDINAL; function, message: String) ;
VAR
   p             : Range ;
   t, condition,
   e,
   desMin, desMax: Tree ;
   location      : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenNo, des) ;
      TryDeclareConstant(tokenNo, expr) ;
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND GccKnowsAbout(desLowestType)
         THEN
            IF GetMinMax(tokenno, desLowestType, desMin, desMax)
            THEN
               e := BuildConvert(location, GetTreeType(desMin), DeReferenceLValue(tokenno, expr), FALSE) ;
               IfOutsideLimitsDo(tokenNo,
                                 BuildConvert(location, GetTreeType(desMin), GetIntegerZero(location), FALSE),
                                 e, desMax, r, function, message) ;
               t := BuildSub(location, BuildConvert(location, Mod2Gcc(desLowestType), e, FALSE),
                             desMin,
                             FALSE) ;
               condition := BuildLessThan(location, Mod2Gcc(des), t) ;
               AddStatement (location, BuildIfThenDoEnd (condition, CodeErrorCheck (r, function, message)))
            END
         ELSE
            InternalError ('should have resolved these types')
         END
      END
   END
END CodeDec ;


(*
   CodeInclExcl -
*)

PROCEDURE CodeInclExcl (tokenno: CARDINAL;
                        r: CARDINAL; function, message: String) ;
VAR
   p             : Range ;
   e,
   desMin, desMax: Tree ;
   location      : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenNo, des) ;
      TryDeclareConstant(tokenNo, expr) ;
      desLowestType := SkipType(GetType(des)) ;
      IF desLowestType#NulSym
      THEN
         IF GccKnowsAbout(expr) AND GccKnowsAbout(desLowestType)
         THEN
            IF GetMinMax(tokenno, desLowestType, desMin, desMax)
            THEN
               e := BuildConvert(location, GetTreeType(desMin), DeReferenceLValue(tokenno, expr), FALSE) ;
               IfOutsideLimitsDo(tokenNo, desMin, e, desMax, r, function, message)
(*  this should not be used for incl/excl as des is a set type
               t := BuildSub(location,
                             desMax,
                             BuildConvert(location, Mod2Gcc(desLowestType), e, FALSE),
                             FALSE) ;
               condition := BuildGreaterThan(Mod2Gcc(des), t) ;
               AddStatement(location, BuildIfThenDoEnd(condition, CodeErrorCheck(r, function, message)))
*)
            END
         ELSE
            InternalError ('should have resolved these types')
         END
      END
   END
END CodeInclExcl ;


(*
   CodeShiftRotate - ensure that the bit shift is within the range
                     -(MAX(set)-MIN(set)+1)..(MAX(set)-MIN(set)+1)
*)

PROCEDURE CodeShiftRotate (tokenno: CARDINAL;
                           r: CARDINAL; function, message: String) ;
VAR
   ofType            : CARDINAL ;
   p                 : Range ;
   e,
   shiftMin, shiftMax,
   desMin, desMax    : Tree ;
   location          : location_t ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenNo, des) ;
      TryDeclareConstant(tokenNo, expr) ;
      desLowestType := SkipType(GetType(des)) ;
      IF desLowestType#NulSym
      THEN
         ofType := SkipType(GetType(desLowestType)) ;
         IF GccKnowsAbout(expr) AND GccKnowsAbout(ofType)
         THEN
            IF GetMinMax(tokenno, ofType, desMin, desMax)
            THEN
               location := TokenToLocation(tokenNo) ;
               desMin := BuildConvert(location, GetIntegerType(), desMin, FALSE) ;
               desMax := BuildConvert(location, GetIntegerType(), desMax, FALSE) ;
               shiftMax := BuildAdd(location,
                                    BuildSub(location, desMax, desMin, FALSE),
                                    GetIntegerOne(location),
                                    FALSE) ;
               shiftMin := BuildNegate(location, shiftMax, FALSE) ;
               e := BuildConvert(location, GetIntegerType(), DeReferenceLValue(tokenno, expr), FALSE) ;
               IfOutsideLimitsDo(tokenNo, shiftMin, e, shiftMax, r, function, message)
            END
         ELSE
            InternalError ('should have resolved these types')
         END
      END
   END
END CodeShiftRotate ;


(*
   CodeStaticArraySubscript -
*)

PROCEDURE CodeStaticArraySubscript (tokenno: CARDINAL;
                                    r: CARDINAL; function, message: String) ;
VAR
   p             : Range ;
   desMin, desMax: Tree ;
   location      : location_t ;
BEGIN
   location := TokenToLocation (tokenno) ;
   p := GetIndice (RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant (tokenNo, expr) ;
      IF GccKnowsAbout (expr) AND GccKnowsAbout (desLowestType)
      THEN
         IF GetMinMax (tokenno, desLowestType, desMin, desMax)
         THEN
            IfOutsideLimitsDo (tokenno, desMin,
                               BuildConvert (location, GetTreeType (desMin), DeReferenceLValue (tokenno, expr), FALSE),
                               desMax, r, function, message)
         ELSE
            InternalError  ('should have resolved the bounds of the static array')
         END
      ELSE
         InternalError  ('should have resolved these types')
      END
   END
END CodeStaticArraySubscript ;


(*
   CodeDynamicArraySubscript -
*)

PROCEDURE CodeDynamicArraySubscript (tokenno: CARDINAL;
                                     r: CARDINAL; function, message: String) ;
VAR
   UnboundedType: CARDINAL ;
   p            : Range ;
   high, e      : Tree ;
   location     : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenNo, expr) ;
      Assert(IsVar(des)) ;
      IF GccKnowsAbout(expr) AND GccKnowsAbout(des)
      THEN
         UnboundedType := GetType(des) ;
         Assert(IsUnbounded(UnboundedType)) ;
         high := BuildConvert(location, GetIntegerType(), GetHighFromUnbounded(location, dimension, des), FALSE) ;
         e := BuildConvert(location, GetIntegerType(), DeReferenceLValue(tokenno, expr), FALSE) ;
         IfOutsideLimitsDo(tokenNo, GetIntegerZero(location), e, high, r, function, message)
      ELSE
         InternalError ('should have resolved these types')
      END
   END
END CodeDynamicArraySubscript ;


(*
   CodeForLoopBegin -
*)

PROCEDURE CodeForLoopBegin (tokenno: CARDINAL;
                            r: CARDINAL; function, message: String) ;
BEGIN
   DoCodeAssignment(tokenno, r, function, message)
END CodeForLoopBegin ;


(*
   CodeForLoopTo -
*)

PROCEDURE CodeForLoopTo (tokenno: CARDINAL;
                         r: CARDINAL; function, message: String) ;
BEGIN
   DoCodeAssignment(tokenno, r, function, message)
END CodeForLoopTo ;


(*
   Pseudo template code for CodeLoopEnd:

   PROCEDURE CheckCardinalInteger (des: CARDINAL; inc: INTEGER) ;
   VAR
      room,
      lg  : CARDINAL ;
   BEGIN
      IF inc>=0
      THEN
         IF des>=0
         THEN
            lg := VAL(CARDINAL, inc) ;
            room := MAX(CARDINAL)-des ;
            IF lg>room
            THEN
               printf("increment exceeds range at end of FOR loop\n") ;
               exit (2)
            END
         ELSE
            (* inc can never cause an overflow given its type *)
         END
      ELSE
         (* inc < 0 *)
         IF des>VAL(CARDINAL, MAX(INTEGER))
         THEN
            (* inc can never cause an underflow given its range *)
         ELSE
            (* des <= MAX(INTEGER) *)
            IF des=MIN(INTEGER)
            THEN
               printf("increment exceeds range at end of FOR loop\n") ;
               exit (4)
            ELSE
               IF inc=MIN(INTEGER)
               THEN
                  IF des=0
                  THEN
                     printf("increment exceeds range at end of FOR loop\n") ;
                     exit (5)
                  END
               ELSE
                  lg := VAL(CARDINAL, -inc) ;
                  IF lg>des
                  THEN
                     printf("increment exceeds range at end of FOR loop\n") ;
                     exit (5)
                  END
               END
            END
         END
      END
   END CheckCardinalInteger ;


   PROCEDURE CheckCardinalCardinal (des: CARDINAL; inc: CARDINAL) ;
   BEGIN
      IF MAX(CARDINAL)-des<inc
      THEN
         printf("increment exceeds range at end of FOR loop\n") ;
         exit (2)
      END
   END CheckCardinalCardinal ;
*)


(*
   SameTypesCodeForLoopEnd - the trivial case.
*)

PROCEDURE SameTypesCodeForLoopEnd (tokenNo: CARDINAL; r: CARDINAL; function, message: String;
                                   p: Range; dmax: Tree) ;
VAR
   inc,
   room,
   statement,
   condition: Tree ;
   location : location_t ;
BEGIN
   location := TokenToLocation(tokenNo) ;
   WITH p^ DO
      inc := DeReferenceLValue(tokenNo, expr) ;
      room := BuildSub(location, dmax, Mod2Gcc(des), FALSE) ;
      condition := BuildLessThan(location, room, inc) ;
      statement := BuildIfCallHandler(condition, r, function, message, IsTrue(condition)) ;
      AddStatement(location, statement)
   END
END SameTypesCodeForLoopEnd ;


(*
   DiffTypesSameForLoopEnd - remember that lowestType will map onto an int, or unsigned int
                             of appropriate size.
*)

PROCEDURE DiffTypesCodeForLoopEnd (tokenNo: CARDINAL; r: CARDINAL; function, message: String;
                                   p: Range; dmax, emin, emax: Tree) ;
VAR
   location  : location_t ;
   desoftypee,
   inc,
   room,
   c1, c2, c3,
   c4, c5, c6,
   c7, c8,
   s1, s2, s3,
   s4, s5, s6,
   s7, s8,
   lg1, lg2,
   dz, ez    : Tree ;
BEGIN
   location := TokenToLocation(tokenNo) ;
   WITH p^ DO
      inc := DeReferenceLValue(tokenNo, expr) ;
      ez := BuildConvert(location, Mod2Gcc(exprLowestType), GetIntegerZero(location), FALSE) ;
      dz := BuildConvert(location, Mod2Gcc(desLowestType), GetIntegerZero(location), FALSE) ;

      c1 := BuildGreaterThanOrEqual(location, inc, ez) ;
      (* if (inc >= 0)                                           [c1] *)
      c2 := BuildGreaterThanOrEqual(location, Mod2Gcc(des), dz) ;
      (*    if (des >= 0)                                        [c2] *)
      lg1 := BuildConvert(location, Mod2Gcc(desLowestType), inc, FALSE) ;
      room := BuildSub(location, dmax, Mod2Gcc(des), FALSE) ;
      c3 := BuildGreaterThan(location, lg1, room) ;           (* [c3]  *)
      (* WarnIf(IsTrue(c1) AND IsTrue(c2) AND IsTrue(c3), function, message) ; --implement me-- *)

      s3 := BuildIfCallHandler(c3, r, function, message, FALSE) ;
      s2 := BuildIfThenDoEnd(c2, s3) ;

      (* else *)
      (*    (* inc < 0 *)                                        [s4]  *)
      (*    if (des <= val(desLowestType, emax)                  [c4]  *)
      c4 := BuildLessThanOrEqual(location, Mod2Gcc(des), BuildConvert(location, Mod2Gcc(desLowestType), emax, FALSE)) ;
      (*    (* des <= MAX(exprLowestType) *) *)
      desoftypee := BuildConvert(location, Mod2Gcc(exprLowestType), Mod2Gcc(des), FALSE) ;
      c5 := BuildEqualTo(location, desoftypee, emin) ;        (* [c5]  *)
      s5 := BuildIfCallHandler(c5, r, function, message, FALSE) ;
      (*       if des = emin  *)
      (*          error                                          [s5]  *)
      (*       end                                                     *)
      c6 := BuildEqualTo(location, inc, emin) ;              (*  [c6]  *)
      (*      if inc = emin                                            *)
      (*         if des = 0                                      [c7]  *)
      c7 := BuildEqualTo(location, Mod2Gcc(des), dz) ;
      s7 := BuildIfCallHandler(c7, r, function, message, FALSE) ;

      (*         end                                                   *)
      (*      else                                                     *)
      (*         lg2 = VAL(desLowestType, -inc)                  [s8]  *)
      lg2 := BuildConvert(location, Mod2Gcc(desLowestType), BuildNegate(location, inc, FALSE), FALSE) ;
      (*         if lg2 > des                                          *)
      (*             error                                             *)
      c8 := BuildGreaterThan(location, lg2, Mod2Gcc(des)) ;
      s8 := BuildIfCallHandler(c8, r, function, message, FALSE) ;
      (*             end                                               *)
      (*          end                                                  *)
      (*       end                                                     *)
      (*    end                                                        *)
      (* end                                                           *)
   END ;

   s6 := BuildIfThenElseEnd(c6, s7, s8) ;
   s4 := BuildIfThenElseEnd(c4, s5, s6) ;
   s1 := BuildIfThenElseEnd(c1, s2, s4) ;
   AddStatement(location, s1)

END DiffTypesCodeForLoopEnd ;


(*
   CodeForLoopEnd - checks to see that des := des + expr does not overflow.
                    This is called at the end of the for loop.  It is more complex
                    than it initially seems as des and expr might be different types.
*)

PROCEDURE CodeForLoopEnd (tokenno: CARDINAL;
                          r: CARDINAL; function, message: String) ;
VAR
   isCard    : BOOLEAN ;
   p         : Range ;
   dmin, dmax,
   emin, emax: Tree ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenno, des) ;   (* use quad tokenno, rather than the range tokenNo *)
      TryDeclareConstant(tokenno, expr) ;  (* use quad tokenno, rather than the range tokenNo *)
      IF desLowestType#NulSym
      THEN
         Assert(GccKnowsAbout(expr)) ;
         IF GccKnowsAbout(desLowestType) AND
            GetMinMax(tokenno, desLowestType, dmin, dmax) AND
            GccKnowsAbout(exprLowestType) AND
            GetMinMax(tokenno, exprLowestType, emin, emax)
         THEN
            PushIntegerTree(dmin) ;
            PushInt(0) ;
            isCard := GreEqu(tokenno) ;
            IF (desLowestType=exprLowestType) AND isCard
            THEN
               SameTypesCodeForLoopEnd(tokenno, r, function, message, p, dmax)
            ELSE
               DiffTypesCodeForLoopEnd(tokenno, r, function, message, p, dmax, emin, emax)
            END
         END
      END
   END
END CodeForLoopEnd ;


(*
   CodeNil -
*)

PROCEDURE CodeNil (r: CARDINAL; function, message: String) ;
VAR
   p           : Range ;
   condition, t: Tree ;
   location    : location_t ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenNo, des) ;
(*
      IF GetMode(des)=LeftValue
      THEN
         (* t := BuildIndirect(Mod2Gcc(des), Mod2Gcc(GetType(des))) *)
      ELSE
         t := Mod2Gcc(des)
      END ;
*)
      t := Mod2Gcc(des) ;
      location := TokenToLocation(tokenNo) ;
      condition := BuildEqualTo(location, BuildConvert(location, GetPointerType(), t, FALSE), GetPointerZero(location)) ;
      AddStatement(location, BuildIfCallHandler(condition, r, function, message, TRUE))
   END
END CodeNil ;


(*
   CodeWholeNonPos - generates range check code for expr<=0.
*)

PROCEDURE CodeWholeNonPos (tokenno: CARDINAL;
                           r: CARDINAL; function, message: String) ;
VAR
   zero     : CARDINAL ;
   p        : Range ;
   condition,
   e        : Tree ;
   location : location_t ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant (tokenNo, expr) ;
      IF GccKnowsAbout (expr)
      THEN
         location := TokenToLocation (tokenno) ;
         e := ZConstToTypedConst (LValueToGenericPtr(location, expr), expr, des) ;
         zero := MakeAndDeclareConstLit (tokenno, MakeKey('0'), ZType) ;
         condition := BuildLessThanOrEqual (location, e, Mod2Gcc (zero)) ;
         AddStatement (location, BuildIfThenDoEnd (condition, CodeErrorCheck (r, function, message)))
      ELSE
         InternalError ('should have resolved expr')
      END
   END
END CodeWholeNonPos ;


(*
   CodeWholeZero - generates range check code for expr=0.
*)

PROCEDURE CodeWholeZero (tokenno: CARDINAL;
                         r: CARDINAL; function, message: String) ;
VAR
   zero     : CARDINAL ;
   p        : Range ;
   condition,
   e        : Tree ;
   location : location_t ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      TryDeclareConstant(tokenNo, expr) ;
      IF GccKnowsAbout(expr)
      THEN
         location := TokenToLocation(tokenno) ;
         e := ZConstToTypedConst(LValueToGenericPtr(location, expr), expr, des) ;
         zero := MakeAndDeclareConstLit (tokenno, MakeKey('0'), ZType) ;
         condition := BuildEqualTo(location,
                                   e, BuildConvert(location, GetTreeType(e), Mod2Gcc(zero), FALSE)) ;
         AddStatement(location, BuildIfThenDoEnd(condition, CodeErrorCheck(r, function, message)))
      ELSE
         InternalError ('should have resolved expr')
      END
   END
END CodeWholeZero ;


(*
   InitCaseBounds - creates a case bound range check.
*)

PROCEDURE InitCaseBounds (b: CARDINAL) : CARDINAL ;
VAR
   p: Range ;
   r: CARDINAL ;
BEGIN
   r := InitRange() ;
   p := PutRangeNoEval(GetIndice(RangeIndex, r), casebounds) ;
   p^.caseList := b ;
   RETURN( r )
END InitCaseBounds ;


(*
   FillInParameters -
*)

PROCEDURE FillInParameters (r: CARDINAL; s: String) : String ;
VAR
   p: Range ;
BEGIN
   p := GetIndice (RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           :  s := MetaString3 (s, des, expr, dimension) |
      returnassignment     :  s := MetaString3 (s, des, expr, dimension) |
      subrangeassignment   :  InternalError ('unexpected case') |
      inc                  :  s := MetaString3 (s, des, expr, dimension) |
      dec                  :  s := MetaString3 (s, des, expr, dimension) |
      incl                 :  s := MetaString3 (s, des, expr, dimension) |
      excl                 :  s := MetaString3 (s, des, expr, dimension) |
      shift                :  s := MetaString3 (s, des, expr, dimension) |
      rotate               :  s := MetaString3 (s, des, expr, dimension) |
      typeassign           :  |
      typeparam            :  |
      typeexpr             :  |
      paramassign          :  s := MetaString3 (s, des, expr, paramNo) |
      staticarraysubscript :  s := MetaString3 (s, des, expr, dimension) |
      dynamicarraysubscript:  s := MetaString3 (s, des, expr, dimension) |
      forloopbegin         :  s := MetaString3 (s, des, expr, dimension) |
      forloopto            :  s := MetaString3 (s, des, expr, dimension) |
      forloopend           :  s := MetaString3 (s, des, expr, dimension) |
      pointernil           :  s := MetaString3 (s, des, expr, dimension) |
      noreturn             :  s := MetaString3 (s, des, expr, dimension) |
      noelse               :  s := MetaString3 (s, des, expr, dimension) |
      casebounds           :  s := MetaString3 (s, des, expr, dimension) |
      wholenonposdiv       :  s := MetaString3 (s, des, expr, dimension) |
      wholenonposmod       :  s := MetaString3 (s, des, expr, dimension) |
      wholezerodiv         :  s := MetaString3 (s, des, expr, dimension) |
      wholezerorem         :  s := MetaString3 (s, des, expr, dimension) |
      none                 :  |

      ELSE
         InternalError ('unexpected case')
      END
   END ;
   RETURN s
END FillInParameters ;


(*
   GetRangeErrorMessage - returns a specific error message for the range, r.
                          It assumes the 3 parameters to be supplied on the MetaError
                          parameter list are:  dest, expr, paramNo or dimension.

XYZ
                    'the initial assignment to {%1a} at the start of the FOR loop will cause a range error, as the type range of {%1taD} does not overlap with {%2tad}')
                    'the final TO value {%2a} of the FOR loop will cause a range error with the iterator variable {%1a}')
*)

PROCEDURE GetRangeErrorMessage (r: CARDINAL) : String ;
VAR
   p: Range ;
   s: String ;
BEGIN
   p := GetIndice (RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           :  s := InitString ('assignment will cause a range error, as the runtime instance value of {%1tad} does not overlap with the type {%2tad}') |
      returnassignment     :  s := InitString ('attempting to return {%2Wa} from a procedure function {%1a} which will exceed exceed the range of type {%1tad}') |
      subrangeassignment   :  InternalError ('unexpected case') |
      inc                  :  s := InitString ('if the INC is ever executed the expression {%2Wa} will cause an overflow error for the designator {%1a} as it exceeds the type range {%1ts:of {%1ts}}') |
      dec                  :  s := InitString ('if the DEC is ever executed the expression {%2Wa} will cause an underflow error for the designator {%1a} as it exceeds the type range {%1ts:of {%1ts}}') |
      incl                 :  s := InitString ('the expression {%2Wa} given in the INCL exceeds the type range {%1ts} of the designator {%1a}') |
      excl                 :  s := InitString ('the expression {%2Wa} given in the EXCL exceeds the type range {%1ts} of the designator {%1a}') |
      shift                :  s := InitString ('the expression {%2Wa} given in the second parameter to SHIFT exceeds the type range {%1ts} of the first parameter {%1a}') |
      rotate               :  s := InitString ('the expression {%2Wa} given in the second parameter to ROTATE exceeds the type range {%1ts} of the first parameter {%1a}') |
      typeassign           :  s := NIL |
      typeparam            :  s := NIL |
      typeexpr             :  s := NIL |
      paramassign          :  s := InitString('if this call is executed then the actual parameter {%2Wa} will be out of range of the {%3N} formal parameter {%1a}') |
      staticarraysubscript :  s := InitString('if this access to the static array {%1Wa:{%2a:{%1a}[{%2a}]}} is ever made then the index will be out of bounds in the {%3N} array subscript') |
      dynamicarraysubscript:  s := InitString('if this access to the dynamic array {%1Wa:{%2a:{%1a}[{%2a}]}} is ever made then the index will be out of bounds in the {%3N} array subscript') |
      forloopbegin         :  s := InitString('if the assignment in this FOR loop is ever executed then the designator {%1Wa} will be exceed the type range {%1ts:of {%1ts}}') |
      forloopto            :  s := InitString('the final value {%2Wa} in this FOR loop will be out of bounds {%1ts:of type {%1ts}} if ever executed') |
      forloopend           :  s := InitString('the FOR loop will cause the designator {%1Wa} to be out of bounds when the BY value {%2a} is added') |
      pointernil           :  s := InitString('if this pointer value {%1Wa} is ever dereferenced it will cause an exception') |
      noreturn             :  s := InitString('{%1W:}this function will exit without executing a RETURN statement') |
      noelse               :  s := InitString('{%1W:}this CASE statement does not have an ELSE statement') |
      casebounds           :  s := InitString('{%1W:}this CASE statement has overlapping ranges') |
      wholenonposdiv       :  s := InitString('this division expression {%2Wa} will cause an exception as this divisor is less than or equal to zero') |
      wholenonposmod       :  s := InitString('this modulus expression {%2Wa} will cause an exception as this divisor is less than or equal to zero') |
      wholezerodiv         :  s := InitString('this division expression {%2Wa} will cause an exception as the divisor is zero') |
      wholezerorem         :  s := InitString('this remainder expression {%2Wa} will cause an exception as the divisor is zero') |
      none                 :  s := NIL

      ELSE
         InternalError ('unexpected case')
      END
   END ;
   RETURN s
END GetRangeErrorMessage ;


(*
   CodeRangeCheck - returns a Tree representing the code for a
                    range test defined by, r.
*)

PROCEDURE CodeRangeCheck (r: CARDINAL; function: String) ;
VAR
   p      : Range ;
   message: String ;
BEGIN
   p := GetIndice (RangeIndex, r) ;
   message := GetRangeErrorMessage (r) ;
   WITH p^ DO
      CASE type OF

      assignment           :  CodeAssignment (tokenNo, r, function, message) |
      returnassignment     :  CodeReturn (tokenNo, r, function, message) |
      subrangeassignment   :  InternalError  ('unexpected case') |
      inc                  :  CodeInc (tokenNo, r, function, message) |
      dec                  :  CodeDec (tokenNo, r, function, message) |
      incl,
      excl                 :  CodeInclExcl (tokenNo, r, function, message) |
      shift,
      rotate               :  CodeShiftRotate (tokenNo, r, function, message) |
      typeassign           :  CodeTypeCheck (tokenNo, r) |
      typeparam            :  CodeTypeCheck (tokenNo, r) |
      typeexpr             :  CodeTypeCheck (tokenNo, r) |
      staticarraysubscript :  CodeStaticArraySubscript (tokenNo, r, function, message) |
      dynamicarraysubscript:  CodeDynamicArraySubscript (tokenNo, r, function, message) |
      forloopbegin         :  CodeForLoopBegin (tokenNo, r, function, message) |
      forloopto            :  CodeForLoopTo (tokenNo, r, function, message) |
      forloopend           :  CodeForLoopEnd (tokenNo, r, function, message) |
      pointernil           :  CodeNil (r, function, message) |
      noreturn             :  AddStatement (TokenToLocation (tokenNo), CodeErrorCheck (r, function, message)) |
      noelse               :  AddStatement (TokenToLocation (tokenNo), CodeErrorCheck (r, function, message)) |
      casebounds           :  CodeCaseBounds (tokenNo, caseList) |
      wholenonposdiv       :  CodeWholeNonPos (tokenNo, r, function, message) |
      wholenonposmod       :  CodeWholeNonPos (tokenNo, r, function, message) |
      wholezerodiv         :  CodeWholeZero (tokenNo, r, function, message) |
      wholezerorem         :  CodeWholeZero (tokenNo, r, function, message) |
      paramassign          :  CodeParameterAssign (tokenNo, r, function, message) |
      none                 :

      ELSE
         InternalError ('unexpected case')
      END
   END
END CodeRangeCheck ;


(*
   AddVarRead - checks to see whether symbol, Sym, is
                a variable or a parameter and if so it
                then adds this quadruple to the variable
                list.
*)
(*
PROCEDURE AddVarRead (sym: CARDINAL; quadNo: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      PutReadQuad(sym, GetMode(sym), quadNo)
   END
END AddVarRead ;
*)


(*
   SubVarRead - checks to see whether symbol, Sym, is
                a variable or a parameter and if so it
                then removes this quadruple from the
                variable list.
*)

(*
PROCEDURE SubVarRead (sym: CARDINAL; quadNo: CARDINAL) ;
BEGIN
   IF (sym#NulSym) AND IsVar(sym)
   THEN
      RemoveReadQuad(sym, GetMode(sym), quadNo)
   END
END SubVarRead ;
*)


(*
   CheckRangeAddVariableRead - ensures that any references to reading
                               variables used by this range check, r,
                               at this, quadNo, are recorded in the
                               symbol table.
*)

(*
PROCEDURE CheckRangeAddVariableRead (r: CARDINAL; quadNo: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      (* AddVarRead(des, quadNo) ; *)
      (* AddVarRead(expr, quadNo) *)
   END
END CheckRangeAddVariableRead ;
*)


(*
   CheckRangeRemoveVariableRead - ensures that any references to reading
                                  variable at this quadNo are removed from
                                  the symbol table.
*)

(*
PROCEDURE CheckRangeRemoveVariableRead (r: CARDINAL; quadNo: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      (* SubVarRead(des, quadNo) ; *)
      (* SubVarRead(expr, quadNo) *)
   END
END CheckRangeRemoveVariableRead ;
*)


(*
   WriteRangeCheck - displays debugging information about range, r.
*)

PROCEDURE WriteRangeCheck (r: CARDINAL) ;
VAR
   p: Range ;
BEGIN
   p := GetIndice(RangeIndex, r) ;
   WITH p^ DO
      CASE type OF

      assignment           :  WriteString('assignment (') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      returnassignment     :  WriteString('returnassignment (') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      subrangeassignment   :  WriteString('subrangeassignment(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      inc                  :  WriteString('inc(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      dec                  :  WriteString('dec(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      incl                 :  WriteString('incl(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      excl                 :  WriteString('excl(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      shift                :  WriteString('shift(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      rotate               :  WriteString('rotate(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      typeexpr             :  WriteString('expr compatible (') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      typeassign           :  WriteString('assignment compatible (') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      typeparam            :  WriteString('parameter compatible (') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      paramassign          :  WriteString('parameter range (') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      staticarraysubscript :  WriteString('staticarraysubscript(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      dynamicarraysubscript:  WriteString('dynamicarraysubscript(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      forloopbegin         :  WriteString('forloopbegin(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      forloopto            :  WriteString('forloopto(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      forloopend           :  WriteString('forloopend(') ; WriteOperand(des) ; WriteString(', ') ; WriteOperand(expr) |
      pointernil           :  WriteString('pointernil(') ; WriteOperand(des) |
      noreturn             :  WriteString('noreturn(') |
      noelse               :  WriteString('noelse(') |
      casebounds           :  WriteString('casebounds(') ; WriteCase(caseList) |
      wholenonposdiv       :  WriteString('wholenonposdiv(') ; WriteOperand(expr) |
      wholenonposmod       :  WriteString('wholenonposmod(') ; WriteOperand(expr) |
      wholezerodiv         :  WriteString('wholezerodiv(') ; WriteOperand(expr) |
      wholezerorem         :  WriteString('wholezerorem(') ; WriteOperand(expr) |
      none                 :  WriteString('none(') |

      ELSE
         InternalError ('unknown case')
      END ;
      Write(')')
   END
END WriteRangeCheck ;


(*
   Init - initializes the modules global variables.
*)

PROCEDURE Init ;
BEGIN
   TopOfRange := 0 ;
   RangeIndex := InitIndex(1)
END Init ;


BEGIN
   Init
END M2Range.
