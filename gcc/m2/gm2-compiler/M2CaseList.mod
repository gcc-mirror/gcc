(* M2CaseList.mod implement ISO case label lists.

Copyright (C) 2009-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2CaseList ;


FROM M2Debug IMPORT Assert ;
FROM M2GCCDeclare IMPORT TryDeclareConstant, GetTypeMin, GetTypeMax ;
FROM M2MetaError IMPORT MetaError1, MetaError2, MetaErrorT0, MetaErrorT1, MetaErrorT2, MetaErrorT3, MetaErrorT4, MetaErrorString1 ;
FROM M2Error IMPORT InternalError ;
FROM M2Range IMPORT OverlapsRange, IsEqual, IsGreater ;
FROM M2ALU IMPORT PushIntegerTree, PopIntegerTree, Addn, Sub, PushInt ;
FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, ForeachIndiceInIndexDo, HighIndice ;
FROM Lists IMPORT InitList, IncludeItemIntoList ;
FROM NameKey IMPORT KeyToCharStar ;
FROM SymbolConversion IMPORT GccKnowsAbout, Mod2Gcc, AddModGcc ;
FROM DynamicStrings IMPORT InitString, InitStringCharStar, ConCat, Mark, KillString ;
FROM m2tree IMPORT Tree ;
FROM m2block IMPORT RememberType ;
FROM m2type IMPORT GetMinFrom ;
FROM Storage IMPORT ALLOCATE ;
FROM M2Base IMPORT IsExpressionCompatible ;
FROM M2Printf IMPORT printf1 ;

FROM SymbolTable IMPORT NulSym, IsConst, IsFieldVarient, IsRecord, IsRecordField, GetVarientTag, GetType,
                        ForeachLocalSymDo, GetSymName, IsEnumeration, SkipType ;

TYPE
   RangePair = POINTER TO RECORD
                  low, high: CARDINAL ;
                  tokenno  : CARDINAL ;
               END ;

   ConflictingPair = POINTER TO RECORD
                        a, b: RangePair ;
                     END ;

   CaseList = POINTER TO RECORD
                 maxRangeId  : CARDINAL ;
                 rangeArray  : Index ;
                 currentRange: RangePair ;
                 varientField: CARDINAL ;
              END ;

   CaseDescriptor = POINTER TO RECORD
                       elseClause   : BOOLEAN ;
                       elseField    : CARDINAL ;
                       record       : CARDINAL ;
                       varient      : CARDINAL ;
                       maxCaseId    : CARDINAL ;
                       caseListArray: Index ;
                       currentCase  : CaseList ;
                       next         : CaseDescriptor ;
                    END ;

   SetRange = POINTER TO RECORD
                 low, high: Tree ;
                 next     : SetRange ;
              END ;

VAR
   caseStack    : CaseDescriptor ;
   caseId       : CARDINAL ;
   caseArray    : Index ;
   conflictArray: Index ;
   FreeRangeList: SetRange ;



(*
   PushCase - create a case entity and push it to an internal stack.
              r, is NulSym if this is a CASE statement.
              If, r, is a record then it indicates it includes one
              or more varients reside in the record.  The particular
              varient is, v.
              Return the case id.
*)

PROCEDURE PushCase (r: CARDINAL; v: CARDINAL) : CARDINAL ;
VAR
   c: CaseDescriptor ;
BEGIN
   INC(caseId) ;
   NEW(c) ;
   IF c=NIL
   THEN
      InternalError ('out of memory error')
   ELSE
      WITH c^ DO
         elseClause := FALSE ;
         elseField := NulSym ;
         record := r ;
         varient := v ;
         maxCaseId := 0 ;
         caseListArray := InitIndex(1) ;
         next := caseStack ;
         currentCase := NIL
      END ;
      caseStack := c ;
      PutIndice(caseArray, caseId, c)
   END ;
   RETURN( caseId )
END PushCase ;


(*
   PopCase - pop the top element of the case entity from the internal
             stack.
*)

PROCEDURE PopCase ;
BEGIN
   IF caseStack=NIL
   THEN
      InternalError ('case stack is empty')
   END ;
   caseStack := caseStack^.next
END PopCase ;


(*
   ElseCase - indicates that this case varient does have an else clause.
*)

PROCEDURE ElseCase (f: CARDINAL) ;
BEGIN
   WITH caseStack^ DO
      elseClause := TRUE ;
      elseField := f
   END
END ElseCase ;


(*
   BeginCaseList - create a new label list.
*)

PROCEDURE BeginCaseList (v: CARDINAL) ;
VAR
   l: CaseList ;
BEGIN
   NEW(l) ;
   IF l=NIL
   THEN
      InternalError ('out of memory error')
   END ;
   WITH l^ DO
      maxRangeId   := 0 ;
      rangeArray   := InitIndex(1) ;
      currentRange := NIL ;
      varientField := v
   END ;
   WITH caseStack^ DO
      INC(maxCaseId) ;
      PutIndice(caseListArray, maxCaseId, l) ;
      currentCase := l
   END
END BeginCaseList ;


(*
   EndCaseList - terminate the current label list.
*)

PROCEDURE EndCaseList ;
BEGIN
   caseStack^.currentCase := NIL
END EndCaseList ;


(*
   AddRange - add a range to the current label list.
*)

PROCEDURE AddRange (r1, r2: CARDINAL; tok: CARDINAL) ;
VAR
   r: RangePair ;
BEGIN
   NEW(r) ;
   IF r=NIL
   THEN
      InternalError ('out of memory error')
   ELSE
      WITH r^ DO
         low := r1 ;
         high := r2 ;
         tokenno := tok
      END ;
      WITH caseStack^.currentCase^ DO
         INC(maxRangeId) ;
         PutIndice(rangeArray, maxRangeId, r) ;
         currentRange := r
      END
   END
END AddRange ;


(*
   GetVariantTagType - returns the type associated with, variant.
*)

PROCEDURE GetVariantTagType (variant: CARDINAL) : CARDINAL ;
VAR
   tag: CARDINAL ;
BEGIN
   tag := GetVarientTag(variant) ;
   IF IsFieldVarient(tag) OR IsRecordField(tag)
   THEN
      RETURN( GetType(tag) )
   ELSE
      RETURN( tag )
   END
END GetVariantTagType ;


(*
   CaseBoundsResolved - returns TRUE if all constants in the case list, c,
                        are known to GCC.
*)

PROCEDURE CaseBoundsResolved (tokenno: CARDINAL; c: CARDINAL) : BOOLEAN ;
VAR
   resolved: BOOLEAN ;
   p       : CaseDescriptor ;
   q       : CaseList ;
   r       : RangePair ;
   min,
   max,
   type,
   i, j    : CARDINAL ;
BEGIN
   p := GetIndice(caseArray, c) ;
   WITH p^ DO
      IF varient#NulSym
      THEN
         (* not a CASE statement, but a varient record containing without an ELSE clause *)
         type := GetVariantTagType(varient) ;
         resolved := TRUE ;
         IF NOT GccKnowsAbout(type)
         THEN
            (* do we need to add, type, to the list of types required to be resolved? *)
            resolved := FALSE
         END ;
         min := GetTypeMin(type) ;
         IF NOT GccKnowsAbout(min)
         THEN
            TryDeclareConstant(tokenno, min) ;
            resolved := FALSE
         END ;
         max := GetTypeMax(type) ;
         IF NOT GccKnowsAbout(max)
         THEN
            TryDeclareConstant(tokenno, max) ;
            resolved := FALSE
         END ;
         IF NOT resolved
         THEN
            RETURN( FALSE )
         END
      END ;
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice(caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            r := GetIndice(q^.rangeArray, j) ;
            IF r^.low#NulSym
            THEN
               IF IsConst(r^.low)
               THEN
                  TryDeclareConstant(tokenno, r^.low) ;
                  IF NOT GccKnowsAbout(r^.low)
                  THEN
                     RETURN( FALSE )
                  END
               ELSE
                  IF r^.high=NulSym
                  THEN
                     MetaError1('the CASE statement variant must be defined by a constant {%1Da:is a {%1d}}', r^.low)
                  ELSE
                     MetaError1('the CASE statement variant low value in a range must be defined by a constant {%1Da:is a {%1d}}',
                                r^.low)
                  END
               END
            END ;
            IF r^.high#NulSym
            THEN
               IF IsConst(r^.high)
               THEN
                  TryDeclareConstant(tokenno, r^.high) ;
                  IF NOT GccKnowsAbout(r^.high)
                  THEN
                     RETURN( FALSE )
                  END
               ELSE
                  MetaError1('the CASE statement variant high value in a range must be defined by a constant {%1Da:is a {%1d}}',
                             r^.high)
               END
            END ;
            INC(j)
         END ;
         INC(i)
      END
   END ;
   RETURN( TRUE )
END CaseBoundsResolved ;


(*
   IsSame - return TRUE if r, s, are in, e.
*)

PROCEDURE IsSame (e: ConflictingPair; r, s: RangePair) : BOOLEAN ;
BEGIN
   WITH e^ DO
      RETURN( ((a=r) AND (b=s)) OR ((a=s) AND (b=r)) )
   END
END IsSame ;


(*
   SeenBefore -
*)

PROCEDURE SeenBefore (r, s: RangePair) : BOOLEAN ;
VAR
   i, h: CARDINAL ;
   e   : ConflictingPair ;
BEGIN
   h := HighIndice(conflictArray) ;
   i := 1 ;
   WHILE i<=h DO
      e := GetIndice(conflictArray, i) ;
      IF IsSame(e, r, s)
      THEN
         RETURN( TRUE )
      END ;
      INC(i)
   END ;
   NEW(e) ;
   WITH e^ DO
      a := r ;
      b := s
   END ;
   PutIndice(conflictArray, h+1, e) ;
   RETURN( FALSE )
END SeenBefore ;


(*
   Overlaps -
*)

PROCEDURE Overlaps (r, s: RangePair) : BOOLEAN ;
VAR
   a, b, c, d: CARDINAL ;
BEGIN
   a := r^.low ;
   c := s^.low ;
   IF r^.high=NulSym
   THEN
      b := a ;
      IF s^.high=NulSym
      THEN
         d := c ;
         IF OverlapsRange(Mod2Gcc(a), Mod2Gcc(b), Mod2Gcc(c), Mod2Gcc(d))
         THEN
            IF NOT SeenBefore(r, s)
            THEN
               MetaErrorT2 (r^.tokenno, 'case label {%1ad} is a duplicate with {%2ad}', a, c) ;
               MetaErrorT2 (s^.tokenno, 'case label {%1ad} is a duplicate with {%2ad}', c, a)
            END ;
            RETURN( TRUE )
         END
      ELSE
         d := s^.high ;
         IF OverlapsRange(Mod2Gcc(a), Mod2Gcc(b), Mod2Gcc(c), Mod2Gcc(d))
         THEN
            IF NOT SeenBefore (r, s)
            THEN
               MetaErrorT3 (r^.tokenno, 'case label {%1ad} is a duplicate in the range {%2ad}..{%3ad}', a, c, d) ;
               MetaErrorT3 (s^.tokenno, 'case range {%2ad}..{%3ad} is a duplicate of case label {%1ad}', c, d, a)
            END ;
            RETURN( TRUE )
         END
      END
   ELSE
      b := r^.high ;
      IF s^.high=NulSym
      THEN
         d := c ;
         IF OverlapsRange (Mod2Gcc(a), Mod2Gcc(b), Mod2Gcc(c), Mod2Gcc(d))
         THEN
            IF NOT SeenBefore(r, s)
            THEN
               MetaErrorT3 (r^.tokenno, 'case range {%1ad}..{%2ad} is a duplicate with case label {%3ad}', a, b, c) ;
               MetaErrorT3 (s^.tokenno, 'case label {%1ad} is a duplicate with case range %{2ad}..{%3ad}', c, a, b)
            END ;
            RETURN( TRUE )
         END
      ELSE
         d := s^.high ;
         IF OverlapsRange(Mod2Gcc(a), Mod2Gcc(b), Mod2Gcc(c), Mod2Gcc(d))
         THEN
            IF NOT SeenBefore(r, s)
            THEN
               MetaErrorT4 (r^.tokenno, 'case range {%1ad}..{%2ad} overlaps case range {%3ad}..{%4ad}', a, b, c, d) ;
               MetaErrorT4 (s^.tokenno, 'case range {%1ad}..{%2ad} overlaps case range {%3ad}..{%4ad}', c, d, a, b)
            END ;
            RETURN( TRUE )
         END
      END
   END ;
   RETURN( FALSE )
END Overlaps ;


(*
   OverlappingCaseBound - returns TRUE if, r, overlaps any case bound in the
                          case statement, c.
*)

PROCEDURE OverlappingCaseBound (r: RangePair; c: CARDINAL) : BOOLEAN ;
VAR
   p      : CaseDescriptor ;
   q      : CaseList ;
   s      : RangePair ;
   i, j   : CARDINAL ;
   overlap: BOOLEAN ;
BEGIN
   p := GetIndice (caseArray, c) ;
   overlap := FALSE ;
   WITH p^ DO
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice (caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            s := GetIndice (q^.rangeArray, j) ;
            IF (s#r) AND Overlaps (r, s)
            THEN
               overlap := TRUE
            END ;
            INC (j)
         END ;
         INC (i)
      END
   END ;
   RETURN( overlap )
END OverlappingCaseBound ;


(*
   OverlappingCaseBounds - returns TRUE if there were any overlapping bounds
                           in the case list, c.  It will generate an error
                           messages for each overlapping bound found.
*)

PROCEDURE OverlappingCaseBounds (c: CARDINAL) : BOOLEAN ;
VAR
   p      : CaseDescriptor ;
   q      : CaseList ;
   r      : RangePair ;
   i, j   : CARDINAL ;
   overlap: BOOLEAN ;
BEGIN
   p := GetIndice(caseArray, c) ;
   overlap := FALSE ;
   WITH p^ DO
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice(caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            r := GetIndice(q^.rangeArray, j) ;
            IF OverlappingCaseBound (r, c)
            THEN
               overlap := TRUE
            END ;
            INC(j)
         END ;
         INC(i)
      END
   END ;
   RETURN( overlap )
END OverlappingCaseBounds ;


(*
   NewRanges -
*)

PROCEDURE NewRanges () : SetRange ;
VAR
   s: SetRange ;
BEGIN
   IF FreeRangeList=NIL
   THEN
      NEW(s)
   ELSE
      s := FreeRangeList ;
      FreeRangeList := FreeRangeList^.next
   END ;
   s^.next := NIL ;
   RETURN( s )
END NewRanges ;


(*
   NewSet -
*)

PROCEDURE NewSet (type: CARDINAL) : SetRange ;
VAR
   s: SetRange ;
BEGIN
   s := NewRanges() ;
   WITH s^ DO
      low := Mod2Gcc(GetTypeMin(type)) ;
      high := Mod2Gcc(GetTypeMax(type)) ;
      next := NIL
   END ;
   RETURN( s )
END NewSet ;


(*
   DisposeRanges -
*)

PROCEDURE DisposeRanges (set: SetRange) : SetRange ;
VAR
   t: SetRange ;
BEGIN
   IF set#NIL
   THEN
      IF FreeRangeList=NIL
      THEN
         FreeRangeList := set
      ELSE
         t := set ;
         WHILE t^.next#NIL DO
            t := t^.next
         END ;
         t^.next := FreeRangeList ;
         FreeRangeList := set
      END
   END ;
   RETURN( NIL )
END DisposeRanges ;


(*
   SubBitRange - subtracts bits, lo..hi, from, set.
*)

PROCEDURE SubBitRange (set: SetRange; lo, hi: Tree; tokenno: CARDINAL) : SetRange ;
VAR
   h, i : SetRange ;
BEGIN
   h := set ;
   WHILE h#NIL DO
      IF (h^.high=NIL) OR IsEqual(h^.high, h^.low)
      THEN
         IF IsEqual(h^.low, lo) OR OverlapsRange(lo, hi, h^.low, h^.low)
         THEN
            IF h=set
            THEN
               set := set^.next ;
               h^.next := NIL ;
               h := DisposeRanges(h) ;
               h := set
            ELSE
               i := set ;
               WHILE i^.next#h DO
                  i := i^.next
               END ;
               i^.next := h^.next ;
               i := h ;
               h := h^.next ;
               i^.next := NIL ;
               i := DisposeRanges(i)
            END
         ELSE
            h := h^.next
         END
      ELSE
         IF OverlapsRange(lo, hi, h^.low, h^.high)
         THEN
            IF IsGreater(h^.low, lo) OR IsGreater(hi, h^.high)
            THEN
               MetaErrorT0 (tokenno, 'variant case range lies outside tag value')
            ELSE
               IF IsEqual(h^.low, lo)
               THEN
                  PushIntegerTree(hi) ;
                  PushInt(1) ;
                  Addn ;
                  h^.low := PopIntegerTree()
               ELSIF IsEqual(h^.high, hi)
               THEN
                  PushIntegerTree(lo) ;
                  PushInt(1) ;
                  Sub ;
                  h^.high := PopIntegerTree()
               ELSE
                  (* lo..hi  exist inside range h^.low..h^.high *)
                  i := NewRanges() ;
                  i^.next := h^.next ;
                  h^.next := i ;
                  i^.high := h^.high ;
                  PushIntegerTree(lo) ;
                  PushInt(1) ;
                  Sub ;
                  h^.high := PopIntegerTree() ;
                  PushIntegerTree(hi) ;
                  PushInt(1) ;
                  Addn ;
                  i^.low := PopIntegerTree()
               END
            END
         ELSE
            h := h^.next
         END
      END
   END ;
   RETURN( set )
END SubBitRange ;


(*
   ExcludeCaseRanges - excludes all case ranges found in, p, from, set
*)

PROCEDURE ExcludeCaseRanges (set: SetRange; p: CaseDescriptor) : SetRange ;
VAR
   i, j: CARDINAL ;
   q   : CaseList ;
   r   : RangePair ;
BEGIN
   WITH p^ DO
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice(caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            r := GetIndice(q^.rangeArray, j) ;
            IF r^.high=NulSym
            THEN
               set := SubBitRange(set, Mod2Gcc(r^.low), Mod2Gcc(r^.low), r^.tokenno)
            ELSE
               set := SubBitRange(set, Mod2Gcc(r^.low), Mod2Gcc(r^.high), r^.tokenno)
            END ;
            INC(j)
         END ;
         INC(i)
      END
   END ;
   RETURN( set )
END ExcludeCaseRanges ;


VAR
   High, Low  : Tree ;
   errorString: String ;


(*
   DoEnumValues -
*)

PROCEDURE DoEnumValues (sym: CARDINAL) ;
BEGIN
   IF (Low#NIL) AND IsEqual(Mod2Gcc(sym), Low)
   THEN
      errorString := ConCat(errorString, InitStringCharStar(KeyToCharStar(GetSymName(sym)))) ;
      Low := NIL
   END ;
   IF (High#NIL) AND IsEqual(Mod2Gcc(sym), High)
   THEN
      errorString := ConCat(errorString, Mark(InitString('..'))) ;
      errorString := ConCat(errorString, Mark(InitStringCharStar(KeyToCharStar(GetSymName(sym))))) ;
      High := NIL
   END
END DoEnumValues ;


(*
   ErrorRange -
*)

PROCEDURE ErrorRange (p: CaseDescriptor; type: CARDINAL; set: SetRange) ;
BEGIN
   type := SkipType(type) ;
   IF IsEnumeration(type)
   THEN
      Low := set^.low ;
      High := set^.high ;
      IF IsEqual(Low, High)
      THEN
         High := NIL ;
         errorString := InitString('enumeration value ') ;
         ForeachLocalSymDo(type, DoEnumValues) ;
         errorString := ConCat(errorString, InitString(' is ignored by the CASE variant record {%1D}'))
      ELSE
         errorString := InitString('enumeration values ') ;
         ForeachLocalSymDo(type, DoEnumValues) ;
         errorString := ConCat(errorString, InitString(' are ignored by the CASE variant record {%1D}'))
      END ;
      MetaErrorString1(errorString, p^.varient)
   END
END ErrorRange ;


(*
   ErrorRanges -
*)

PROCEDURE ErrorRanges (p: CaseDescriptor; type: CARDINAL; set: SetRange) ;
BEGIN
   WHILE set#NIL DO
      ErrorRange(p, type, set) ;
      set := set^.next
   END
END ErrorRanges ;


(*
   MissingCaseBounds - returns TRUE if there were any missing bounds
                       in the varient record case list, c.  It will
                       generate an error message for each missing
                       bounds found.
*)

PROCEDURE MissingCaseBounds (tokenno: CARDINAL; c: CARDINAL) : BOOLEAN ;
VAR
   p      : CaseDescriptor ;
   type   : CARDINAL ;
   missing: BOOLEAN ;
   set    : SetRange ;
BEGIN
   p := GetIndice(caseArray, c) ;
   missing := FALSE ;
   WITH p^ DO
      IF (record#NulSym) AND (varient#NulSym) AND (NOT elseClause)
      THEN
         (* not a CASE statement, but a varient record containing without an ELSE clause *)
         type := GetVariantTagType(varient) ;
         set := NewSet(type) ;
         set := ExcludeCaseRanges(set, p) ;
         IF set#NIL
         THEN
            missing := TRUE ;
            MetaErrorT2 (tokenno,
                         'not all variant record alternatives in the {%kCASE} clause are specified, hint you either need to specify each value of {%2ad} or use an {%kELSE} clause',
                         varient, type) ;
            ErrorRanges(p, type, set)
         END ;
         set := DisposeRanges(set)
      END
   END ;
   RETURN( missing )
END MissingCaseBounds ;


(*
   InRangeList - returns TRUE if the value, tag, is defined in the case list.

PROCEDURE InRangeList (cl: CaseList; tag: CARDINAL) : BOOLEAN ;
VAR
   i, h: CARDINAL ;
   r   : RangePair ;
   a   : Tree ;
BEGIN
   WITH cl^ DO
      i := 1 ;
      h := HighIndice(rangeArray) ;
      WHILE i<=h DO
         r := GetIndice(rangeArray, i) ;
         WITH r^ DO
            IF high=NulSym
            THEN
               a := Mod2Gcc(low)
            ELSE
               a := Mod2Gcc(high)
            END ;
            IF OverlapsRange(Mod2Gcc(low), a, Mod2Gcc(tag), Mod2Gcc(tag))
            THEN
               RETURN( TRUE )
            END
         END ;
         INC(i)
      END
   END ;
   RETURN( FALSE )
END InRangeList ;
*)


(*
   WriteCase - dump out the case list (internal debugging).
*)

PROCEDURE WriteCase (c: CARDINAL) ;
BEGIN
   (* this debugging procedure should be finished.  *)
   printf1 ("%d", c)
END WriteCase ;


(*
   checkTypes - checks to see that, constant, and, type, are compatible.
*)

PROCEDURE checkTypes (constant, type: CARDINAL) : BOOLEAN ;
VAR
   consttype: CARDINAL ;
BEGIN
   IF (constant#NulSym) AND IsConst(constant)
   THEN
      consttype := GetType(constant) ;
      IF NOT IsExpressionCompatible(consttype, type)
      THEN
         MetaError2('the CASE statement variant tag {%1ad} must be type compatible with the constant {%2Da:is a {%2d}}',
                    type, constant) ;
         RETURN( FALSE )
      END
   END ;
   RETURN( TRUE )
END checkTypes ;


(*
   inRange - returns TRUE if, min <= i <= max.
*)

PROCEDURE inRange (i, min, max: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( OverlapsRange(Mod2Gcc(i), Mod2Gcc(i), Mod2Gcc(min), Mod2Gcc(max)) )
END inRange ;


(*
   TypeCaseBounds - returns TRUE if all bounds in case list, c, are
                    compatible with the tagged type.
*)

PROCEDURE TypeCaseBounds (c: CARDINAL) : BOOLEAN ;
VAR
   p         : CaseDescriptor ;
   q         : CaseList ;
   r         : RangePair ;
   min, max,
   type,
   i, j      : CARDINAL ;
   compatible: BOOLEAN ;
BEGIN
   p := GetIndice(caseArray, c) ;
   type := NulSym ;
   WITH p^ DO
      type := NulSym ;
      IF varient#NulSym
      THEN
         (* not a CASE statement, but a varient record containing without an ELSE clause *)
         type := GetVariantTagType(varient) ;
         min := GetTypeMin(type) ;
         max := GetTypeMax(type)
      END ;
      IF type=NulSym
      THEN
         RETURN( TRUE )
      END ;
      compatible := TRUE ;
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice(caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            r := GetIndice(q^.rangeArray, j) ;
            IF (r^.low#NulSym) AND (NOT inRange(r^.low, min, max))
            THEN
               MetaError2('the CASE statement variant range {%1ad} exceeds that of the tag type {%2ad}',
                          r^.low, type) ;
               compatible := FALSE
            END ;
            IF NOT checkTypes(r^.low, type)
            THEN
               compatible := FALSE
            END ;
            IF (r^.high#NulSym) AND (NOT inRange(r^.high, min, max))
            THEN
               MetaError2('the CASE statement variant range {%1ad} exceeds that of the tag type {%2ad}',
                          r^.high, type) ;
               compatible := FALSE
            END ;
            IF NOT checkTypes(r^.high, type)
            THEN
               compatible := FALSE
            END ;
            INC(j)
         END ;
         INC(i)
      END ;
      RETURN( compatible )
   END
END TypeCaseBounds ;


BEGIN
   caseStack := NIL ;
   caseId := 0 ;
   caseArray := InitIndex(1) ;
   conflictArray := InitIndex(1) ;
   FreeRangeList := NIL
END M2CaseList.
