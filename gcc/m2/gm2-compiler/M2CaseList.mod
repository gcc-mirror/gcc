(* M2CaseList.mod implement ISO case label lists.

Copyright (C) 2009-2024 Free Software Foundation, Inc.
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
FROM M2MetaError IMPORT MetaError1, MetaError2, MetaErrorT0, MetaErrorT1, MetaErrorT2, MetaErrorT3, MetaErrorT4, MetaErrorStringT0, MetaErrorString1 ;
FROM M2Error IMPORT InternalError ;
FROM M2Range IMPORT OverlapsRange, IsEqual, IsGreater ;
FROM M2ALU IMPORT PushIntegerTree, PopIntegerTree, Addn, Sub, PushInt, PushCard ;
FROM Indexing IMPORT Index, InitIndex, PutIndice, GetIndice, ForeachIndiceInIndexDo, HighIndice ;
FROM Lists IMPORT InitList, IncludeItemIntoList, RemoveItemFromList, NoOfItemsInList, GetItemFromList ;
FROM NameKey IMPORT NulName, KeyToCharStar ;
FROM SymbolConversion IMPORT GccKnowsAbout, Mod2Gcc, AddModGcc ;
FROM DynamicStrings IMPORT InitString, InitStringCharStar, InitStringChar, ConCat, Mark, KillString ;
FROM gcctypes IMPORT tree ;
FROM m2block IMPORT RememberType ;
FROM m2type IMPORT GetMinFrom ;
FROM m2expr IMPORT GetIntegerOne, CSTIntToString, CSTIntToChar ;
FROM Storage IMPORT ALLOCATE ;
FROM M2Base IMPORT IsExpressionCompatible, Char ;
FROM M2LexBuf IMPORT TokenToLocation ;
FROM NumberIO IMPORT WriteCard ;

FROM SymbolTable IMPORT NulSym, IsConst, IsFieldVarient, IsRecord, IsRecordField, GetVarientTag, GetType,
                        ForeachLocalSymDo, GetSymName, IsEnumeration, SkipType, NoOfElements, GetNth,
                        IsSubrange, MakeConstLit, IsConstString, GetStringLength, MakeConstVar, PutConst,
                        PopValue ;

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
                       resolved     : BOOLEAN ;
                       elseClause   : BOOLEAN ;
                       elseField    : CARDINAL ;
                       record       : CARDINAL ;
                       varient      : CARDINAL ;
                       expression   : CARDINAL ;
                       maxCaseId    : CARDINAL ;
                       caseListArray: Index ;
                       currentCase  : CaseList ;
                       next         : CaseDescriptor ;
                    END ;

   SetRange = POINTER TO RECORD
                 low, high: tree ;
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
              rec is NulSym if this is a CASE statement.
              If rec is a record then it indicates a possible
              varients reside in the record to check.
              Both rec and va might be NulSym and then the expr
              will contain the selector expression to a case statement.
              Return the case id.
*)

PROCEDURE PushCase (rec, va, expr: CARDINAL) : CARDINAL ;
VAR
   c: CaseDescriptor ;
BEGIN
   INC (caseId) ;
   NEW (c) ;
   IF c = NIL
   THEN
      InternalError ('out of memory error')
   ELSE
      WITH c^ DO
         resolved := FALSE ;
         elseClause := FALSE ;
         elseField := NulSym ;
         record := rec ;
         varient := va ;
         expression := expr ;
         maxCaseId := 0 ;
         caseListArray := InitIndex (1) ;
         next := caseStack ;
         currentCase := NIL
      END ;
      caseStack := c ;
      PutIndice (caseArray, caseId, c)
   END ;
   RETURN caseId
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
   p: CaseDescriptor ;
BEGIN
   p := GetIndice (caseArray, c) ;
   IF p^.resolved
   THEN
      RETURN TRUE
   ELSE
      IF CheckCaseBoundsResolved (tokenno, c)
      THEN
         ConvertNulStr2NulChar (tokenno, c) ;
         RETURN TRUE
      ELSE
         RETURN FALSE
      END
   END
END CaseBoundsResolved ;


(*
   CheckCaseBoundsResolved - return TRUE if all constants in the case list c are known to GCC.
*)

PROCEDURE CheckCaseBoundsResolved (tokenno: CARDINAL; c: CARDINAL) : BOOLEAN ;
VAR
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
END CheckCaseBoundsResolved ;


(*
   ConvertNulStr2NulChar -
*)

PROCEDURE ConvertNulStr2NulChar (tokenno: CARDINAL; c: CARDINAL) ;
VAR
   p   : CaseDescriptor ;
   q   : CaseList ;
   r   : RangePair ;
   i, j: CARDINAL ;
BEGIN
   p := GetIndice (caseArray, c) ;
   WITH p^ DO
      i := 1 ;
      WHILE i <= maxCaseId DO
         q := GetIndice (caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            r := GetIndice (q^.rangeArray, j) ;
            r^.low := NulStr2NulChar (tokenno, r^.low) ;
            r^.high := NulStr2NulChar (tokenno, r^.high) ;
            INC (j)
         END ;
         INC (i)
      END
   END
END ConvertNulStr2NulChar ;


(*
   NulStr2NulChar - if sym is a const string of length 0 then return
                    a nul char instead otherwise return sym.
*)

PROCEDURE NulStr2NulChar (tok: CARDINAL; sym: CARDINAL) : CARDINAL ;
BEGIN
   IF sym # NulSym
   THEN
      IF IsConst (sym) AND IsConstString (sym) AND GccKnowsAbout (sym)
      THEN
         IF GetStringLength (tok, sym) = 0
         THEN
            sym := MakeConstVar (tok, NulName) ;
            PutConst (sym, Char) ;
            PushCard (0) ;
            PopValue (sym) ;
            TryDeclareConstant (tok, sym) ;
            Assert (GccKnowsAbout (sym))
         END
      END
   END ;
   RETURN sym
END NulStr2NulChar ;


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
   GetCaseExpression - return the type from the expression.
*)

PROCEDURE GetCaseExpression (p: CaseDescriptor) : CARDINAL ;
VAR
   type: CARDINAL ;
BEGIN
   WITH p^ DO
      IF expression = NulSym
      THEN
         type := NulSym
      ELSE
         type := SkipType (GetType (expression))
      END
   END ;
   RETURN type
END GetCaseExpression ;


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
   p := GetIndice (caseArray, c) ;
   overlap := FALSE ;
   WITH p^ DO
      i := 1 ;
      WHILE i<=maxCaseId DO
         q := GetIndice (caseListArray, i) ;
         j := 1 ;
         WHILE j<=q^.maxRangeId DO
            r := GetIndice (q^.rangeArray, j) ;
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
   NewRanges - return a new range from the freelist or heap.
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
   NewSet - returns a new set based on type with the low and high fields assigned
            to the min and max values for the type.
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
   DisposeRanges - place set and its list onto the free list.
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
   RemoveRange - removes the range descriptor h from set and return the
                 possibly new head of set.
*)

PROCEDURE RemoveRange (set: SetRange; h: SetRange) : SetRange ;
VAR
   i: SetRange ;
BEGIN
   IF h=set
   THEN
      set := set^.next ;
      h^.next := NIL ;
      h := DisposeRanges(h) ;
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
   END ;
   RETURN set
END RemoveRange ;


(*
   SubBitRange - subtracts bits, lo..hi, from, set.
*)

PROCEDURE SubBitRange (set: SetRange; lo, hi: tree; tokenno: CARDINAL) : SetRange ;
VAR
   h, i: SetRange ;
BEGIN
   h := set ;
   WHILE h#NIL DO
      (* Check to see if a single set element h is obliterated by lo..hi.  *)
      IF (h^.high=NIL) OR IsEqual(h^.high, h^.low)
      THEN
         IF IsEqual(h^.low, lo) OR OverlapsRange(lo, hi, h^.low, h^.low)
         THEN
            set := RemoveRange (set, h) ;
            h := set
         ELSE
            h := h^.next
         END
      (* Now check to see if the lo..hi match exactly with the set range.  *)
      ELSIF (h^.high#NIL) AND IsEqual (lo, h^.low) AND IsEqual (hi, h^.high)
      THEN
         (* Remove h and return as lo..hi have been removed.  *)
         RETURN RemoveRange (set, h)
      ELSE
         (* All other cases require modifying the existing set range.  *)
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
   CheckLowHigh - checks to see the low value <= high value and issues an error
                  if this is not true.
*)

PROCEDURE CheckLowHigh (rp: RangePair) ;
VAR
   lo, hi: tree ;
   temp  : CARDINAL ;
BEGIN
   lo := Mod2Gcc (rp^.low) ;
   hi := Mod2Gcc (rp^.high) ;
   IF IsGreater (lo, hi)
   THEN
      MetaErrorT2 (rp^.tokenno, 'case range should be low..high rather than high..low, range specified as {%1Euad}..{%2Euad}', rp^.low, rp^.high) ;
      temp := rp^.high ;
      rp^.high := rp^.low ;
      rp^.low := temp
   END
END CheckLowHigh ;


(*
   ExcludeCaseRanges - excludes all case ranges found in, p, from, set
*)

PROCEDURE ExcludeCaseRanges (set: SetRange; cd: CaseDescriptor) : SetRange ;
VAR
   i, j: CARDINAL ;
   cl  : CaseList ;
   rp  : RangePair ;
BEGIN
   WITH cd^ DO
      i := 1 ;
      WHILE i <= maxCaseId DO
         cl := GetIndice (caseListArray, i) ;
         j := 1 ;
         WHILE j <= cl^.maxRangeId DO
            rp := GetIndice (cl^.rangeArray, j) ;
            IF rp^.high = NulSym
            THEN
               set := SubBitRange (set,
                                   Mod2Gcc (rp^.low),
                                   Mod2Gcc (rp^.low), rp^.tokenno)
            ELSE
               CheckLowHigh (rp) ;
               set := SubBitRange (set,
                                   Mod2Gcc (rp^.low),
                                   Mod2Gcc (rp^.high), rp^.tokenno)
            END ;
            INC (j)
         END ;
         INC (i)
      END
   END ;
   RETURN set
END ExcludeCaseRanges ;


VAR
   errorString: String ;


(*
   IncludeElement - only include enumeration field into errorString if it lies between low..high.
*)

PROCEDURE IncludeElement (enumList: List; field: CARDINAL; low, high: tree) ;
VAR
   fieldTree: tree ;
BEGIN
   IF field # NulSym
   THEN
      fieldTree := Mod2Gcc (field) ;
      IF OverlapsRange (fieldTree, fieldTree, low, high)
      THEN
         IncludeItemIntoList (enumList, field)
      END
   END
END IncludeElement ;


(*
   IncludeElements - only include enumeration field values low..high in errorString.
*)

PROCEDURE IncludeElements (type: CARDINAL; enumList: List; low, high: tree) ;
VAR
   field     : CARDINAL ;
   i,
   NoElements: CARDINAL ;
BEGIN
   NoElements := NoOfElements (type) ;
   i := 1 ;
   WHILE i <= NoElements DO
      field := GetNth (type, i) ;
      IncludeElement (enumList, field, low, high) ;
      INC (i)
   END
END IncludeElements ;


(*
   ErrorRangeEnum - include enumeration fields Low to High in errorString.
*)

PROCEDURE ErrorRangeEnum (type: CARDINAL; set: SetRange; enumList: List) ;
VAR
   Low, High: tree ;
BEGIN
   Low := set^.low ;
   High := set^.high ;
   IF Low = NIL
   THEN
      Low := High
   END ;
   IF High = NIL
   THEN
      High := Low
   END ;
   IF (Low # NIL) AND (High # NIL)
   THEN
      IncludeElements (type, enumList, Low, High)
   END
END ErrorRangeEnum ;


(*
   ErrorRanges - return a list of all enumeration fields not present in the case statement.
                 The return value will be nil if type is not an enumeration type.
*)

PROCEDURE ErrorRanges (type: CARDINAL; set: SetRange) : List ;
VAR
   enumSet: List ;
BEGIN
   type := SkipType (type) ;
   IF IsEnumeration (type)
   THEN
      InitList (enumSet) ;
      WHILE set#NIL DO
         ErrorRangeEnum (type, set, enumSet) ;
         set := set^.next
      END ;
      RETURN enumSet
   END ;
   RETURN NIL
END ErrorRanges ;


(*
   appendString - appends str to errorString.
*)

PROCEDURE appendString (str: String) ;
BEGIN
   errorString := ConCat (errorString, str)
END appendString ;


(*
   appendEnum - appends enum to errorString.
*)

PROCEDURE appendEnum (enum: CARDINAL) ;
BEGIN
   appendString (Mark (InitStringCharStar (KeyToCharStar (GetSymName (enum)))))
END appendEnum ;


(*
   appendStr - appends str to errorString.
*)

PROCEDURE appendStr (str: ARRAY OF CHAR) ;
BEGIN
   appendString (Mark (InitString (str)))
END appendStr ;


(*
   EnumerateErrors - populate errorString with the contents of enumList.
*)

PROCEDURE EnumerateErrors (enumList: List) ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList (enumList) ;
   IF (enumList # NIL) AND (n > 0)
   THEN
      IF n = 1
      THEN
         errorString := InitString ('{%W}the missing enumeration field is: ') ;
      ELSE
         errorString := InitString ('{%W}the missing enumeration fields are: ') ;
      END ;
      appendEnum (GetItemFromList (enumList, 1)) ;
      IF n > 1
      THEN
         IF n > 2
         THEN
            i := 2 ;
            WHILE i <= n-1 DO
               appendStr (', ') ;
               appendEnum (GetItemFromList (enumList, i)) ;
               INC (i)
            END
         END ;
         appendStr (' and ') ;
         appendEnum (GetItemFromList (enumList, n))
      END
   END
END EnumerateErrors ;


(*
   NoOfSetElements - return the number of set elements.
*)

PROCEDURE NoOfSetElements (set: SetRange) : tree ;
BEGIN
   PushInt (0) ;
   WHILE set # NIL DO
      IF ((set^.low # NIL) AND (set^.high = NIL)) OR
         ((set^.low = NIL) AND (set^.high # NIL))
      THEN
         PushInt (1) ;
         Addn
      ELSIF (set^.low # NIL) AND (set^.high # NIL)
      THEN
         PushIntegerTree (set^.high) ;
         PushIntegerTree (set^.low) ;
         Sub ;
         PushInt (1) ;
         Addn ;
         Addn
      END ;
      set := set^.next
   END ;
   RETURN PopIntegerTree ()
END NoOfSetElements ;


(*
   isPrintableChar - a cautious isprint.
*)

PROCEDURE isPrintableChar (value: tree) : BOOLEAN ;
BEGIN
   CASE CSTIntToChar (value) OF

   'a'..'z':  RETURN TRUE |
   'A'..'Z':  RETURN TRUE |
   '0'..'9':  RETURN TRUE |
   '!', '@':  RETURN TRUE |
   '#', '$':  RETURN TRUE |
   '%', '^':  RETURN TRUE |
   '&', '*':  RETURN TRUE |
   '(', ')':  RETURN TRUE |
   '[', ']':  RETURN TRUE |
   '{', '}':  RETURN TRUE |
   '-', '+':  RETURN TRUE |
   '_', '=':  RETURN TRUE |
   ':', ';':  RETURN TRUE |
   "'", '"':  RETURN TRUE |
   ',', '.':  RETURN TRUE |
   '<', '>':  RETURN TRUE |
   '/', '?':  RETURN TRUE |
   '\', '|':  RETURN TRUE |
   '~', '`':  RETURN TRUE |
   ' '     :  RETURN TRUE

   ELSE
      RETURN FALSE
   END
END isPrintableChar ;


(*
   appendTree - append tree value to the errorString.  It attempts to pretty print
                CHAR constants and will fall back to CHR (x) if necessary.
*)

PROCEDURE appendTree (value: tree; type: CARDINAL) ;
BEGIN
    IF SkipType (GetType (type)) = Char
    THEN
       IF isPrintableChar (value)
       THEN
          IF CSTIntToChar (value) = "'"
          THEN
             appendString (InitStringChar ('"')) ;
             appendString (InitStringChar (CSTIntToChar (value))) ;
             appendString (InitStringChar ('"'))
          ELSE
             appendString (InitStringChar ("'")) ;
             appendString (InitStringChar (CSTIntToChar (value))) ;
             appendString (InitStringChar ("'"))
          END
       ELSE
          appendString (InitString ('CHR (')) ;
          appendString (InitStringCharStar (CSTIntToString (value))) ;
          appendString (InitStringChar (')'))
       END
    ELSE
       appendString (InitStringCharStar (CSTIntToString (value)))
    END
END appendTree ;


(*
   SubrangeErrors - create an errorString containing all set ranges.
*)

PROCEDURE SubrangeErrors (subrangetype: CARDINAL; set: SetRange) ;
VAR
   sr       : SetRange ;
   rangeNo  : CARDINAL ;
   nMissing,
   zero, one: tree ;
BEGIN
   nMissing := NoOfSetElements (set) ;
   PushInt (0) ;
   zero := PopIntegerTree () ;
   IF IsGreater (nMissing, zero)
   THEN
      PushInt (1) ;
      one := PopIntegerTree () ;
      IF IsGreater (nMissing, one)
      THEN
         errorString := InitString ('{%W}there are a total of ')
      ELSE
         errorString := InitString ('{%W}there is a total of ')
      END ;
      appendString (InitStringCharStar (CSTIntToString (nMissing))) ;
      appendStr (' missing values in the subrange, the {%kCASE} statement needs labels (or an {%kELSE} statement)') ;
      appendStr (' for the following values: ') ;
      sr := set ;
      rangeNo := 0 ;
      WHILE sr # NIL DO
         INC (rangeNo) ;
         IF rangeNo > 1
         THEN
            IF sr^.next = NIL
            THEN
               appendStr (' and ')
            ELSE
               appendStr (', ')
            END
         END ;
         IF sr^.low = NIL
         THEN
            appendTree (sr^.high, subrangetype)
         ELSIF (sr^.high = NIL) OR IsEqual (sr^.low, sr^.high)
         THEN
            appendTree (sr^.low, subrangetype)
         ELSE
            appendTree (sr^.low, subrangetype) ;
            appendStr ('..') ;
            appendTree (sr^.high, subrangetype)
         END ;
         sr := sr^.next
      END
   END
END SubrangeErrors ;


(*
   EmitMissingRangeErrors - emits a singular/plural error message for an enumeration type.
*)

PROCEDURE EmitMissingRangeErrors (tokenno: CARDINAL; type: CARDINAL; set: SetRange) ;
BEGIN
   errorString := NIL ;
   IF IsEnumeration (type)
   THEN
      EnumerateErrors (ErrorRanges (type, set))
   ELSIF IsSubrange (type)
   THEN
      SubrangeErrors (type, set)
   END ;
   IF errorString # NIL
   THEN
      MetaErrorStringT0 (tokenno, errorString)
   END
END EmitMissingRangeErrors ;


(*
   MissingCaseBounds - returns true if there were any missing bounds
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
   p := GetIndice (caseArray, c) ;
   missing := FALSE ;
   WITH p^ DO
      IF NOT elseClause
      THEN
         IF (record # NulSym) AND (varient # NulSym)
         THEN
            (* Not a case statement, but a varient record without an else clause.  *)
            type := GetVariantTagType (varient) ;
            set := NewSet (type) ;
            set := ExcludeCaseRanges (set, p) ;
            IF set # NIL
            THEN
               missing := TRUE ;
               MetaErrorT2 (tokenno,
                            'not all variant record alternatives in the {%kCASE} clause are specified, hint you either need to specify each value of {%2ad} or use an {%kELSE} clause',
                            varient, type) ;
               EmitMissingRangeErrors (tokenno, type, set)
            END ;
            set := DisposeRanges (set)
         END
      END
   END ;
   RETURN missing
END MissingCaseBounds ;


(*
   MissingCaseStatementBounds - returns true if the case statement has a missing
                                clause.  It will also generate error messages.
*)

PROCEDURE MissingCaseStatementBounds (tokenno: CARDINAL; c: CARDINAL) : BOOLEAN ;
VAR
   p      : CaseDescriptor ;
   type   : CARDINAL ;
   missing: BOOLEAN ;
   set    : SetRange ;
BEGIN
   p := GetIndice (caseArray, c) ;
   missing := FALSE ;
   WITH p^ DO
      IF NOT elseClause
      THEN
         type := GetCaseExpression (p) ;
         IF type # NulSym
         THEN
            IF IsEnumeration (type) OR IsSubrange (type)
            THEN
               (* A case statement sequence without an else clause but
                  selecting using an enumeration type.  *)
               set := NewSet (type) ;
               set := ExcludeCaseRanges (set, p) ;
               IF set # NIL
               THEN
                  missing := TRUE ;
                  MetaErrorT1 (tokenno,
                               'not all {%1Wd} values in the {%kCASE} statements are specified, hint you either need to specify each value of {%1ad} or use an {%kELSE} clause',
                               type) ;
                  EmitMissingRangeErrors (tokenno, type, set)
               END ;
               set := DisposeRanges (set)
            END
         END
      END
   END ;
   RETURN missing
END MissingCaseStatementBounds ;


(*
   InRangeList - returns true if the value, tag, is defined in the case list.

procedure InRangeList (cl: CaseList; tag: cardinal) : boolean ;
var
   i, h: cardinal ;
   r   : RangePair ;
   a   : tree ;
begin
   with cl^ do
      i := 1 ;
      h := HighIndice(rangeArray) ;
      while i<=h do
         r := GetIndice(rangeArray, i) ;
         with r^ do
            if high=NulSym
            then
               a := Mod2Gcc(low)
            else
               a := Mod2Gcc(high)
            end ;
            if OverlapsRange(Mod2Gcc(low), a, Mod2Gcc(tag), Mod2Gcc(tag))
            then
               return( true )
            end
         end ;
         inc(i)
      end
   end ;
   return( false )
end InRangeList ;
*)


(*
   WriteCase - dump out the case list (internal debugging).
*)

PROCEDURE WriteCase (c: CARDINAL) ;
BEGIN
   (* this debugging PROCEDURE should be finished.  *)
   WriteCard (c, 0)
END WriteCase ;


(*
   checkTypes - checks to see that, constant, and, type, are compatible.
*)

PROCEDURE checkTypes (constant, type: CARDINAL) : BOOLEAN ;
VAR
   consttype: CARDINAL ;
BEGIN
   IF (constant # NulSym) AND IsConst (constant)
   THEN
      consttype := GetType (constant) ;
      IF NOT IsExpressionCompatible (consttype, type)
      THEN
         MetaError2 ('the case statement variant tag {%1ad} must be type compatible with the constant {%2Da:is a {%2d}}',
                     type, constant) ;
         RETURN FALSE
      END
   END ;
   RETURN TRUE
END checkTypes ;


(*
   inRange - returns true if, min <= i <= max.
*)

PROCEDURE inRange (i, min, max: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN OverlapsRange (Mod2Gcc (i), Mod2Gcc (i), Mod2Gcc (min), Mod2Gcc (max))
END inRange ;


(*
   TypeCaseBounds - returns true if all bounds in case list, c, are
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
            INC (j)
         END ;
         INC (i)
      END ;
      RETURN compatible
   END
END TypeCaseBounds ;


BEGIN
   caseStack := NIL ;
   caseId := 0 ;
   caseArray := InitIndex(1) ;
   conflictArray := InitIndex(1) ;
   FreeRangeList := NIL
END M2CaseList.
