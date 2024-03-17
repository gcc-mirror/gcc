(* M2ALU.mod gcc implementation of the M2ALU module.

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

IMPLEMENTATION MODULE M2ALU ;

(*
    Title      : M2ALU.mod
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Jul 10 12:04:50 2000
    Description: gcc implementation of the M2ALU module, this module provides an interface
                 between some of the Modula-2 front end optimization routines and tree
                 construction required so that efficient trees can be passed to gcc's
                 backend. M2ALU allows constant expressions to be calculated.
*)

FROM ASCII IMPORT nul ;
FROM SYSTEM IMPORT WORD ;
FROM NameKey IMPORT KeyToCharStar, MakeKey, CharKey ;
FROM M2Error IMPORT InternalError, FlushErrors ;
FROM M2Debug IMPORT Assert ;
FROM Storage IMPORT ALLOCATE ;
FROM StringConvert IMPORT ostoi, bstoi, stoi, hstoi ;
FROM M2GCCDeclare IMPORT GetTypeMin, GetTypeMax, CompletelyResolved, DeclareConstant ;
FROM M2GenGCC IMPORT PrepareCopyString, StringToChar ;
FROM M2Bitset IMPORT Bitset ;
FROM SymbolConversion IMPORT Mod2Gcc, GccKnowsAbout ;
FROM M2Printf IMPORT printf0, printf2 ;
FROM M2Base IMPORT MixTypes, GetBaseTypeMinMax, Char, IsRealType, IsComplexType, ZType ;
FROM DynamicStrings IMPORT String, InitString, Mark, ConCat, Slice, InitStringCharStar, KillString, InitStringChar, string ;
FROM M2LexBuf IMPORT TokenToLineNo, FindFileNameFromToken, TokenToLocation ;
FROM M2MetaError IMPORT MetaError0, MetaError1, MetaError2, MetaErrorStringT0,
                        MetaErrorT0, MetaErrorT1, MetaErrorT2,  MetaErrorT3 ;

FROM SymbolTable IMPORT NulSym, IsEnumeration, IsSubrange, IsValueSolved, PushValue,
                        ForeachFieldEnumerationDo, MakeTemporary, PutVar, PopValue, GetType,
                        MakeConstLit, GetArraySubscript,
                        IsSet, SkipType, IsRecord, IsArray, IsConst, IsConstructor,
                        IsConstString, SkipTypeAndSubrange, GetDeclaredMod,
                        GetSubrange, GetSymName, GetNth, GetString, GetStringLength,
                        ModeOfAddr ;

IMPORT DynamicStrings ;

FROM m2tree IMPORT Tree ;
FROM m2linemap IMPORT location_t, UnknownLocation ;

FROM m2expr IMPORT BuildAdd, BuildSub, BuildMult,
                   BuildDivTrunc, BuildModTrunc, BuildDivFloor, BuildModFloor,
                   BuildLSL, BuildLSR,
                   BuildLogicalOr, BuildLogicalAnd, BuildSymmetricDifference,
                   GetWordOne, GetCardinalZero, TreeOverflow, RemoveOverflow,
                   GetCstInteger ;

FROM m2decl IMPORT GetBitsPerBitset, BuildIntegerConstant, BuildConstLiteralNumber ;
FROM m2misc IMPORT DebugTree ;

FROM m2type IMPORT RealToTree, Constructor, GetIntegerType, GetLongRealType,
                   BuildStartSetConstructor, BuildSetConstructorElement, BuildEndSetConstructor,
                   BuildRecordConstructorElement, BuildEndRecordConstructor, BuildStartRecordConstructor,
                   BuildNumberOfArrayElements, BuildCharConstant, BuildCharConstantChar,
                   BuildArrayConstructorElement, BuildStartArrayConstructor, BuildEndArrayConstructor,
                   GetM2CharType ;

FROM m2convert IMPORT ConvertConstantAndCheck, ToWord, ToInteger, ToCardinal, ToBitset ;
FROM m2block IMPORT RememberConstant ;

FROM m2expr IMPORT GetPointerZero, GetIntegerZero, GetIntegerOne,
                   CompareTrees, FoldAndStrip, AreRealOrComplexConstantsEqual, AreConstantsEqual ;


TYPE
   cellType   = (none, integer, real, complex, set, constructor, array, record) ;


CONST
   Debugging    = FALSE ;
   DebugGarbage =  TRUE ;

TYPE
   listOfRange = POINTER TO rList ;
   rList       = RECORD
                    low, high: CARDINAL ;  (* symbol table *)
                    next     : listOfRange ;
                 END ;

   listOfFields = POINTER TO fList ;
   fList        = RECORD
                     field   : CARDINAL ;  (* symbol table *)
                     next    : listOfFields ;
                  END ;

   listOfElements = POINTER TO eList ;
   eList          = RECORD
                       element : CARDINAL ;  (* symbol table *)
                       by      : CARDINAL ;  (* symbol table *)
                       next    : listOfElements ;
                    END ;

   PtrToValue = POINTER TO cell ;
   cell       = RECORD
                   location       : location_t ;
                   areAllConstants,
                   solved         : BOOLEAN ;
                   constructorType: CARDINAL ;
                   next           : PtrToValue ;
                   numberValue    : Tree ;

                   CASE type: cellType OF

                   none,
                   integer, real,
                   complex      : |
                   set          : setValue   : listOfRange |
                   constructor,
                   record       : fieldValues: listOfFields |
                   array        : arrayValues: listOfElements

                   END
                END ;

   DoSetProcedure = PROCEDURE (CARDINAL, listOfRange, listOfRange) : listOfRange ;


VAR
   ElementFreeList : listOfElements ;
   FieldFreeList   : listOfFields ;
   RangeFreeList   : listOfRange ;
   FreeList,
   TopOfStack      : PtrToValue ;
   EnumerationValue: Tree ;
   EnumerationField: CARDINAL ;
   CurrentTokenNo  : CARDINAL ;
   (* WatchedValue    : PtrToValue ;  *)


(*
   New - allocate a PtrToValue. Firstly check the FreeList, if empty call upon New.
*)

PROCEDURE New () : PtrToValue ;
VAR
   v: PtrToValue ;
BEGIN
   IF FreeList=NIL
   THEN
      NEW (v)
   ELSE
      v := FreeList ;
      FreeList := FreeList^.next
   END ;
   WITH v^ DO
      numberValue := NIL
   END ;
   RETURN InitRecord (v)
END New ;


(*
   InitRecord - initialize the non variant fields of, v.  Return v.
*)

PROCEDURE InitRecord (v: PtrToValue) : PtrToValue ;
BEGIN
   WITH v^ DO
      location := UnknownLocation () ;
      areAllConstants := FALSE ;
      solved := FALSE ;
      constructorType := NulSym ;
      numberValue := NIL
   END ;
   RETURN v
END InitRecord ;


(*
   NewRange - assigns, v, to a new area of memory.
*)

PROCEDURE NewRange (VAR v: listOfRange) ;
BEGIN
   IF RangeFreeList=NIL
   THEN
      NEW(v) ;
      IF v=NIL
      THEN
         InternalError ('out of memory error')
      END
   ELSE
      v := RangeFreeList ;
      RangeFreeList := RangeFreeList^.next
   END
END NewRange ;


(*
   DisposeRange - adds the list, v, to the free list.
*)

PROCEDURE DisposeRange (VAR v: listOfRange) ;
VAR
   r: listOfRange ;
BEGIN
   IF v#NIL
   THEN
      r := v ;
      WHILE (r#NIL) AND (r^.next#NIL) DO
         r := r^.next
      END ;
      IF r#NIL
      THEN
         r^.next := RangeFreeList
      END ;
      RangeFreeList := v ;
      v := NIL
   END
END DisposeRange ;


(*
   IsOnFieldFreeList - returns TRUE if, r, is on the FieldFreeList.
*)

PROCEDURE IsOnFieldFreeList (r: listOfFields) : BOOLEAN ;
VAR
   s: listOfFields ;
BEGIN
   s := FieldFreeList ;
   WHILE s#NIL DO
      IF s=r
      THEN
         RETURN( TRUE )
      ELSE
         s := s^.next
      END
   END ;
   RETURN( FALSE )
END IsOnFieldFreeList ;


(*
   IsOnElementFreeList - returns TRUE if, r, is on the ElementFreeList.
*)

PROCEDURE IsOnElementFreeList (r: listOfElements) : BOOLEAN ;
VAR
   s: listOfElements ;
BEGIN
   s := ElementFreeList ;
   WHILE s#NIL DO
      IF s=r
      THEN
         RETURN( TRUE )
      ELSE
         s := s^.next
      END
   END ;
   RETURN( FALSE )
END IsOnElementFreeList ;


(*
   DisposeFields - adds the list, v, to the free list.
*)

PROCEDURE DisposeFields (VAR v: listOfFields) ;
VAR
   r: listOfFields ;
BEGIN
   IF v#NIL
   THEN
      r := v ;
      WHILE r^.next#NIL DO
         Assert(NOT IsOnFieldFreeList(r)) ;
         r := r^.next
      END ;
      r^.next := FieldFreeList ;
      FieldFreeList := v ;
      v := NIL
   END
END DisposeFields ;


(*
   NewField - adds the list, v, to the free list.
*)

PROCEDURE NewField (VAR v: listOfFields) ;
BEGIN
   IF FieldFreeList=NIL
   THEN
      NEW(v) ;
      IF v=NIL
      THEN
         InternalError ('out of memory error')
      END
   ELSE
      v := FieldFreeList ;
      FieldFreeList := FieldFreeList^.next
   END
END NewField ;


(*
   NewElement - returns a new element record.
*)

PROCEDURE NewElement (VAR e: listOfElements) ;
BEGIN
   IF ElementFreeList=NIL
   THEN
      NEW(e) ;
      IF e=NIL
      THEN
         InternalError ('out of memory error')
      END
   ELSE
      e := ElementFreeList ;
      ElementFreeList := ElementFreeList^.next
   END
END NewElement ;


(*
   DisposeElements - returns the list, e, to the free list.
*)

PROCEDURE DisposeElements (VAR e: listOfElements) ;
VAR
   r: listOfElements ;
BEGIN
   IF e#NIL
   THEN
      r := e ;
      WHILE r^.next#NIL DO
         Assert(NOT IsOnElementFreeList(r)) ;
         r := r^.next
      END ;
      r^.next := ElementFreeList ;
      ElementFreeList := e ;
      e := NIL
   END
END DisposeElements ;


(*
   CheckNotAlreadyOnFreeList - checks to see whether, v, is already on the free list
                               and aborts if this is the case.
*)

PROCEDURE CheckNotAlreadyOnFreeList (v: PtrToValue) ;
VAR
   l: PtrToValue ;
BEGIN
   IF DebugGarbage
   THEN
      l := FreeList ;
      WHILE l#NIL DO
         IF l=v
         THEN
            InternalError ('value is already on the free list')
         END ;
         l := l^.next
      END
   END
END CheckNotAlreadyOnFreeList ;


(*
   CheckNotOnStack - checks to see whether, v, is already on the stack
                     and aborts if this is the case.
*)

PROCEDURE CheckNotOnStack (v: PtrToValue) ;
VAR
   l: PtrToValue ;
BEGIN
   IF DebugGarbage
   THEN
      l := TopOfStack ;
      WHILE l#NIL DO
         IF l=v
         THEN
            InternalError ('value is already on the stack')
         END ;
         l := l^.next
      END
   END
END CheckNotOnStack ;


(*
   Dispose - place, v, onto the FreeList.
*)

PROCEDURE Dispose (v: PtrToValue) ;
BEGIN
   CheckNotAlreadyOnFreeList(v) ;
   CheckNotOnStack(v) ;
   CASE v^.type OF

   set        :  DisposeRange(v^.setValue) |
   constructor,
   record     :  DisposeFields(v^.fieldValues) |
   array      :  DisposeElements(v^.arrayValues)

   ELSE
   END ;
   v^.next := FreeList ;
   FreeList := v
END Dispose ;


(*
   AddRange - returns a ListOfRange which is prepended to the front of the current list.
*)

PROCEDURE AddRange (head: listOfRange; l, h: CARDINAL) : listOfRange ;
VAR
   r: listOfRange ;
BEGIN
   NewRange(r) ;
   WITH r^ DO
      low  := l ;
      high := h ;
      next := head
   END ;
   RETURN( r )
END AddRange ;


(*
   DupRange - duplicates and returns the list, t.
*)

PROCEDURE DupRange (r: listOfRange) : listOfRange ;
VAR
   s: listOfRange ;
BEGIN
   s := NIL ;
   WHILE r#NIL DO
      s := AddRange(s, r^.low, r^.high) ;
      r := r^.next
   END ;
   RETURN( s )
END DupRange ;


(*
   InitValue - initializes and returns a memory cell.
*)

PROCEDURE InitValue () : PtrToValue ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   IF v=NIL
   THEN
      InternalError ('out of memory error')
   ELSE
      WITH v^ DO
         location        := UnknownLocation () ;
         type            := none ;
         areAllConstants := TRUE ;
         solved          := FALSE ;
         next            := NIL ;
         constructorType := NulSym
      END ;
      RETURN( v )
   END
END InitValue ;


(*
   IsValueTypeNone - returns TRUE if the value on the top stack has no value.
*)

PROCEDURE IsValueTypeNone () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=none
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeNone ;


(*
   IsValueTypeInteger - returns TRUE if the value on the top stack is an integer.
*)

PROCEDURE IsValueTypeInteger () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeInteger ;


(*
   IsValueTypeReal - returns TRUE if the value on the top stack is a real.
*)

PROCEDURE IsValueTypeReal () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=real
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeReal ;


(*
   IsValueTypeComplex - returns TRUE if the value on the top stack is a complex.
*)

PROCEDURE IsValueTypeComplex () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=complex
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeComplex ;


(*
   IsValueTypeSet - returns TRUE if the value on the top stack is a set.
*)

PROCEDURE IsValueTypeSet () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=set
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeSet ;


(*
   IsValueTypeConstructor - returns TRUE if the value on the top
                            stack is a constructor.
*)

PROCEDURE IsValueTypeConstructor () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=constructor
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeConstructor ;


(*
   IsValueTypeArray - returns TRUE if the value on the top stack is
                      an array.
*)

PROCEDURE IsValueTypeArray () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=array
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeArray ;


(*
   IsValueTypeRecord - returns TRUE if the value on the top stack is
                       a record.
*)

PROCEDURE IsValueTypeRecord () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=record
      THEN
         Push(v) ;
         RETURN( TRUE )
      ELSE
         Push(v) ;
         RETURN( FALSE )
      END
   END
END IsValueTypeRecord ;


(*
   GetSetValueType - returns the set type on top of the ALU stack.
*)

PROCEDURE GetSetValueType () : CARDINAL ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   Push(v) ;
   WITH v^ DO
      IF type=set
      THEN
         RETURN( constructorType )
      ELSE
         InternalError ('expecting set type')
      END
   END
END GetSetValueType ;


(*
   PushIntegerTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushIntegerTree (t: Tree) ;
VAR
   v: PtrToValue ;
BEGIN
   v := InitValue() ;
   WITH v^ DO
      type            := integer ;
      numberValue     := t ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushIntegerTree ;


(*
   PopIntegerTree - pops a gcc tree value from the ALU stack.
*)

PROCEDURE PopIntegerTree () : Tree ;
VAR
   v: PtrToValue ;
   t: Tree ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         t := numberValue
      ELSE
         InternalError ('expecting type of constant to be a whole number')
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopIntegerTree ;


(*
   PushRealTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushRealTree (t: Tree) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type            := real ;
      numberValue     := t ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushRealTree ;


(*
   PopRealTree - pops a gcc tree value from the ALU stack.
*)

PROCEDURE PopRealTree () : Tree ;
VAR
   v: PtrToValue ;
   t: Tree ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=real
      THEN
         t := numberValue
      ELSE
         InternalError ('expecting type of constant to be a real number')
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopRealTree ;


(*
   PushComplexTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushComplexTree (t: Tree) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type            := complex ;
      numberValue     := t ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushComplexTree ;


(*
   PopComplexTree - pops a gcc tree value from the ALU stack.
*)

PROCEDURE PopComplexTree () : Tree ;
VAR
   v: PtrToValue ;
   t: Tree ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=complex
      THEN
         t := numberValue
      ELSE
         InternalError ('expecting type of constant to be a complex number')
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopComplexTree ;


(*
   PushSetTree - pushes a gcc tree onto the ALU stack.
                 The tree, t, is expected to contain a
                 word value. It is converted into a set
                 type (sym). Bit 0 maps onto MIN(sym).
*)

PROCEDURE PushSetTree (tokenno: CARDINAL;
                       t: Tree; sym: CARDINAL) ;
VAR
   v: PtrToValue ;
   c,
   i: INTEGER ;
   r: listOfRange ;
   l: location_t ;
BEGIN
   l := TokenToLocation(tokenno) ;
   r := NIL ;
   i := 0 ;
   WHILE (i<GetBitsPerBitset()) AND
         (CompareTrees(GetIntegerZero(l), t)#0) DO
      IF CompareTrees(GetIntegerOne(l),
                      BuildLogicalAnd(l, t, GetIntegerOne(l), FALSE))=0
      THEN
         PushCard(i) ;
         c := Val(tokenno, SkipType(sym), PopIntegerTree()) ;
         DeclareConstant(tokenno, c) ;
         r := AddRange(r, c, c)
      END ;
      t := BuildLSR(l, t, GetIntegerOne(l), FALSE) ;
      INC(i)
   END ;
   SortElements(tokenno, r) ;
   CombineElements(tokenno, r) ;
   v := New() ;
   WITH v^ DO
      location        := l ;
      type            := set ;
      constructorType := sym ;
      areAllConstants := FALSE ;
      solved          := FALSE ;
      setValue        := r
   END ;
   Eval(tokenno, v) ;
   Push(v)
END PushSetTree ;


(*
   PopSetTree - pops a gcc tree from the ALU stack.
*)

PROCEDURE PopSetTree (tokenno: CARDINAL) : Tree ;
VAR
   v: PtrToValue ;
   t: Tree ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=set
      THEN
         Eval(tokenno, v) ;
         IF NOT v^.solved
         THEN
            InternalError ('the set has not been resolved')
         END ;
         IF NOT v^.areAllConstants
         THEN
            InternalError ('the set must only contain constants')
         END ;
         t := ConstructSetConstant(tokenno, v)
      ELSE
         InternalError ('expecting type of constant to be a set')
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopSetTree ;


(*
   PopConstructorTree - returns a tree containing the compound literal.
*)

PROCEDURE PopConstructorTree (tokenno: CARDINAL) : Tree ;
VAR
   v: PtrToValue ;
   t: Tree ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      Eval(tokenno, v) ;
      IF NOT v^.solved
      THEN
         InternalError ('the constructor has not been resolved')
      END ;
      CASE type OF

      constructor:  InternalError('expecting constructor to be resolved into specific type') |
      array      :  t := ConstructArrayConstant(tokenno, v) |
      record     :  t := ConstructRecordConstant(tokenno, v) |
      set        :  t := ConstructSetConstant(tokenno, v)

      ELSE
         InternalError ('expecting type to be a constructor')
      END
   END ;
   Dispose(v) ;
   RETURN( t )
END PopConstructorTree ;


(*
   Pop - pops and returns top element from the stack.
*)

PROCEDURE Pop () : PtrToValue ;
VAR
   v: PtrToValue ;
BEGIN
   IF TopOfStack=NIL
   THEN
      InternalError ('stack underflow error')
   ELSE
      v          := TopOfStack ;
      TopOfStack := TopOfStack^.next
   END ;
   CheckNotAlreadyOnFreeList(v) ;
   RETURN( v )
END Pop ;


(*
   Push - pushes the value onto the stack.
*)

PROCEDURE Push (v: PtrToValue) ;
BEGIN
   CheckNotAlreadyOnFreeList(v) ;
   CheckNotOnStack(v) ;
   v^.next    := TopOfStack ;
   TopOfStack := v
END Push ;


(*
   Reduce - remove the top element of the stack.
*)

PROCEDURE Reduce ;
BEGIN
   Dispose (Pop ())
END Reduce ;


(*
   PrintValue - debugging procedure to display the value on the top of the stack.
*)

PROCEDURE PrintValue ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         DebugTree(numberValue)
      END
   END ;
   Push(v)
END PrintValue ;


(*
   DupFields - duplicates the field list in order.
*)

PROCEDURE DupFields (f: listOfFields) : listOfFields ;
VAR
   p, q, l: listOfFields ;
BEGIN
   p := NIL ;
   l := NIL ;
   WHILE f#NIL DO
      NewField(q) ;
      IF p=NIL
      THEN
         p := q
      END ;
      q^.field := f^.field ;
      q^.next := NIL ;
      IF l#NIL
      THEN
         l^.next := q
      END ;
      l := q ;
      f := f^.next
   END ;
   RETURN( p )
END DupFields ;


(*
   DupElements - duplicates the array list in order.
*)

PROCEDURE DupElements (f: listOfElements) : listOfElements ;
VAR
   p, q, l: listOfElements ;
BEGIN
   p := NIL ;
   l := NIL ;
   WHILE f#NIL DO
      NewElement(q) ;
      IF p=NIL
      THEN
         p := q
      END ;
      q^.element := f^.element ;
      q^.by := f^.by ;
      q^.next := NIL ;
      IF l#NIL
      THEN
         l^.next := q
      END ;
      l := q ;
      f := f^.next
   END ;
   RETURN( p )
END DupElements ;


(*
   PushFrom - pushes a copy of the contents of, v, onto stack.
*)

PROCEDURE PushFrom (v: PtrToValue) ;
VAR
   t: PtrToValue ;
BEGIN
   CheckNotAlreadyOnFreeList(v) ;
   t := New() ;     (* as it is a copy *)
   t^ := v^ ;
   CASE v^.type OF

   set        :  t^.setValue := DupRange(v^.setValue) |
   constructor,
   record     :  t^.fieldValues := DupFields(v^.fieldValues) |
   array      :  t^.arrayValues := DupElements(v^.arrayValues)

   ELSE
   END ;
   Push(t)
END PushFrom ;


(*
   PopInto - pops the top element from the stack and places it into, v.
*)

PROCEDURE PopInto (v: PtrToValue) ;
VAR
   t: PtrToValue ;
BEGIN
   t := Pop() ;
   v^ := t^ ;
   CASE v^.type OF

   set        :  t^.setValue := NIL |
   record,
   constructor:  t^.fieldValues := NIL |
   array      :  t^.arrayValues := NIL |
   none,
   integer,
   real,
   complex    :  v^.numberValue := RememberConstant(FoldAndStrip(t^.numberValue))

   ELSE
      InternalError ('not expecting this value')
   END ;
   Dispose(t)
END PopInto ;


(*
   PushCard - pushes a cardinal onto the stack.
*)

PROCEDURE PushCard (c: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type            := integer ;
      numberValue     := BuildIntegerConstant(INTEGER(c)) ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushCard ;


(*
   PushInt - pushes an integer onto the stack.
*)

PROCEDURE PushInt (i: INTEGER) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type            := integer ;
      numberValue     := BuildIntegerConstant(i) ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushInt ;


(*
   PushChar - pushes a char onto the stack.
*)

PROCEDURE PushChar (c: CHAR) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      type            := integer ;
      numberValue     := BuildIntegerConstant(ORD(c)) ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushChar ;


(*
   PopChar - pops a char from the stack.
*)

PROCEDURE PopChar (tokenno: CARDINAL) : CHAR ;
VAR
   v : PtrToValue ;
   ch: CHAR ;
BEGIN
   v := Pop () ;
   ch := 0C ;
   WITH v^ DO
      IF type = integer
      THEN
         ch := VAL (CHAR, GetCstInteger (numberValue))
      ELSE
         MetaErrorT0 (tokenno, '{%E}cannot convert constant to a CHAR')
      END
   END ;
   Push (v) ;
   RETURN ch
END PopChar ;


(*
   IsReal - returns TRUE if a is a REAL number.
*)

PROCEDURE IsReal (a: DynamicStrings.String) : BOOLEAN ;
BEGIN
   RETURN( DynamicStrings.Index(a, '.', 0)#-1 )
END IsReal ;


(*
   PushString - pushes the numerical value of the string onto the stack.
*)

PROCEDURE PushString (tokenno: CARDINAL; s: Name; issueError: BOOLEAN) ;
VAR
   ch      : CHAR ;
   a, b    : DynamicStrings.String ;
   length  : CARDINAL ;
   location: location_t ;
BEGIN
   a := DynamicStrings.InitStringCharStar (KeyToCharStar (s)) ;
   b := NIL ;
   length := DynamicStrings.Length (a) ;
   IF length>0
   THEN
      DEC (length) ;
      ch := DynamicStrings.char (a, length) ;
      location := TokenToLocation (tokenno) ;
      CASE ch OF

      'H': (* hexadecimal *)
           b := DynamicStrings.Slice (a, 0, -1) ;
           PushIntegerTree (BuildConstLiteralNumber (location,
                                                     DynamicStrings.string (b),
                                                     16, issueError)) |
      'A': (* binary *)
           b := DynamicStrings.Slice (a, 0, -1) ;
           PushIntegerTree (BuildConstLiteralNumber (location,
                                                     DynamicStrings.string (b),
                                                     2, issueError)) |
      'C', (* --fixme-- question:
              should we type this as a char rather than an int? *)
      'B': (* octal *)
           b := DynamicStrings.Slice (a, 0, -1) ;
           PushIntegerTree (BuildConstLiteralNumber (location,
                                                     DynamicStrings.string (b),
                                                     8, issueError))

      ELSE
         IF IsReal (a)
         THEN
            PushRealTree (RealToTree (KeyToCharStar (s)))
         ELSE
            PushIntegerTree (BuildConstLiteralNumber (location, KeyToCharStar (s),
                                                      10, issueError))
         END
      END
   ELSE
      InternalError ('expecting constant literal')
   END ;
   a := DynamicStrings.KillString (a) ;
   b := DynamicStrings.KillString (b)
END PushString ;


(*
   IsSolvedGCC - returns TRUE if the value, sym, is solved.
                 If TRUE then it also ensures this symbol is
                 entered into the double book keeping table
                 for GM2 <-> GCC.
*)

PROCEDURE IsSolvedGCC (sym: CARDINAL) : BOOLEAN ;
BEGIN
   IF IsValueSolved(sym)
   THEN
      IF NOT GccKnowsAbout(sym)
      THEN
         DeclareConstant(GetDeclaredMod(sym), sym)
      END ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END IsSolvedGCC ;


(*
   CoerseLongRealToCard - performs a coersion between a REAL to a CARDINAL
*)

PROCEDURE CoerseLongRealToCard ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=real
      THEN
         numberValue     := ConvertConstantAndCheck(location, GetIntegerType(), numberValue) ;
         type            := integer ;
         areAllConstants := TRUE ;
         solved          := TRUE
      ELSE
         InternalError ('expecting a REAL number')
      END
   END ;
   Push(v)
END CoerseLongRealToCard ;


(*
   ConvertRealToInt - converts a REAL into an INTEGER
*)

PROCEDURE ConvertRealToInt ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=real
      THEN
         numberValue     := ConvertConstantAndCheck(location, GetIntegerType(), numberValue) ;
         type            := integer ;
         areAllConstants := TRUE ;
         solved          := TRUE
      ELSE
         InternalError ('expecting a REAL number')
      END
   END ;
   Push(v)
END ConvertRealToInt ;


(*
   ConvertIntToReal - converts a INTEGER into a LONGREAL
*)

PROCEDURE ConvertIntToReal ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         numberValue     := ConvertConstantAndCheck(location, GetLongRealType(), numberValue) ;
         type            := real ;
         areAllConstants := TRUE ;
         solved          := TRUE
      ELSE
         InternalError ('expecting an INTEGER number')
      END
   END ;
   Push(v)
END ConvertIntToReal ;


(*
   ConvertToInt - converts the value into an INTEGER. This should be used
                  if we are computing the number of elements in a char set to
                  avoid an overflow.
*)

PROCEDURE ConvertToInt ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      IF type=integer
      THEN
         numberValue     := ConvertConstantAndCheck(location, GetIntegerType(), numberValue) ;
         solved          := TRUE ;
         areAllConstants := TRUE
      ELSE
         InternalError ('expecting an INTEGER number')
      END
   END ;
   Push(v)
END ConvertToInt ;


(*
   ConvertToType - converts the top of stack to type, t.
*)

PROCEDURE ConvertToType (t: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   IF t#NulSym
   THEN
      WITH v^ DO
         IF type=integer
         THEN
            numberValue     := ConvertConstantAndCheck(location, Mod2Gcc(t), numberValue) ;
            solved          := TRUE ;
            areAllConstants := TRUE
         ELSE
            InternalError ('expecting an INTEGER number')
         END
      END
   END ;
   Push(v)
END ConvertToType ;



(*
   IsSolved - returns true if the memory cell indicated by v
              has a known value.
*)

PROCEDURE IsSolved (v: PtrToValue) : BOOLEAN ;
BEGIN
   IF v=NIL
   THEN
      InternalError ('uninitialized value')
   ELSE
      RETURN( v^.solved )
   END
END IsSolved ;


(*
   IsValueConst - returns true if the memory cell indicated by v
                  is only defined by constants.  For example
                  no variables are used in the constructor.
*)

PROCEDURE IsValueConst (v: PtrToValue) : BOOLEAN ;
BEGIN
   IF v=NIL
   THEN
      InternalError ('uninitialized value')
   ELSE
      RETURN( v^.areAllConstants )
   END
END IsValueConst ;


(*
   EitherReal - returns TRUE if either, Op1, or, Op2, are Real.
*)

PROCEDURE EitherReal (Op1, Op2: PtrToValue) : BOOLEAN ;
BEGIN
   RETURN( (Op1^.type=real) OR (Op2^.type=real) )
END EitherReal ;


(*
   EitherComplex - returns TRUE if either, Op1, or, Op2, are Real.
*)

PROCEDURE EitherComplex (Op1, Op2: PtrToValue) : BOOLEAN ;
BEGIN
   RETURN( (Op1^.type=complex) OR (Op2^.type=complex) )
END EitherComplex ;


(*
   Add - adds the top two elements on the stack.

         The Stack:

         Entry             Exit

  Ptr ->
         +------------+
         | Op1        |                   <- Ptr
         |------------|    +------------+
         | Op2        |    | Op2 + Op1  |
         |------------|    |------------|
*)

PROCEDURE Addn ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealAdd(Op1, Op2)
   ELSIF EitherComplex(Op1, Op2)
   THEN
      ComplexAdd(Op1, Op2)
   ELSE
      Temp := New() ;    (* as it is a temp *)
      WITH Temp^ DO
         location     := Op1^.location ;
         type         := integer ;
         numberValue  := BuildAdd(location, Op1^.numberValue, Op2^.numberValue, FALSE) ;
         solved       := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Addn ;


(*
   RealAdd - adds two numbers. One of which is a Real.
*)

PROCEDURE RealAdd (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;
   WITH Temp^ DO
      location    := Op1^.location ;
      numberValue := BuildAdd(location, Op1^.numberValue, Op2^.numberValue, FALSE) ;
      type        := real ;
      solved      := TRUE
   END ;
   Push(Temp)
END RealAdd ;


(*
   ComplexAdd - adds two complex numbers.
*)

PROCEDURE ComplexAdd (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF (Op1^.type=complex) AND (Op2^.type=complex)
   THEN
      Temp := New() ;
      WITH Temp^ DO
         location    := Op1^.location ;
         numberValue := BuildAdd(location, Op1^.numberValue, Op2^.numberValue, FALSE) ;
         type        := complex ;
         solved      := TRUE
      END ;
      Push(Temp)
   ELSE
      InternalError ('expecting both operands to be of type COMPLEX')
   END
END ComplexAdd ;


(*
   Sub - subtracts the top two elements on the stack.

         The Stack:

         Entry             Exit

  Ptr ->
         +------------+
         | Op1        |                   <- Ptr
         |------------|    +------------+
         | Op2        |    | Op2 - Op1  |
         |------------|    |------------|
*)

PROCEDURE Sub ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealSub(Op1, Op2)
   ELSIF EitherComplex(Op1, Op2)
   THEN
      ComplexSub(Op1, Op2)
   ELSE
      Temp := New() ;      (* as it is a temp *)
      WITH Temp^ DO
         location    := Op1^.location ;
         type        := integer ;
         numberValue := BuildSub(location, Op2^.numberValue, Op1^.numberValue, TRUE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Sub ;


(*
   RealSub - subtracts two numbers. One of which is a Real.
*)

PROCEDURE RealSub (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;
   WITH Temp^ DO
      location    := Op1^.location ;
      numberValue := BuildSub(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
      type        := real ;
      solved      := TRUE
   END ;
   Push(Temp)
END RealSub ;


(*
   ComplexSub - subtracts two complex numbers.
*)

PROCEDURE ComplexSub (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF (Op1^.type=complex) AND (Op2^.type=complex)
   THEN
      Temp := New() ;
      WITH Temp^ DO
         location    := Op1^.location ;
         numberValue := BuildSub(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
         type        := complex ;
         solved      := TRUE
      END ;
      Push(Temp)
   ELSE
      InternalError ('expecting both operands to be of type COMPLEX')
   END
END ComplexSub ;


(*
   Mult - multiplies the top two elements on the stack.

          The Stack:

          Entry             Exit

   Ptr ->
          +------------+
          | Op1        |                   <- Ptr
          |------------|    +------------+
          | Op2        |    | Op2 * Op1  |
          |------------|    |------------|
*)

PROCEDURE Multn ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealMult(Op1, Op2)
   ELSIF EitherComplex(Op1, Op2)
   THEN
      ComplexMult(Op1, Op2)
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         location    := Op1^.location ;
         type        := integer ;
         numberValue := BuildMult(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END Multn ;


(*
   RealMult - multiplies two numbers. One of which is a Real.
*)

PROCEDURE RealMult (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;     (* as it is a temp *)
   WITH Temp^ DO
      location    := Op1^.location ;
      numberValue := BuildMult(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
      type        := real ;
      solved      := TRUE
   END ;
   Push(Temp)
END RealMult ;


(*
   ComplexMult - multiplies two complex numbers.
*)

PROCEDURE ComplexMult (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF (Op1^.type=complex) AND (Op2^.type=complex)
   THEN
      Temp := New() ;
      WITH Temp^ DO
         location    := Op1^.location ;
         numberValue := BuildMult(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
         type        := complex ;
         solved      := TRUE
      END ;
      Push(Temp)
   ELSE
      InternalError ('expecting both operands to be of type COMPLEX')
   END
END ComplexMult ;


(*
   DivTrunc - divides the top two elements on the stack.

              The Stack:

              Entry             Exit

       Ptr ->
              +------------+
              | Op1        |                     <- Ptr
              |------------|    +--------------+
              | Op2        |    | Op2 DIV Op1  |
              |------------|    |--------------|
*)

PROCEDURE DivTrunc ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealDiv(Op1, Op2)
   ELSIF EitherComplex(Op1, Op2)
   THEN
      ComplexDiv(Op1, Op2)
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         location    := Op1^.location ;
         type        := integer ;
         numberValue := BuildDivTrunc(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END DivTrunc ;


(*
   DivFloor - divides the top two elements on the stack.

              The Stack:

              Entry             Exit

       Ptr ->
              +------------+
              | Op1        |                     <- Ptr
              |------------|    +--------------+
              | Op2        |    | Op2 DIV Op1  |
              |------------|    |--------------|
*)

PROCEDURE DivFloor ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      RealDiv(Op1, Op2)
   ELSIF EitherComplex(Op1, Op2)
   THEN
      ComplexDiv(Op1, Op2)
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         location    := Op1^.location ;
         type        := integer ;
         numberValue := BuildDivFloor(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END DivFloor ;


(*
   RealDiv - divides two numbers. One of which is a Real.
*)

PROCEDURE RealDiv (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF Op1^.type=integer
   THEN
      Push(Op1) ;
      ConvertIntToReal ;
      Op1 := Pop()
   END ;
   IF Op2^.type=integer
   THEN
      Push(Op2) ;
      ConvertIntToReal ;
      Op2 := Pop()
   END ;
   Temp := New() ;     (* as it is a temp *)
   WITH Temp^ DO
      location    := Op1^.location ;
      numberValue := BuildDivTrunc(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
      type        := real ;
      solved      := TRUE
   END ;
   Push(Temp)
END RealDiv ;


(*
   ComplexDiv - divides two complex numbers.
*)

PROCEDURE ComplexDiv (Op1, Op2: PtrToValue) ;
VAR
   Temp: PtrToValue ;
BEGIN
   IF (Op1^.type=complex) AND (Op2^.type=complex)
   THEN
      Temp := New() ;
      WITH Temp^ DO
         location    := Op1^.location ;
         numberValue := BuildDivTrunc(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
         type        := complex ;
         solved      := TRUE
      END ;
      Push(Temp)
   ELSE
      InternalError ('expecting both operands to be of type COMPLEX')
   END
END ComplexDiv ;


(*
   ModFloor - modulus of the top two elements on the stack.

              The Stack:

              Entry             Exit

       Ptr ->
              +------------+
              | Op1        |                     <- Ptr
              |------------|    +--------------+
              | Op2        |    | Op2 MOD Op1  |
              |------------|    |--------------|
*)

PROCEDURE ModFloor ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      MetaError0 ('cannot perform {%EkMOD} on REAL types')
   ELSIF EitherComplex(Op1, Op2)
   THEN
      MetaError0 ('cannot perform {%EkMOD} on COMPLEX types')
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         location    := Op1^.location ;
         type        := integer ;
         numberValue := BuildModFloor(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END ModFloor ;


(*
   ModTrunc - modulus of the top two elements on the stack.

              The Stack:

              Entry             Exit

       Ptr ->
              +------------+
              | Op1        |                     <- Ptr
              |------------|    +--------------+
              | Op2        |    | Op2 MOD Op1  |
              |------------|    |--------------|
*)

PROCEDURE ModTrunc ;
VAR
   Temp,
   Op1, Op2: PtrToValue ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF EitherReal(Op1, Op2)
   THEN
      MetaError0 ('cannot perform {%EkMOD} on REAL types')
   ELSIF EitherComplex(Op1, Op2)
   THEN
      MetaError0 ('cannot perform {%EkMOD} on COMPLEX types')
   ELSE
      Temp := New() ;     (* as it is a temp *)
      WITH Temp^ DO
         location    := Op1^.location ;
         type        := integer ;
         numberValue := BuildModTrunc(location, Op2^.numberValue, Op1^.numberValue, FALSE) ;
         solved      := TRUE
      END ;
      Push(Temp)
   END ;
   Dispose(Op1) ;
   Dispose(Op2)
END ModTrunc ;


(*
   AreSetsEqual - returns TRUE if sets, op1, and, op2, contain the same
                  members.
*)

PROCEDURE AreSetsEqual (tokenno: CARDINAL; op1, op2: PtrToValue) : BOOLEAN ;
VAR
   low1, low2,
   high1, high2: CARDINAL ;
   i           : CARDINAL ;
BEGIN
   i := 1 ;
   Eval(tokenno, op1) ;
   Eval(tokenno, op2) ;
   IF NOT (op1^.solved AND op2^.solved)
   THEN
      InternalError ('can only compare set values when they are known')
   END ;
   LOOP
      IF GetRange(op1, i, low1, high1)
      THEN
         IF GetRange(op2, i, low2, high2)
         THEN
            PushValue(low1) ;
            PushValue(low2) ;
            IF NotEqu(tokenno)
            THEN
               RETURN( FALSE )
            END ;
            PushValue(high1) ;
            PushValue(high2) ;
            IF NotEqu(tokenno)
            THEN
               RETURN( FALSE )
            END ;
            INC(i)
         ELSE
            (* op2 is out of ranges, but op1 still has >= 1 range left *)
            RETURN( FALSE )
         END
      ELSE
         IF GetRange(op2, i, low2, high2)
         THEN
            (* op1 is out of ranges, but op2 still has >= 1 range left *)
            RETURN( FALSE )
         ELSE
            (* both out of ranges and they were the same *)
            RETURN( TRUE )
         END
      END
   END
END AreSetsEqual ;


(*
   Equ - returns true if the top two elements on the stack
         are identical.

         The Stack:

         Entry             Exit

  Ptr ->
         +------------+
         | Op1        |
         |------------|
         | Op2        |
         |------------|    Empty

         RETURN( Op2 = Op1 )
*)

PROCEDURE Equ (tokenno: CARDINAL) : BOOLEAN ;
VAR
   Op1, Op2: PtrToValue ;
   result  : BOOLEAN ;
BEGIN
   Op1 := Pop() ;
   Op2 := Pop() ;
   IF (Op1^.type=set) AND (Op2^.type=set)
   THEN
      result := AreSetsEqual(tokenno, Op1, Op2)
   ELSIF (Op1^.type=set) OR (Op2^.type=set)
   THEN
      MetaErrorT0 (tokenno, 'cannot perform a comparison between a number and a set') ;
      result := FALSE
   ELSE
      IF Op1^.type#Op2^.type
      THEN
         MetaErrorT0 (tokenno, 'cannot perform a comparison between a different type constants') ;
         result := FALSE
      ELSIF (Op1^.type=complex) OR (Op1^.type=real)
      THEN
         result := AreRealOrComplexConstantsEqual(Op1^.numberValue, Op2^.numberValue)
      ELSE
         result := AreConstantsEqual(Op1^.numberValue, Op2^.numberValue)
      END
   END ;
   Dispose(Op1) ;
   Dispose(Op2) ;
   RETURN( result )
END Equ ;


(*
   NotEqu - returns true if the top two elements on the stack
            are not identical.

            The Stack:

            Entry             Exit

     Ptr ->
            +------------+
            | Op1        |
            |------------|
            | Op2        |
            |------------|    Empty

            RETURN( Op2 # Op1 )
*)

PROCEDURE NotEqu (tokenno: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( NOT Equ(tokenno) )
END NotEqu ;


(*
   Less - returns true if Op2 < Op1

          The Stack:

          Entry             Exit

   Ptr ->
          +------------+
          | Op1        |
          |------------|
          | Op2        |
          |------------|    Empty

          RETURN( Op2 < Op1 )
*)

PROCEDURE Less (tokenno: CARDINAL) : BOOLEAN ;
VAR
   v1, v2: PtrToValue ;
   result: BOOLEAN ;
   res   : INTEGER ;
BEGIN
   v1 := Pop () ;
   v2 := Pop () ;
   IF (v1^.type = set) AND (v2^.type = set)
   THEN
      result := NOT IsSuperset (tokenno, v2, v1)
   ELSIF (v1^.type = set) OR (v2^.type = set)
   THEN
      MetaErrorT0 (tokenno, 'cannot perform a comparison between a number and a set') ;
      result := FALSE
   ELSE
      res := CompareTrees (v2^.numberValue, v1^.numberValue) ;
      IF res = -1
      THEN
         result := TRUE
      ELSE
         result := FALSE
      END ;
      (* result := (CompareTrees(v2^.numberValue, v1^.numberValue)=-1) *)
   END ;
   Dispose (v1) ;
   Dispose (v2) ;
   RETURN result
END Less ;


(*
   Gre - returns true if Op2 > Op1

         The Stack:

         Entry             Exit

  Ptr ->
         +------------+
         | Op1        |
         |------------|
         | Op2        |
         |------------|    Empty

         RETURN( Op2 > Op1 )
*)

PROCEDURE Gre (tokenno: CARDINAL) : BOOLEAN ;
VAR
   v1, v2: PtrToValue ;
   result: BOOLEAN ;
BEGIN
   v1 := Pop() ;
   v2 := Pop() ;
   IF (v1^.type=set) AND (v2^.type=set)
   THEN
      result := NOT IsSubset(tokenno, v2, v1)
   ELSIF (v1^.type=set) OR (v2^.type=set)
   THEN
      MetaErrorT0 (tokenno, 'cannot perform a comparison between a number and a set') ;
      FlushErrors ;
      result := FALSE
   ELSE
      result := (CompareTrees(v2^.numberValue, v1^.numberValue)=1)
   END ;
   Dispose(v1) ;
   Dispose(v2) ;
   RETURN( result )
END Gre ;


(*
   IsSubset - returns TRUE if the set as defined by, s1, is a subset of set, s2.
*)

PROCEDURE IsSubset (tokenno: CARDINAL; s1, s2: PtrToValue) : BOOLEAN ;
BEGIN
   Push(s1) ;
   Push(s2) ;
   SetAnd(tokenno) ;
   Push(s1) ;
   RETURN( Equ(tokenno) )
END IsSubset ;


(*
   LessEqu - returns true if Op2<Op1

             The Stack:

             Entry             Exit

      Ptr ->
             +------------+
             | Op1        |
             |------------|
             | Op2        |
             |------------|    Empty

             RETURN( Op2 <= Op1 )
*)

PROCEDURE LessEqu (tokenno: CARDINAL) : BOOLEAN ;
VAR
   v1, v2: PtrToValue ;
   result: BOOLEAN ;
BEGIN
   v1 := Pop() ;
   v2 := Pop() ;
   IF (v1^.type=set) AND (v2^.type=set)
   THEN
      result := IsSubset(tokenno, v2, v1)
   ELSIF (v1^.type=set) OR (v2^.type=set)
   THEN
      MetaErrorT0 (tokenno, 'cannot perform a comparison between a number and a set') ;
      FlushErrors ;
      result := FALSE
   ELSE
      result := (CompareTrees(v2^.numberValue, v1^.numberValue)<=0)
   END ;
   Dispose(v1) ;
   Dispose(v2) ;
   RETURN( result )
END LessEqu ;


(*
   IsSuperset - returns TRUE if the set as defined by, s1, is a superset of set, s2.
*)

PROCEDURE IsSuperset (tokenno: CARDINAL; s1, s2: PtrToValue) : BOOLEAN ;
BEGIN
   PushFrom(s1) ;
   PushFrom(s2) ;
   SetAnd(tokenno) ;
   PushFrom(s2) ;
   RETURN( Equ(tokenno) )
END IsSuperset ;


(*
   GreEqu - returns true if Op2 >= Op1
            are not identical.

            The Stack:

            Entry             Exit

     Ptr ->
            +------------+
            | Op1        |
            |------------|
            | Op2        |
            |------------|    Empty

            RETURN( Op2 >= Op1 )
*)

PROCEDURE GreEqu (tokenno: CARDINAL) : BOOLEAN ;
VAR
   v1, v2: PtrToValue ;
   result: BOOLEAN ;
BEGIN
   v1 := Pop() ;
   v2 := Pop() ;
   IF (v1^.type=set) AND (v2^.type=set)
   THEN
      result := IsSuperset(tokenno, v2, v1)
   ELSIF (v1^.type=set) OR (v2^.type=set)
   THEN
      MetaErrorT0 (tokenno, 'cannot perform a comparison between a number and a set') ;
      FlushErrors ;
      result := FALSE
   ELSE
      result := (CompareTrees(v2^.numberValue, v1^.numberValue)>=0)
   END ;
   Dispose(v1) ;
   Dispose(v2) ;
   RETURN( result )
END GreEqu ;


(*
   IsNulSet - returns TRUE if the top element is the nul set constant, {}.
*)

PROCEDURE IsNulSet () : BOOLEAN ;
VAR
   v: PtrToValue ;
   r: BOOLEAN ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      r := (type=set) AND (setValue=NIL)
   END ;
   Push(v) ;
   RETURN( r )
END IsNulSet ;


(*
   IsGenericNulSet - returns TRUE if the top element is the generic nul set constant, {}.
*)

PROCEDURE IsGenericNulSet () : BOOLEAN ;
VAR
   v: PtrToValue ;
   r: BOOLEAN ;
BEGIN
   v := Pop() ;
   WITH v^ DO
      r := (type=set) AND (setValue=NIL) AND (constructorType=NulSym)
   END ;
   Push(v) ;
   RETURN( r )
END IsGenericNulSet ;


(*
   PushNulSet - pushes an empty set {} onto the ALU stack. The subrange type used
                to construct the set is defined by, constructorType.
                If this is NulSym then
                the set is generic and compatible with all sets.

                The Stack:

                Entry             Exit

                                                 <- Ptr
                                  +------------+
                                  | {}         |
                Ptr ->            |------------|

*)

PROCEDURE PushNulSet (settype: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := InitValue() ;
   WITH v^ DO
      type            := set ;
      constructorType := settype ;
      areAllConstants := TRUE ;
      solved          := CompletelyResolved(settype) ;
      setValue        := NIL ;
      next            := NIL ;
   END ;
   Push(v)
END PushNulSet ;


(*
   PushEmptyConstructor - pushes an empty constructor {} onto the ALU stack.
                          This is expected to be filled in by subsequent
                          calls to AddElements, AddRange or AddField.

                          The Stack:

                          Entry             Exit

                                                       <- Ptr
                                        +------------+
                                        | {}         |
                   Ptr ->               |------------|

*)

PROCEDURE PushEmptyConstructor (constype: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := InitValue() ;
   WITH v^ DO
      type            := constructor ;
      constructorType := constype ;
      areAllConstants := TRUE ;
      solved          := CompletelyResolved(constype) ;
      fieldValues     := NIL ;
      next            := NIL ;
   END ;
   Push(v)
END PushEmptyConstructor ;


(*
   PushEmptyArray - pushes an empty array {} onto the ALU stack.
                    This is expected to be filled in by subsequent
                    calls to AddElements.

                    The Stack:

                    Entry             Exit

                                                     <- Ptr
                                      +------------+
                                      | {}         |
             Ptr ->                   |------------|

*)

PROCEDURE PushEmptyArray (arraytype: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := InitValue() ;
   WITH v^ DO
      type            := array ;
      constructorType := arraytype ;
      areAllConstants := TRUE ;
      solved          := CompletelyResolved(arraytype) ;
      arrayValues     := NIL ;
      next            := NIL ;
   END ;
   Push(v)
END PushEmptyArray ;


(*
   PushEmptyRecord - pushes an empty record {} onto the ALU stack.
                     This is expected to be filled in by subsequent
                     calls to AddField.

                     The Stack:

                     Entry             Exit

                                                      <- Ptr
                                       +------------+
                                       | {}         |
              Ptr ->                   |------------|

*)

PROCEDURE PushEmptyRecord (recordtype: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := InitValue() ;
   WITH v^ DO
      type            := record ;
      constructorType := recordtype ;
      areAllConstants := TRUE ;
      solved          := CompletelyResolved(recordtype) ;
      arrayValues     := NIL ;
      next            := NIL ;
   END ;
   Push(v)
END PushEmptyRecord ;


(*
   AddElements - adds the elements, el BY, n, to the array constant.

                 Ptr ->
                                                           <- Ptr
                        +------------+      +------------+
                        | Array      |      | Array      |
                        |------------|      |------------|

*)

PROCEDURE AddElements (tokenno: CARDINAL; el, n: CARDINAL) ;
VAR
   v: PtrToValue ;
   e: listOfElements ;
BEGIN
   v := Pop() ;
   v := CoerseTo(tokenno, array, v) ;
   IF v^.type=array
   THEN
      NewElement(e) ;
      WITH e^ DO
         element := el ;
         by      := n ;
         next    := NIL
      END ;
      AddElementToEnd(v, e) ;
      WITH v^ DO
         solved := solved AND IsSolvedGCC(el) AND IsSolvedGCC(n)
      END
   ELSE
      InternalError ('expecting array type')
   END ;
   Push(v)
END AddElements ;


(*
   AddElement -

PROCEDURE AddElement (v: listOfElements;
                      e, b: CARDINAL) : listOfElements ;
VAR
   el: listOfElements ;
BEGIN
   NEW(el) ;
   IF el=NIL
   THEN
      InternalError ('out of memory')
   END ;
   (* held in reverse order here *)
   WITH el^ DO
      element := e ;
      by      := b ;
      next    := v^.next
   END ;
   v^.next := el ;
   RETURN( v )
END AddElement ;
*)


(*
   cellTypeString - returns a string corresponding to, s.
*)

PROCEDURE cellTypeString (s: cellType) : String ;
BEGIN
   CASE s OF

   none       : RETURN( InitString('none') ) |
   integer    : RETURN( InitString('integer') ) |
   real       : RETURN( InitString('real') ) |
   complex    : RETURN( InitString('complex') ) |
   set        : RETURN( InitString('set') ) |
   constructor: RETURN( InitString('constructor') ) |
   array      : RETURN( InitString('array') ) |
   record     : RETURN( InitString('record') )

   ELSE
      InternalError ('unexpected value of s')
   END ;
   RETURN( NIL )
END cellTypeString ;


(*
   ToSetValue - converts a list of fields into a list of ranges.
                In effect it turns a generic constructor into
                a set type.
*)

PROCEDURE ToSetValue (f: listOfFields) : listOfRange ;
VAR
   g   : listOfFields ;
   r, s: listOfRange ;
BEGIN
   g := f ;
   r := NIL ;
   WHILE f#NIL DO
      NewRange(s) ;
      WITH s^ DO
         low  := f^.field ;
         high := low ;
         next := r
      END ;
      IF r=NIL
      THEN
         r := s
      END ;
      f := f^.next
   END ;
   DisposeFields(g) ;
   RETURN( r )
END ToSetValue ;


(*
   ToArrayValue - converts a list of fields into an array initialiser.
                  In effect it turns a generic constructor into
                  an array type.
*)

PROCEDURE ToArrayValue (tok: CARDINAL; f: listOfFields) : listOfElements ;
VAR
   g   : listOfFields ;
   r, s: listOfElements ;
BEGIN
   g := f ;
   r := NIL ;
   WHILE f#NIL DO
      NewElement(s) ;
      WITH s^ DO
         element := f^.field ;
         by      := MakeConstLit (tok, MakeKey('1'), ZType) ;
         next    := r
      END ;
      IF r=NIL
      THEN
         r := s
      END ;
      f := f^.next
   END ;
   DisposeFields(g) ;
   RETURN( r )
END ToArrayValue ;


(*
   ChangeToConstructor - change the top of stack value to a constructor, type.
                         (Constructor, Set, Array or Record).
*)

PROCEDURE ChangeToConstructor (tokenno: CARDINAL; constype: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   IF IsValueTypeConstructor() OR IsValueTypeSet() OR
      IsValueTypeArray() OR IsValueTypeRecord()
   THEN
      RETURN
   ELSIF IsValueTypeNone()
   THEN
      v := Pop() ;
      WITH v^ DO
         type            := constructor ;
         constructorType := constype ;
         solved          := CompletelyResolved(constype) ;
         fieldValues     := NIL ;
         next            := NIL ;
      END ;
      IF IsSet(SkipType(constype))
      THEN
         v := CoerseTo(tokenno, set, v)
      ELSIF IsRecord(SkipType(constype))
      THEN
         v := CoerseTo(tokenno, record, v)
      ELSIF IsArray(SkipType(constype))
      THEN
         v := CoerseTo(tokenno, array, v)
      END ;
      Push(v)
   ELSE
      InternalError('cannot change constant to a constructor type')
   END
END ChangeToConstructor ;


(*
   CoerseTo - attempts to coerses a cellType, v, into, type, t.
              Normally this will be a generic constructors converting
              into set or array.
*)

PROCEDURE CoerseTo (tokenno: CARDINAL;
                    t: cellType; v: PtrToValue) : PtrToValue ;
VAR
   s1, s2, s3: DynamicStrings.String ;
BEGIN
   WITH v^ DO
      IF t=type
      THEN
         RETURN( v )
      ELSIF (type=constructor) AND (t=set)
      THEN
         type := set ;
         setValue := ToSetValue(fieldValues) ;
         RETURN( v )
      ELSIF (type=constructor) AND (t=array)
      THEN
         type := array ;
         arrayValues := ToArrayValue (tokenno, fieldValues) ;
         RETURN( v )
      ELSIF (type=constructor) AND (t=record)
      THEN
         (* nothing to do other than change tag *)
         type := record ;
         RETURN( v )
      ELSE
         s1 := cellTypeString (t) ;
         s2 := cellTypeString (type) ;
         s3 := ConCat(InitString('cannot mix construction of a '),
                      Mark(ConCat(Mark(s1),
                                  Mark(ConCat(InitString(' with a '),
                                              (Mark(s2))))))) ;
         MetaErrorStringT0 (tokenno, s3) ;
         RETURN( v )
      END
   END
END CoerseTo ;


(*
   SetNegate - negates the top set on the stack.

               Ptr ->                                               <- Ptr
                      +-----------+                  +------------+
                      | Set       |                  | Set        |
                      |-----------|                  |------------|
*)

PROCEDURE SetNegate (tokenno: CARDINAL) ;
VAR
   min,
   max : CARDINAL ;
   r, s: listOfRange ;
   v   : PtrToValue ;
   i   : CARDINAL ;
BEGIN
   v := Pop() ;
   Eval(tokenno, v) ;
   IF v^.constructorType=NulSym
   THEN
      MetaError0 ('cannot negate a generic set, set should be prefixed by a simple type')
   END ;
   r := NIL ;
   min := GetTypeMin(GetType(v^.constructorType)) ;
   max := GetTypeMax(GetType(v^.constructorType)) ;
   i := min ;
   s := v^.setValue ;
   IF Debugging
   THEN
      printf0('attempting to negate set\n') ;
      DisplayElements(s)
   END ;
   WHILE s#NIL DO
      PushValue(s^.low) ;
      PushValue(min) ;
      IF Gre(tokenno)
      THEN
         PushValue(i) ;
         PushValue(max) ;
         IF LessEqu(tokenno)
         THEN
            r := AddRange(r, i, DupConst(tokenno, s^.low, -1))
         END
      END ;
      PushValue(s^.high) ;
      PushValue(max) ;
      IF Less(tokenno)
      THEN
         i := DupConst(tokenno, s^.high, 1) ;
         s := s^.next
      ELSE
         s := NIL
      END
   END ;
   IF Debugging
   THEN
      printf0('negated set so far\n') ;
      DisplayElements(r)
   END ;
   DisposeRange(v^.setValue) ;
   PushValue(i) ;
   PushValue(max) ;
   IF LessEqu(tokenno)
   THEN
      r := AddRange(r, i, max)
   END ;
   IF Debugging
   THEN
      printf0('final negated set value\n') ;
      DisplayElements(r)
   END ;
   WITH v^ DO
      solved := FALSE ;
      setValue := r ;
   END ;
   Eval(tokenno, v) ;
   Push(v)
END SetNegate ;


(*
   AddBitRange - adds the range op1..op2 to the underlying set.

                 Ptr ->
                                                           <- Ptr
                        +------------+      +------------+
                        | Set        |      | Set        |
                        |------------|      |------------|

*)

PROCEDURE AddBitRange (tokenno: CARDINAL; op1, op2: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   v := CoerseTo(tokenno, set, v) ;
   IF v^.type=set
   THEN
      WITH v^ DO
         setValue        := AddRange(setValue, op1, op2) ;
         solved          := solved AND IsSolvedGCC(op1) AND IsSolvedGCC(op2) ;
         areAllConstants := areAllConstants AND IsConst(op1) AND IsConst(op2)
      END
   END ;
   Push(v)
END AddBitRange ;


(*
   AddBit - adds the bit op1 to the underlying set. INCL(Set, op1)

            Ptr ->
                                                      <- Ptr
                   +------------+      +------------+
                   | Set        |      | Set        |
                   |------------|      |------------|
*)

PROCEDURE AddBit (tokenno: CARDINAL; op1: CARDINAL) ;
BEGIN
   AddBitRange(tokenno, op1, op1)
END AddBit ;


(*
   AddElementToEnd - appends, e, to the end of list, v.
*)

PROCEDURE AddElementToEnd (v: PtrToValue; e: listOfElements) ;
VAR
   a: listOfElements ;
BEGIN
   IF v^.arrayValues=NIL
   THEN
      v^.arrayValues := e
   ELSE
      a := v^.arrayValues ;
      WHILE a^.next#NIL DO
         a := a^.next
      END ;
      a^.next := e
   END
END AddElementToEnd ;


(*
   AddFieldToEnd - appends, f, to the end of list, v.
*)

PROCEDURE AddFieldToEnd (v: PtrToValue; f: listOfFields) ;
VAR
   a: listOfFields ;
BEGIN
   IF v^.fieldValues=NIL
   THEN
      v^.fieldValues := f
   ELSE
      a := v^.fieldValues ;
      WHILE a^.next#NIL DO
         a := a^.next
      END ;
      a^.next := f
   END
END AddFieldToEnd ;


(*
   AddField - adds the field op1 to the underlying constructor.

                 Ptr ->
                                                           <- Ptr
                        +------------+      +------------+
                        | const      |      | const      |
                        |------------|      |------------|

*)

PROCEDURE AddField (tokenno: CARDINAL; op1: CARDINAL) ;
VAR
   v: PtrToValue ;
   f: listOfFields ;
   e: listOfElements ;
BEGIN
   v := Pop() ;
   CASE v^.type OF

   set        : Push(v) ;
                AddBit(tokenno, op1) ;
                RETURN |
   array      : WITH v^ DO
                   solved := solved AND IsSolvedGCC(op1) ;
                   areAllConstants := areAllConstants AND IsConst(op1)
                END ;
                NewElement(e) ;
                WITH e^ DO
                   element := op1 ;
                   by      := MakeConstLit (tokenno, MakeKey('1'), ZType) ;
                   next    := NIL
                END ;
                AddElementToEnd(v, e) |
   constructor,
   record     : WITH v^ DO
                   solved := solved AND IsSolvedGCC(op1) ;
                   areAllConstants := areAllConstants AND IsConst(op1)
                END ;
                NewField(f) ;
                WITH f^ DO
                   field := op1 ;
                   next  := NIL
                END ;
                AddFieldToEnd(v, f)

   ELSE
      InternalError ('not expecting this constant type')
   END ;
   Push(v)
END AddField ;


(*
   ElementsSolved - returns TRUE if all ranges in the set have been solved.
*)

PROCEDURE ElementsSolved (tokenno: CARDINAL; r: listOfRange) : BOOLEAN ;
BEGIN
   WHILE r#NIL DO
      WITH r^ DO
         IF NOT IsConst (low)
         THEN
            MetaErrorT1 (tokenno, 'a constant set can only contain constant set elements, {%1Ead} is not a constant',
                         low)
         END ;
         IF (high # low) AND (NOT IsConst (high))
         THEN
            MetaErrorT1 (tokenno, 'a constant set can only contain constant set elements, {%1Ead} is not a constant',
                         high)
         END ;
         IF NOT (IsSolvedGCC(low) AND IsSolvedGCC(high))
         THEN
            RETURN( FALSE )
         END
      END ;
      r := r^.next
   END ;
   RETURN( TRUE )
END ElementsSolved ;


(*
   ArrayElementsSolved - returns TRUE if all ranges in the set have been solved.
*)

PROCEDURE ArrayElementsSolved (e: listOfElements) : BOOLEAN ;
BEGIN
   WHILE e#NIL DO
      WITH e^ DO
         IF NOT (IsSolvedGCC(element) AND IsSolvedGCC(by))
         THEN
            RETURN( FALSE )
         END
      END ;
      e := e^.next
   END ;
   RETURN( TRUE )
END ArrayElementsSolved ;


(*
   EvalFieldValues - returns TRUE if all fields in the record have been solved.
*)

PROCEDURE EvalFieldValues (e: listOfFields) : BOOLEAN ;
BEGIN
   WHILE e#NIL DO
      WITH e^ DO
         IF IsConst(field)
         THEN
            IF NOT IsSolvedGCC(field)
            THEN
               RETURN( FALSE )
            END
         ELSE
            (* RETURN( FALSE ) *)
         END
      END ;
      e := e^.next
   END ;
   RETURN( TRUE )
END EvalFieldValues ;


(*
   Swap - swaps the contents of, i, and, j.
*)

PROCEDURE Swap (i, j: listOfRange) ;
VAR
   t: CARDINAL ;
BEGIN
   t := i^.low ;
   i^.low := j^.low ;
   j^.low := t ;

   t := i^.high ;
   i^.high := j^.high ;
   j^.high := t
END Swap ;


(*
   DisplayElements -
*)

PROCEDURE DisplayElements (i: listOfRange) ;
BEGIN
   WHILE i#NIL DO
      PushValue(i^.low) ;
      PrintValue ;
      Reduce ;
      PushValue(i^.high) ;
      PrintValue ;
      Reduce ;
      i := i^.next
   END
END DisplayElements ;


(*
   SortElements - sorts the list as defined by, h, into ascending range order.
                  The low element is the sort key.
*)

PROCEDURE SortElements (tokenno: CARDINAL; h: listOfRange) ;
VAR
   i, j, k: listOfRange ;
BEGIN
   i := h ;
   WHILE i#NIL DO
      j := i ;
      k := i^.next ;
      WHILE k#NIL DO
         PushValue(k^.low) ;
         ConvertToInt ;
         PushValue(j^.low) ;
         ConvertToInt ;
         IF Less(tokenno)
         THEN
            j := k ;
         END ;
         k := k^.next
      END ;
      Swap(i, j) ;
      i := i^.next
   END
END SortElements ;


(*
   CombineElements - given a sorted list determine whether there is any
                     overlap in the low..high bounds. If overlap exists
                     then remove it.
*)

PROCEDURE CombineElements (tokenno: CARDINAL; r: listOfRange) ;
VAR
   t, j: listOfRange ;
BEGIN
   WHILE r#NIL DO
      j := r^.next ;
      WHILE j#NIL DO
         PushValue(r^.high) ;
         ConvertToInt ;
         PushCard(1) ;
         Addn ;
         PushValue(j^.low) ;
         ConvertToInt ;
         IF GreEqu(tokenno)
         THEN
            r^.high := j^.high ;
            t := j^.next ;
            r^.next := j^.next ;
            j^.next := NIL ;
            DisposeRange(j) ;
            j := t
         ELSE
            j := NIL
         END
      END ;
      r := r^.next
   END
END CombineElements ;


(*
   EvalSetValues - returns TRUE if all elements in this set have been resolved.
*)

PROCEDURE EvalSetValues (tokenno: CARDINAL; r: listOfRange) : BOOLEAN ;
BEGIN
   IF ElementsSolved (tokenno, r)
   THEN
      SortElements(tokenno, r) ;
      CombineElements(tokenno, r) ;
      RETURN( TRUE )
   ELSE
      RETURN( FALSE )
   END
END EvalSetValues ;


(*
   Eval - attempts to solve a constructor type.
*)

PROCEDURE Eval (tokenno: CARDINAL; v: PtrToValue) ;
BEGIN
   CheckNotAlreadyOnFreeList(v) ;
   WITH v^ DO
      IF NOT solved
      THEN
         IF IsSet(SkipType(constructorType))
         THEN
            v := CoerseTo(tokenno, set, v)
         ELSIF IsRecord(SkipType(constructorType))
         THEN
            v := CoerseTo(tokenno, record, v)
         ELSIF IsArray(SkipType(constructorType))
         THEN
            v := CoerseTo(tokenno, array, v)
         END ;
         areAllConstants := DefinedByConstants(v) ;
         CASE type OF

         set   :   Assert((constructorType=NulSym) OR IsSet(SkipType(constructorType))) ;
                   solved := CompletelyResolved(constructorType) AND EvalSetValues(tokenno, setValue) |
         array :   Assert((constructorType=NulSym) OR IsArray(SkipType(constructorType))) ;
                   solved := CompletelyResolved(constructorType) AND ArrayElementsSolved(arrayValues) |
         record:   Assert((constructorType=NulSym) OR IsRecord(SkipType(constructorType))) ;
                   solved := CompletelyResolved(constructorType) AND EvalFieldValues(fieldValues)

         ELSE
            (* do nothing *)
         END
      END
   END
END Eval ;


(*
   WalkSetValueDependants -
*)

PROCEDURE WalkSetValueDependants (r: listOfRange; p: WalkAction) ;
BEGIN
   WHILE r#NIL DO
      WITH r^ DO
         p(low) ;
         p(high)
      END ;
      r := r^.next
   END
END WalkSetValueDependants ;


(*
   IsSetValueDependants -
*)

PROCEDURE IsSetValueDependants (r: listOfRange; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
BEGIN
   result := TRUE ;
   WHILE r#NIL DO
      WITH r^ DO
         IF NOT q(low)
         THEN
            result := FALSE
         END ;
         IF NOT q(high)
         THEN
            result := FALSE
         END
      END ;
      r := r^.next
   END ;
   RETURN( result )
END IsSetValueDependants ;


(*
   WalkFieldValueDependants -
*)

PROCEDURE WalkFieldValueDependants (f: listOfFields; p: WalkAction) ;
BEGIN
   WHILE f#NIL DO
      WITH f^ DO
         p(field)
      END ;
      f := f^.next
   END
END WalkFieldValueDependants ;


(*
   IsFieldValueDependants -
*)

PROCEDURE IsFieldValueDependants (f: listOfFields; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
BEGIN
   result := TRUE ;
   WHILE f#NIL DO
      WITH f^ DO
         IF NOT q(field)
         THEN
            result := FALSE
         END
      END ;
      f := f^.next
   END ;
   RETURN( result )
END IsFieldValueDependants ;


(*
   WalkArrayValueDependants -
*)

PROCEDURE WalkArrayValueDependants (a: listOfElements; p: WalkAction) ;
BEGIN
   WHILE a#NIL DO
      WITH a^ DO
         p(element) ;
         p(by)
      END ;
      a := a^.next
   END
END WalkArrayValueDependants ;


(*
   IsArrayValueDependants -
*)

PROCEDURE IsArrayValueDependants (a: listOfElements; q: IsAction) : BOOLEAN ;
VAR
   result: BOOLEAN ;
BEGIN
   result := TRUE ;
   WHILE a#NIL DO
      WITH a^ DO
         IF NOT q(element)
         THEN
            result := FALSE
         END ;
         IF NOT q(by)
         THEN
            result := FALSE
         END
      END ;
      a := a^.next
   END ;
   RETURN( result )
END IsArrayValueDependants ;


(*
   IsConstructorDependants - return TRUE if all q(dependants) of,
                             sym, return TRUE.
*)

PROCEDURE IsConstructorDependants (sym: CARDINAL; q: IsAction) : BOOLEAN ;
VAR
   v         : PtrToValue ;
   typeResult,
   result    : BOOLEAN ;
BEGIN
   PushValue(sym) ;
   IF IsValueTypeNone()
   THEN
      v := Pop() ;
      result := FALSE
   ELSE
      v := Pop() ;
      WITH v^ DO
         typeResult := q(constructorType) ;
         CASE type OF

         none       :  result := FALSE |
         set        :  result := IsSetValueDependants(setValue, q) |
         constructor,
         record     :  result := IsFieldValueDependants(fieldValues, q) |
         array      :  result := IsArrayValueDependants(arrayValues, q)

         ELSE
            InternalError ('not expecting this type')
         END ;
         result := result AND typeResult
      END
   END ;
   RETURN( result )
END IsConstructorDependants ;


(*
   WalkConstructorDependants - walk the constructor, sym, calling
                               p for each dependant.
*)

PROCEDURE WalkConstructorDependants (sym: CARDINAL; p: WalkAction) ;
VAR
   v: PtrToValue ;
BEGIN
   PushValue(sym) ;
   IF IsValueTypeNone()
   THEN
      v := Pop()
   ELSE
      v := Pop() ;
      WITH v^ DO
         p(constructorType) ;
         CASE type OF

         none       :  |
         set        :  WalkSetValueDependants(setValue, p) |
         constructor,
         record     :  WalkFieldValueDependants(fieldValues, p) |
         array      :  WalkArrayValueDependants(arrayValues, p)

         ELSE
            InternalError ('not expecting this type')
         END
      END
   END
END WalkConstructorDependants ;


(*
   PutConstructorSolved - records that this constructor is solved.
*)

PROCEDURE PutConstructorSolved (sym: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   PushValue(sym) ;
   v := Pop() ;
   v^.solved := TRUE ;
   Push(v) ;
   PopValue(sym)
END PutConstructorSolved ;


(*
   EvaluateValue - attempts to evaluate the symbol, sym, value.
*)

PROCEDURE EvaluateValue (sym: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   PushValue(sym) ;
   v := Pop() ;
   Eval(GetDeclaredMod(sym), v) ;
   Push(v) ;
   PopValue(sym)
END EvaluateValue ;


(*
   TryEvaluateValue - attempts to evaluate the symbol, sym, value.
*)

PROCEDURE TryEvaluateValue (sym: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   PushValue(sym) ;
   v := Pop() ;
   WITH v^ DO
      CASE type OF

      set, array, record:  IF v^.constructorType=NulSym
                           THEN
                              (* must wait *)
                              RETURN
                           ELSE
                              Eval(GetDeclaredMod(sym), v)
                           END

      ELSE
         (* nothing to do *)
      END ;
      IF solved
      THEN
         Push(v) ;
         PopValue(sym)
      END
   END
END TryEvaluateValue ;


(*
   DefinedByConstants - returns TRUE if the value, v, is defined by constants.
                        It assigns, v^.areAllConstants, with the result.
*)

PROCEDURE DefinedByConstants (v: PtrToValue) : BOOLEAN ;
BEGIN
   WITH v^ DO
      CASE type OF

      none,
      integer,
      real,
      complex    :  areAllConstants := TRUE |
      set        :  areAllConstants := rangeConstant(setValue) |
      constructor,
      record     :  areAllConstants := fieldsConstant(fieldValues) |
      array      :  areAllConstants := arrayConstant(arrayValues)

      ELSE
         InternalError ('unexpected type')
      END ;
      RETURN( areAllConstants )
   END
END DefinedByConstants ;


(*
   rangeConstant - returns TRUE if all the range entities are constant.
*)

PROCEDURE rangeConstant (r: listOfRange) : BOOLEAN ;
BEGIN
   WHILE r#NIL DO
      IF (NOT IsConst(r^.low)) OR (NOT IsConst(r^.high))
      THEN
         RETURN( FALSE )
      END ;
      r := r^.next ;
   END ;
   RETURN( TRUE )
END rangeConstant ;


(*
   fieldsConstant - returns TRUE if all the field entities are constant.
*)

PROCEDURE fieldsConstant (f: listOfFields) : BOOLEAN ;
BEGIN
   WHILE f#NIL DO
      IF NOT IsConst(f^.field)
      THEN
         RETURN( FALSE )
      END ;
      f := f^.next
   END ;
   RETURN( TRUE )
END fieldsConstant ;


(*
   arrayConstant - returns TRUE if the, element, and, by, components
                   of an array constructor are constant.
*)

PROCEDURE arrayConstant (e: listOfElements) : BOOLEAN ;
BEGIN
   WHILE e#NIL DO
      IF (NOT IsConst(e^.element)) AND (NOT IsConst(e^.by))
      THEN
         RETURN( FALSE )
      END ;
      e := e^.next
   END ;
   RETURN( TRUE )
END arrayConstant ;


(*
   FindValueEnum -
*)

PROCEDURE FindValueEnum (field: WORD) ;
BEGIN
   PushValue(field) ;
   PushIntegerTree(EnumerationValue) ;
   IF Equ(CurrentTokenNo)
   THEN
      EnumerationField := field
   END
END FindValueEnum ;


(*
   Val - returns a GCC symbol enumeration or a GCC constant which has, value, and which is
         of type, type.
*)

PROCEDURE Val (tokenno: CARDINAL; type: CARDINAL; value: Tree) : CARDINAL ;
VAR
   sym: CARDINAL ;
BEGIN
   IF IsEnumeration(type)
   THEN
      EnumerationField := NulSym ;
      EnumerationValue := value ;
      CurrentTokenNo := tokenno ;
      ForeachFieldEnumerationDo(type, FindValueEnum) ;
      IF EnumerationField=NulSym
      THEN
         InternalError ('enumeration value exceeds range')
      END ;
      RETURN( EnumerationField )
   ELSE
      sym := MakeTemporary(tokenno, ImmediateValue) ;
      PutVar(sym, type) ;
      CheckOverflow(tokenno, value) ;
      PushIntegerTree(value) ;
      PopValue(sym) ;
      RETURN( sym )
   END
END Val ;


(*
   DupConst - duplicates and returns a constant, sym, but adds, offset to its value.
*)

PROCEDURE DupConst (tokenno: CARDINAL; sym: CARDINAL; offset: INTEGER) : CARDINAL ;
BEGIN
   PushValue(sym) ;
   PushInt(offset) ;
   Addn ;
   RETURN( Val(tokenno, GetType(sym), PopIntegerTree()) )
END DupConst ;


(*
   DupConstAndAdd - duplicates and returns a constant, sym,
                    but adds the symbol, extra.
*)

PROCEDURE DupConstAndAdd (tokenno: CARDINAL;
                          sym: CARDINAL; extra: Tree) : CARDINAL ;
BEGIN
   PushValue(sym) ;
   PushIntegerTree(extra) ;
   Addn ;
   RETURN( Val(tokenno, GetType(sym), PopIntegerTree()) )
END DupConstAndAdd ;


(*
   DupConstAndAddMod - duplicates and returns a constant, sym,
                       but adds the symbol, extra, and ensures that
                       the result in within limits: min..max using
                       modulo arithmetic.
*)

PROCEDURE DupConstAndAddMod (tokenno: CARDINAL;
                             sym: CARDINAL; extra: Tree;
                             l, h: CARDINAL) : CARDINAL ;
BEGIN
   (* result := (((sym-l) + extra) MOD (h-l)) + l) *)
   PushValue(sym) ;
   PushValue(l) ;
   Sub ;
   PushIntegerTree(extra) ;
   Addn ;
   PushValue(h) ;
   PushValue(l) ;
   Sub ;
   ModTrunc ;
   PushValue(l) ;
   Addn ;
   RETURN( Val(tokenno, GetType(sym), PopIntegerTree()) )
END DupConstAndAddMod ;


(*
   Remove - removes, v, from list, h.
*)

PROCEDURE Remove (VAR h: listOfRange; v: listOfRange) ;
VAR
   i: listOfRange ;
BEGIN
   IF h=v
   THEN
      h := h^.next
   ELSE
      i := h ;
      WHILE (i#NIL) AND (i^.next#v) DO
         i := i^.next
      END ;
      IF i=NIL
      THEN
         InternalError ('expecting v to be on the list')
      ELSE
         i := v^.next
      END
   END ;
   v^.next := NIL ;
   DisposeRange(v)
END Remove ;


(*
   RemoveBit - remove bit, op1, from range, v, on list, h.
*)

PROCEDURE RemoveBit (tokenno: CARDINAL; VAR h: listOfRange; v: listOfRange; op1: CARDINAL) ;
BEGIN
   WITH v^ DO
      PushValue(low) ;
      PushValue(high) ;
      IF Equ(tokenno)
      THEN
         (* single bit in this range *)
         PushValue(low) ;
         PushValue(op1) ;
         IF Equ(tokenno)
         THEN
            (* remove entry *)
            Remove(h, v) ;
            RETURN
         END
      ELSE
         (* is op1 equal to low? *)
         PushValue(op1) ;
         PushValue(low) ;
         IF Equ(tokenno)
         THEN
            low := DupConst(tokenno, low, 1)
         ELSE
            PushValue(op1) ;
            PushValue(high) ;
            IF Equ(tokenno)
            THEN
               high := DupConst(tokenno, high, -1)
            ELSE
               high := DupConst(tokenno, op1, -1) ;
               h := AddRange(h, DupConst(tokenno, op1, 1), high) ;
               SortElements(tokenno, h)
            END
         END
      END
   END
END RemoveBit ;


(*
   PerformSubBit -
*)

PROCEDURE PerformSubBit (tokenno: CARDINAL; VAR h: listOfRange; op1: CARDINAL) ;
VAR
   v: listOfRange ;
BEGIN
   v := h ;
   WHILE v#NIL DO
      WITH v^ DO
         PushValue(low) ;
         PushValue(op1) ;
         IF LessEqu(tokenno)
         THEN
            PushValue(op1) ;
            PushValue(high) ;
            IF LessEqu(tokenno)
            THEN
               RemoveBit(tokenno, h, v, op1) ;
               RETURN
            END
         END
      END ;
      v := v^.next
   END
END PerformSubBit ;


(*
   SubBit - removes a bit op1 from the underlying set. EXCL(Set, Op1)

            Ptr ->
                                                      <- Ptr
                   +------------+      +------------+
                   | Set        |      | Set        |
                   |------------|      |------------|
*)

PROCEDURE SubBit (tokenno: CARDINAL; op1: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   IF v^.type=set
   THEN
      Eval(tokenno, v) ;
      WITH v^ DO
         IF solved
         THEN
            IF IsSolvedGCC(op1)
            THEN
               PerformSubBit(tokenno, setValue, op1) ;
               solved := FALSE
            ELSE
               InternalError ('can only subtract a bit from a set when the bit value is known')
            END
         ELSE
            InternalError ('can only subtract a bit from a set when the set value is known')
         END
      END ;
      Eval(tokenno, v)
   ELSE
      InternalError ('expecting set type constant')
   END ;
   Push(v)
END SubBit ;


(*
   PerformSetIn - returns TRUE if op1 is in set.
*)

PROCEDURE PerformSetIn (tokenno: CARDINAL; op1: CARDINAL; h: listOfRange) : BOOLEAN ;
BEGIN
   WHILE h#NIL DO
      WITH h^ DO
         PushValue(op1) ;
         ConvertToInt ;
         PushValue(low) ;
         ConvertToInt ;
         IF GreEqu(tokenno)
         THEN
            PushValue(op1) ;
            PushValue(high) ;
            IF LessEqu(tokenno)
            THEN
               RETURN( TRUE )
            END
         ELSE
            (* op1 is smaller than this and all subsequent ranges *)
            RETURN( FALSE )
         END
      END ;
      h := h^.next
   END ;
   RETURN( FALSE )
END PerformSetIn ;


(*
   SetIn - returns true if Op2 IN Op1

           The Stack:

           Entry             Exit

    Ptr ->
           +------------+
           | Set        |
           |------------|    Empty

           RETURN( Op1 IN Set )
*)

PROCEDURE SetIn (tokenno: CARDINAL; Op1: CARDINAL) : BOOLEAN ;
VAR
   Set   : PtrToValue ;
   result: BOOLEAN ;
BEGIN
   Set := Pop() ;
   IF Set^.type#set
   THEN
      InternalError ('expecting ALU operand to be a set')
   END ;
   Eval(tokenno, Set) ;
   IF IsSolvedGCC(Op1) AND Set^.solved
   THEN
      result := PerformSetIn(tokenno, Op1, Set^.setValue)
   ELSE
      InternalError ('one or more operands have not been resolved')
   END ;
   Dispose(Set) ;
   RETURN( result )
END SetIn ;


(*
   SetOp - perform the function doOp on the top two elements of the stack.
*)

PROCEDURE SetOp (tokenno: CARDINAL; doOp: DoSetProcedure) ;
VAR
   Result,
   Set1, Set2: PtrToValue ;
BEGIN
   Set1 := Pop() ;
   Set2 := Pop() ;
   Eval(tokenno, Set1) ;
   Eval(tokenno, Set2) ;
   IF NOT (Set1^.solved AND Set2^.solved)
   THEN
      InternalError ('one or more operands have not been resolved')
   END ;
   IF Set1^.type#set
   THEN
      InternalError ('expecting type of constant to be a set')
   END ;
   IF Set2^.type#set
   THEN
      InternalError ('expecting type of constant to be a set')
   END ;
   Result := New() ;
   WITH Result^ DO
      type            := set ;
      setValue        := doOp(tokenno, Set1^.setValue, Set2^.setValue) ;
      constructorType := MixTypes(Set1^.constructorType,
                                  Set2^.constructorType, tokenno) ;
      solved          := FALSE
   END ;
   (* Set1 and Set2 have given their range lists to the Result *)
   Set1^.setValue := NIL ;
   Set2^.setValue := NIL ;
   Eval(tokenno, Result) ;
   Push(Result) ;
   Dispose(Set1) ;
   Dispose(Set2)
END SetOp ;


(*
   PerformOr - performs a logical OR between the two ranges.
               The ranges, r1, r2, are destroyed.
*)

PROCEDURE PerformOr (tokenno: CARDINAL; r1, r2: listOfRange) : listOfRange ;
VAR
   i: listOfRange ;
BEGIN
   i := r1 ;
   WHILE (i#NIL) AND (i^.next#NIL) DO
      i := i^.next
   END ;
   IF i=NIL
   THEN
      r1 := r2
   ELSE
      i^.next := r2
   END ;
   SortElements(tokenno, r1) ;
   CombineElements(tokenno, r1) ;
   RETURN( r1 )
END PerformOr ;


(*
   SetOr -  performs an inclusive OR of the top two elements on the stack.

            The Stack:

            Entry             Exit

     Ptr ->
            +------------+
            | Set1       |                   <- Ptr
            |------------|    +------------+
            | Set2       |    | Set1 + Set2|
            |------------|    |------------|

*)

PROCEDURE SetOr (tokenno: CARDINAL) ;
BEGIN
   SetOp(tokenno, PerformOr)
END SetOr ;


(*
   Min - returns the symbol which has the least value.
*)

PROCEDURE Min (tokenno: CARDINAL; a, b: CARDINAL) : CARDINAL ;
BEGIN
   PushValue(a) ;
   ConvertToInt ;
   PushValue(b) ;
   ConvertToInt ;
   IF Less(tokenno)
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   Max - returns the symbol which has the greatest value.
*)

PROCEDURE Max (tokenno: CARDINAL; a, b: CARDINAL) : CARDINAL ;
BEGIN
   PushValue(a) ;
   ConvertToInt ;
   PushValue(b) ;
   ConvertToInt ;
   IF Gre(tokenno)
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   IsRangeIntersection - returns TRUE if ranges, r1, and, r2, intersect.
*)

PROCEDURE IsRangeIntersection (tokenno: CARDINAL; r1, r2: listOfRange) : BOOLEAN ;
BEGIN
   IF (r1=NIL) OR (r2=NIL)
   THEN
      RETURN( FALSE )
   ELSE
      (* easier to prove NOT outside limits *)
      PushValue(r1^.low) ;
      ConvertToInt ;
      PushValue(r2^.high) ;
      ConvertToInt ;
      IF Gre(tokenno)
      THEN
         RETURN( FALSE )
      ELSE
         PushValue(r1^.high) ;
         ConvertToInt ;
         PushValue(r2^.low) ;
         ConvertToInt ;
         IF Less(tokenno)
         THEN
            RETURN( FALSE )
         ELSE
            RETURN( TRUE )
         END
      END
   END
END IsRangeIntersection ;


(*
   IsRangeLess - returns TRUE if r1^.low is < r2^.low
*)

PROCEDURE IsRangeLess (tokenno: CARDINAL; r1, r2: listOfRange) : BOOLEAN ;
BEGIN
   IF (r1=NIL) OR (r2=NIL)
   THEN
      InternalError ('not expecting NIL ranges')
   END ;
   PushValue(r1^.high) ;
   ConvertToInt ;
   PushValue(r2^.low) ;
   ConvertToInt ;
   RETURN( Less(tokenno) )
END IsRangeLess ;


(*
   MinTree - returns the tree symbol which has the least value.
*)

PROCEDURE MinTree (tokenno: CARDINAL; a, b: Tree) : Tree ;
BEGIN
   PushIntegerTree(a) ;
   ConvertToInt ;
   PushIntegerTree(b) ;
   ConvertToInt ;
   IF Less(tokenno)
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END MinTree ;


(*
   MaxTree - returns the symbol which has the greatest value.
*)

PROCEDURE MaxTree (tokenno: CARDINAL; a, b: Tree) : Tree ;
BEGIN
   PushIntegerTree(a) ;
   ConvertToInt ;
   PushIntegerTree(b) ;
   ConvertToInt ;
   IF Gre(tokenno)
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END MaxTree ;


(*
   IsIntersectionTree - returns TRUE if ranges, a..b, and, c..d, intersect.
*)

PROCEDURE IsIntersectionTree (tokenno: CARDINAL; a, b, c, d: Tree) : BOOLEAN ;
BEGIN
   (* easier to prove NOT outside limits *)
   PushIntegerTree(a) ;
   ConvertToInt ;
   PushIntegerTree(d) ;
   ConvertToInt ;
   IF Gre(tokenno)
   THEN
      RETURN( FALSE )
   ELSE
      PushIntegerTree(b) ;
      ConvertToInt ;
      PushIntegerTree(c) ;
      ConvertToInt ;
      IF Less(tokenno)
      THEN
         RETURN( FALSE )
      ELSE
         RETURN( TRUE )
      END
   END
END IsIntersectionTree ;


(*
   SubTree - returns the tree value containing (a-b)
*)

PROCEDURE SubTree (a, b: Tree) : Tree ;
BEGIN
   PushIntegerTree(a) ;
   PushIntegerTree(b) ;
   Sub ;
   RETURN( PopIntegerTree() )
END SubTree ;


(*
   PerformAnd - performs a logical AND between the two ranges.
                The ranges, r1, r2, are unaltered.
*)

PROCEDURE PerformAnd (tokenno: CARDINAL; r1, r2: listOfRange) : listOfRange ;
VAR
   r: listOfRange ;
BEGIN
   r := NIL ;
   WHILE (r1#NIL) AND (r2#NIL) DO
      IF IsRangeIntersection(tokenno, r1, r2)
      THEN
         r := AddRange(r, Max(tokenno, r1^.low, r2^.low), Min(tokenno, r1^.high, r2^.high)) ;
         IF r^.high=r1^.high
         THEN
            r1 := r1^.next
         ELSE
            r2 := r2^.next
         END
      ELSIF IsRangeLess(tokenno, r1, r2)
      THEN
         (* move r1 onto the next range *)
         r1 := r1^.next
      ELSE
         (* move r2 onto the next range *)
         r2 := r2^.next
      END
   END ;
   RETURN( r )
END PerformAnd ;


(*
   SetAnd - performs a set AND the top two elements on the stack.

            The Stack:

            Entry             Exit

     Ptr ->
            +------------+
            | Op1        |                   <- Ptr
            |------------|    +------------+
            | Op2        |    | Op2 * Op1  |
            |------------|    |------------|
*)

PROCEDURE SetAnd (tokenno: CARDINAL) ;
BEGIN
   SetOp(tokenno, PerformAnd)
END SetAnd ;


(*
   SetDifference - performs a set difference of the top two elements on the stack.
                   For each member in the set
                      if member in Op2 and not member in Op1

                   The Stack:

                   Entry             Exit

            Ptr ->
                   +------------+
                   | Op1        |                   <- Ptr
                   |------------|    +-------------------+
                   | Op2        |    | Op2 and (not Op1) |
                   |------------|    |-------------------|
*)

PROCEDURE SetDifference (tokenno: CARDINAL) ;
VAR
   Set1, Set2: PtrToValue ;
BEGIN
   Set1 := Pop() ;
   Set2 := Pop() ;
   Eval(tokenno, Set1) ;
   Eval(tokenno, Set2) ;
   IF NOT (Set1^.solved AND Set2^.solved)
   THEN
      InternalError ('one or more operands have not been resolved')
   END ;
   IF Set1^.setValue=NIL
   THEN
      (* null set, return Set2 *)
      Push(Set1) ;
   ELSE
      Push(Set1) ;
      SetNegate(tokenno) ;
      Push(Set2) ;
      SetAnd(tokenno)
   END
END SetDifference ;


(*
   SetSymmetricDifference - performs a set difference of the top two elements on the stack.

                            The Stack:

                            Entry             Exit

                     Ptr ->
                            +------------+
                            | Op1        |                    <- Ptr
                            |------------|    +-------------+
                            | Op2        |    | Op2 xor Op1 |
                            |------------|    |-------------|
*)

PROCEDURE SetSymmetricDifference (tokenno: CARDINAL) ;
VAR
   Set1, Set2: PtrToValue ;
BEGIN
   Set1 := Pop() ;
   Set2 := Pop() ;
   Eval(tokenno, Set1) ;
   Eval(tokenno, Set2) ;
   IF NOT (Set1^.solved AND Set2^.solved)
   THEN
      InternalError ('one or more operands have not been resolved')
   END ;
   IF Set1^.setValue=NIL
   THEN
      Dispose(Set1) ;
      Push(Set2)
   ELSIF Set2^.setValue=NIL
   THEN
      Dispose(Set2) ;
      Push(Set1)
   ELSE
      (* Set1 or Set2 and (not (Set1 and Set2)) *)
      PushFrom(Set1) ;
      PushFrom(Set2) ;
      SetAnd(tokenno) ;
      SetNegate(tokenno) ;
      Push(Set1) ;
      Push(Set2) ;
      SetOr(tokenno) ;
      SetAnd(tokenno)
   END
END SetSymmetricDifference ;


(*
   SetShift - if op1 is positive
              then
                 result := op2 << op1
              else
                 result := op2 >> op1
              fi


              The Stack:

                     Entry             Exit

              Ptr ->
                     +------------+
                     | Op1        |                   <- Ptr
                     |------------|    +------------+
                     | Op2        |    | result     |
                     |------------|    |------------|

*)

PROCEDURE SetShift (tokenno: CARDINAL) ;
VAR
   res,
   Shift,
   Set    : PtrToValue ;
   n      : CARDINAL ;
   r1, r2 : CARDINAL ;
BEGIN
   IF NOT IsValueTypeInteger()
   THEN
      InternalError ('expecting integer type')
   END ;
   Shift := Pop() ;
   IF NOT IsValueTypeSet()
   THEN
      InternalError ('expecting set type')
   END ;
   Set := Pop() ;
   Eval(tokenno, Set) ;
   IF NOT Set^.solved
   THEN
      InternalError ('set has not been resolved')
   END ;
   IF Set^.setValue=NIL
   THEN
      Push(Set)
   ELSE
      res := New() ;
      res^ := Set^ ;
      WITH res^ DO
         setValue := NIL ;
         n := 1 ;
         WHILE GetRange(Set, n, r1, r2) DO
            setValue := AddRange(setValue,
                                 DupConstAndAdd(tokenno, r1, Shift),
                                 DupConstAndAdd(tokenno, r2, Shift)) ;
            INC(n)
         END ;
         Push(res) ;
         IF constructorType#NulSym
         THEN
            PushNulSet(constructorType) ;
            SetNegate(tokenno) ;
            SetAnd(tokenno)
         END
      END ;
      Dispose(Set)
   END
END SetShift ;


(*
   SetRotate - if op1 is positive
               then
                  result := ROTATERIGHT(op2, op1)
               else
                  result := ROTATELEFT(op2, op1)
               fi


               The Stack:

                      Entry             Exit

               Ptr ->
                      +------------+
                      | Op1        |                   <- Ptr
                      |------------|    +------------+
                      | Op2        |    | result     |
                      |------------|    |------------|
*)

PROCEDURE SetRotate (tokenno: CARDINAL) ;
VAR
   res,
   Rotate,
   Set    : PtrToValue ;
   n      : CARDINAL ;
   l, h,
   type,
   r1, r2 : CARDINAL ;
BEGIN
   IF NOT IsValueTypeInteger()
   THEN
      InternalError ('expecting integer type')
   END ;
   Rotate := Pop() ;
   IF NOT IsValueTypeSet()
   THEN
      InternalError ('expecting set type')
   END ;
   Set := Pop() ;
   Eval(tokenno, Set) ;
   IF NOT Set^.solved
   THEN
      InternalError ('set has not been resolved')
   END ;
   IF Set^.setValue=NIL
   THEN
      Push(Set)
   ELSE
      type := Set^.constructorType ;
      IF type=NulSym
      THEN
         MetaErrorT0 (tokenno, 'cannot perform a ROTATE on a generic set') ;
         Push(Set) ;
         RETURN
      END ;
      l := GetTypeMin(type) ;
      h := GetTypeMax(type) ;
      res := New() ;
      res^ := Set^ ;
      WITH res^ DO
         setValue := NIL ;
         n := 1 ;
         WHILE GetRange(Set, n, r1, r2) DO
            setValue := AddRange(setValue,
                                 DupConstAndAddMod(tokenno, r1, Rotate, l, h),
                                 DupConstAndAddMod(tokenno, r2, Rotate, l, h)) ;
            INC(n)
         END
      END ;
      Push(res) ;
      Dispose(Set)
   END
END SetRotate ;


(*
   GetValue - returns and pops the value from the top of stack.
*)

PROCEDURE GetValue (tokenno: CARDINAL) : PtrToValue ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   Eval(tokenno, v) ;
   RETURN( v )
END GetValue ;


(*
   GetRange - returns TRUE if range number, n, exists in the value, v.
              A non empty set is defined by having 1..N ranges
*)

PROCEDURE GetRange (v: PtrToValue; n: CARDINAL; VAR low, high: CARDINAL) : BOOLEAN ;
VAR
   l: listOfRange ;
BEGIN
   WITH v^ DO
      IF type#set
      THEN
         InternalError ('expecting set constant')
      END ;
      l := setValue ;
      WHILE n>1 DO
         IF l=NIL
         THEN
            RETURN( FALSE )
         END ;
         l := l^.next ;
         DEC(n)
      END ;
      IF l=NIL
      THEN
         RETURN( FALSE )
      END ;
      low := l^.low ;
      high := l^.high
   END ;
   RETURN( TRUE )
END GetRange ;


(*
   BuildStructBitset - v is the PtrToValue.
                       low and high are the limits of the subrange.
*)

PROCEDURE BuildStructBitset (tokenno: CARDINAL; v: PtrToValue; low, high: Tree) : Tree ;
VAR
   BitsInSet : Tree ;
   bpw       : CARDINAL ;
   cons      : Constructor ;
BEGIN
   PushIntegerTree(low) ;
   ConvertToInt ;
   low := PopIntegerTree() ;
   PushIntegerTree(high) ;
   ConvertToInt ;
   high := PopIntegerTree() ;
   bpw  := GetBitsPerBitset() ;

   PushIntegerTree(high) ;
   PushIntegerTree(low) ;
   Sub ;
   PushCard(1) ;
   Addn ;
   BitsInSet := PopIntegerTree() ;

   cons := BuildStartSetConstructor(Mod2Gcc(v^.constructorType)) ;

   PushIntegerTree(BitsInSet) ;
   PushCard(0) ;
   WHILE Gre(tokenno) DO
      PushIntegerTree(BitsInSet) ;
      PushCard(bpw-1) ;
      IF GreEqu(tokenno)
      THEN
         PushIntegerTree(low) ;
         PushCard(bpw-1) ;
         Addn ;

         BuildSetConstructorElement(cons, BuildBitset(tokenno, v, low, PopIntegerTree())) ;

         PushIntegerTree(low) ;
         PushCard(bpw) ;
         Addn ;
         low := PopIntegerTree() ;
         PushIntegerTree(BitsInSet) ;
         PushCard(bpw) ;
         Sub ;
         BitsInSet := PopIntegerTree()
      ELSE
         (* printf2('range is %a..%a\n', GetSymName(low), GetSymName(high)) ; *)

         BuildSetConstructorElement(cons, BuildBitset(tokenno, v, low, high)) ;

         PushCard(0) ;
         BitsInSet := PopIntegerTree()
      END ;
      PushIntegerTree(BitsInSet) ;
      PushCard(0)
   END ;
   RETURN( BuildEndSetConstructor(cons) )
END BuildStructBitset ;


(*
   ConstructLargeOrSmallSet - generates a constant representing the set value of the symbol, sym.
                              We manufacture the constant by using a initialization
                              structure of cardinals.

                              { (cardinal), (cardinal) etc }
*)

PROCEDURE ConstructLargeOrSmallSet (tokenno: CARDINAL; v: PtrToValue; low, high: CARDINAL) : Tree ;
BEGIN
   PushValue(high) ;
   ConvertToInt ;
   PushValue(low) ;
   ConvertToInt ;
   Sub ;
   PushCard(GetBitsPerBitset()) ;
   IF Less(tokenno)
   THEN
      (* small set *)
      RETURN( BuildBitset(tokenno, v, Mod2Gcc(low), Mod2Gcc(high)) )
   ELSE
      (* large set *)
      RETURN( BuildStructBitset(tokenno, v, Mod2Gcc(low), Mod2Gcc(high)) )
   END
END ConstructLargeOrSmallSet ;


(*
   ConstructSetConstant - builds a struct of integers which represents the
                          set const as defined by, v.
*)

PROCEDURE ConstructSetConstant (tokenno: CARDINAL; v: PtrToValue) : Tree ;
VAR
   n1, n2   : Name ;
   gccsym   : Tree ;
   baseType,
   high, low: CARDINAL ;
BEGIN
   WITH v^ DO
      IF constructorType=NulSym
      THEN
         InternalError ('set type must be known in order to generate a constant')
      ELSE
         baseType := SkipType(GetType(constructorType)) ;
         IF Debugging
         THEN
            n1 := GetSymName(constructorType) ;
            n2 := GetSymName(baseType) ;
            printf2('ConstructSetConstant of type %a and baseType %a\n', n1, n2)
         END ;
         IF IsSubrange(baseType)
         THEN
            GetSubrange(baseType, high, low) ;
            gccsym := ConstructLargeOrSmallSet(tokenno, v, low, high)
         ELSE
            gccsym := ConstructLargeOrSmallSet(tokenno, v, GetTypeMin(baseType), GetTypeMax(baseType))
         END ;
         RETURN( gccsym )
      END
   END
END ConstructSetConstant ;


(*
   ConvertConstToType - returns a Tree containing an initialiser,
                        init, ready to be assigned to a record or
                        array constructor.
*)

PROCEDURE ConvertConstToType (tokenno: CARDINAL; field: CARDINAL; init: CARDINAL) : Tree ;
VAR
   initT,
   nBytes: Tree ;
BEGIN
   IF IsConstString(init) AND IsArray(SkipType(GetType(field))) AND
      (SkipTypeAndSubrange(GetType(GetType(field)))=Char)
   THEN
      IF NOT PrepareCopyString (tokenno, nBytes, initT, init, GetType (field))
      THEN
         MetaErrorT2 (tokenno,
                      'string constant {%1Ea} is too large to be assigned to the {%2d} {%2a}',
                      init, field)
      END ;
      RETURN initT
   ELSE
      RETURN( ConvertConstantAndCheck(TokenToLocation(tokenno), Mod2Gcc(GetType(field)), Mod2Gcc(init)) )
   END
END ConvertConstToType ;


(*
   ConstructRecordConstant - builds a struct initializer, as defined by, v.
*)

PROCEDURE ConstructRecordConstant (tokenno: CARDINAL; v: PtrToValue) : Tree ;
VAR
   n1, n2      : Name ;
   i,
   Field,
   baseType    : CARDINAL ;
   cons        : Constructor ;
BEGIN
   WITH v^ DO
      IF constructorType=NulSym
      THEN
         InternalError ('record type must be known in order to generate a constant')
      ELSE
         baseType := SkipType(constructorType) ;
         IF Debugging
         THEN
            n1 := GetSymName(constructorType) ;
            n2 := GetSymName(baseType) ;
            printf2('ConstructRecordConstant of type %a and baseType %a\n', n1, n2)
         END ;
         cons := BuildStartRecordConstructor(Mod2Gcc(baseType)) ;
         i := 1 ;
         REPEAT
            Field := GetNth(baseType, i) ;
            IF Field#NulSym
            THEN
               IF GccKnowsAbout(GetType(Field))
               THEN
                  BuildRecordConstructorElement(cons, ConvertConstToType(tokenno, Field, GetConstructorField(v, i)))
               ELSE
                  MetaErrorT0 (tokenno, 'trying to construct a compound literal and using a record field which does not exist')
               END
            END ;
            INC(i)
         UNTIL Field=NulSym ;
         RETURN( BuildEndRecordConstructor(cons) )
      END
   END
END ConstructRecordConstant ;


(*
   GetConstructorField - returns a symbol containing the constructor field, i.
*)

PROCEDURE GetConstructorField (v: PtrToValue; i: CARDINAL) : CARDINAL ;
VAR
   j: CARDINAL ;
   f: listOfFields ;
BEGIN
   WITH v^ DO
      IF type#record
      THEN
         InternalError ('constructor type must be a record in order to push a field')
      ELSE
         IF constructorType=NulSym
         THEN
            InternalError ('constructor type must be a record in order to push a field')
         ELSE
            j := 1 ;
            f := fieldValues ;
            WHILE (j<i) AND (f#NIL) DO
               f := f^.next ;
               INC(j)
            END ;
            IF f=NIL
            THEN
               MetaError1 ('the {%1EN} element does not exist in the constant compound literal', i) ;
               RETURN( NulSym )
            ELSE
               RETURN( f^.field )
            END
         END
      END
   END
END GetConstructorField ;


(*
   GetConstructorElement - returns a symbol containing the array constructor element, i.
*)

PROCEDURE GetConstructorElement (tokenno: CARDINAL; v: PtrToValue; i: CARDINAL) : CARDINAL ;
VAR
   j: Tree ;
   e: listOfElements ;
BEGIN
   WITH v^ DO
      IF type#array
      THEN
         InternalError ('constructor type must be an array')
      ELSE
         IF constructorType=NulSym
         THEN
            InternalError ('constructor type must be an array')
         ELSE
            PushCard(i) ;
            j := PopIntegerTree() ;
            e := arrayValues ;
            WHILE e#NIL DO
               PushValue(e^.by) ;
               PushIntegerTree(j) ;
               IF GreEqu(tokenno)
               THEN
                  RETURN( e^.element )
               END ;
               PushIntegerTree(j) ;
               ConvertToInt ;
               PushValue(e^.by) ;
               ConvertToInt ;
               Sub ;
               j := PopIntegerTree() ;
               e := e^.next
            END ;
            IF e=NIL
            THEN
               IF IsArray(SkipType(constructorType)) AND (GetType(SkipType(constructorType))=Char)
               THEN
                  RETURN MakeConstLit (tokenno, MakeKey('0'), Char)
               ELSE
                  MetaErrorT2 (tokenno,
                               'the {%1EN} element does not exist in the {%2ad} array declaration used by the compound literal', i, constructorType) ;
               END
            END
         END
      END
   END ;
   RETURN NulSym
END GetConstructorElement ;


(*
   IsString - returns TRUE if sym is an ARRAY [..] OF CHAR
*)

PROCEDURE IsString (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (IsArray(sym) AND (SkipType(GetType(sym))=Char)) OR
           IsConstString(sym) OR
           (IsConst(sym) AND (SkipType(GetType(sym))=Char))
END IsString ;


(*
   StringFitsArray -
*)

PROCEDURE StringFitsArray (arrayType, el: CARDINAL; tokenno: CARDINAL) : BOOLEAN ;
VAR
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   PushIntegerTree(BuildNumberOfArrayElements(location, Mod2Gcc(arrayType))) ;
   IF IsConstString(el)
   THEN
      PushCard(GetStringLength(tokenno, el))
   ELSIF IsConst(el) AND (SkipType(GetType(el))=Char) AND IsValueSolved(el)
   THEN
      PushCard(1)
   ELSE
      PushCard(0) ;
      MetaError1 ('cannot build a string using {%1Ead}', el)
   END ;
   RETURN GreEqu(tokenno)
END StringFitsArray ;


(*
   GetArrayLimits -
*)

PROCEDURE GetArrayLimits (array: CARDINAL; VAR low, high: CARDINAL) ;
VAR
   Subscript,
   Subrange : CARDINAL ;
BEGIN
   Subscript := GetArraySubscript(array) ;
   Subrange := SkipType(GetType(Subscript)) ;
   IF IsEnumeration(Subrange)
   THEN
      GetBaseTypeMinMax(Subrange, low, high)
   ELSE
      GetSubrange(Subrange, high, low)
   END
END GetArrayLimits ;


(*
   InitialiseArrayOfCharWithString -
*)

PROCEDURE InitialiseArrayOfCharWithString (tokenno: CARDINAL; cons: Tree;
                                           el, baseType, arrayType: CARDINAL) : Tree ;
VAR
   isChar   : BOOLEAN ;
   s, letter: String ;
   i, l     : CARDINAL ;
   high, low: CARDINAL ;
   value,
   indice   : Tree ;
   location : location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   GetArrayLimits(baseType, low, high) ;
   l := 0 ;
   s := NIL ;
   IF IsConstString(el)
   THEN
      isChar := FALSE ;
      s := InitStringCharStar(KeyToCharStar(GetString(el))) ;
      l := GetStringLength(tokenno, el)
   ELSIF IsConst(el) AND (SkipType(GetType(el))=Char) AND IsValueSolved(el)
   THEN
      isChar := TRUE
   ELSE
      MetaError1 ('cannot build a string using {%1Ead}', el) ;
      isChar := FALSE
   END ;
   i := 0 ;
   REPEAT
      PushValue(low) ;
      PushCard(i) ;
      Addn ;
      indice := PopIntegerTree() ;
      letter := NIL ;
      IF isChar
      THEN
         isChar := FALSE ;
         PushValue(el) ;
         value := PopIntegerTree()
      ELSIF i<l
      THEN
         IF i+1<l
         THEN
            letter := Slice(s, i, i+1)
         ELSE
            letter := Slice(s, i, 0)
         END ;
         value := BuildCharConstant(location, string(letter)) ;
      ELSE
         letter := InitStringChar(nul) ;
         value := BuildCharConstant(location, string(letter))
      END ;
      value := ConvertConstantAndCheck(location, Mod2Gcc(arrayType), value) ;
      letter := KillString(letter) ;
      BuildArrayConstructorElement(cons, value, indice) ;
      PushValue(low) ;
      PushCard(i) ;
      Addn ;
      PushValue(high) ;
      INC(i)
   UNTIL GreEqu(tokenno) ;
   s := KillString(s) ;
   IF NOT StringFitsArray(baseType, el, tokenno)
   THEN
      MetaError2 ('string {%1Ea} is too large to fit into array {%2ad}', el, baseType)
   END ;
(*
   IF v#NIL
   THEN
      el := GetConstructorElement(tokenno, v, 2) ;
      IF el#NulSym
      THEN
         MetaError1('not allowed to have multiple strings to initialise an array of characters {%1Ua}', el)
      END
   END ;
*)
   RETURN( BuildEndArrayConstructor(cons) )
END InitialiseArrayOfCharWithString ;


(*
   CheckElementString -
*)

PROCEDURE CheckElementString (el, arrayType: CARDINAL; tokenno: CARDINAL) : Tree ;
VAR
   cons: Tree ;
BEGIN
   IF IsString(arrayType) AND IsString(el)
   THEN
      cons := BuildStartArrayConstructor(Mod2Gcc(arrayType)) ;
      RETURN( InitialiseArrayOfCharWithString(tokenno, cons, el, arrayType, SkipType(GetType(arrayType))) )
   ELSE
      RETURN( Mod2Gcc(el) )
   END
END CheckElementString ;


(*
   InitialiseArrayWith -
*)

PROCEDURE InitialiseArrayWith (tokenno: CARDINAL; cons: Tree;
                               v: PtrToValue; el, high, low, arrayType: CARDINAL) : Tree ;
VAR
   location: location_t ;
   i       : CARDINAL ;
   indice,
   value   : Tree ;
BEGIN
   location := TokenToLocation (tokenno) ;
   i := 0 ;
   WHILE el#NulSym DO
      PushValue (low) ;
      ConvertToInt ;
      PushInt (i) ;
      Addn ;
      indice := PopIntegerTree () ;
      value := CheckElementString (el, arrayType, tokenno) ;
      IF value = NIL
      THEN
         MetaErrorT0 (tokenno, '{%W}too few characters found when trying to construct a compound literal array') ;
         value := GetCardinalZero (location)
      END ;
      value := ConvertConstantAndCheck (location, Mod2Gcc (arrayType), value) ;
      BuildArrayConstructorElement (cons, value, indice) ;
      PushValue (low) ;
      ConvertToInt ;
      PushInt (i) ;
      Addn ;
      PushValue (high) ;
      ConvertToInt ;
      IF GreEqu (tokenno)
      THEN
         RETURN BuildEndArrayConstructor (cons)
      END ;
      INC (i) ;
      el := GetConstructorElement (tokenno, v, i+1)
   END ;
   RETURN BuildEndArrayConstructor (cons)
END InitialiseArrayWith ;


(*
   CheckGetCharFromString - return TRUE if a char from the position arrayIndex in the list of
                            constDecl elements can be extracted.  The character is returned
                            in value.
*)

PROCEDURE CheckGetCharFromString (location: location_t;
                                  tokenno: CARDINAL ;
                                  constDecl: PtrToValue;
                                  consType: CARDINAL ;
                                  arrayIndex: CARDINAL;
                                  VAR value: Tree) : BOOLEAN ;
VAR
   elementIndex: CARDINAL ;
   element     : CARDINAL ;
   offset,
   totalLength : CARDINAL ;
   key         : Name ;
BEGIN
   totalLength := 0 ;
   elementIndex := 1 ;
   REPEAT
      element := GetConstructorElement (tokenno, constDecl, elementIndex) ;
      offset := totalLength ;
      IF IsConstString (element)
      THEN
         INC (totalLength, GetStringLength (tokenno, element)) ;
         IF totalLength > arrayIndex
         THEN
            key := GetString (element) ;
            DEC (arrayIndex, offset) ;
            value := BuildCharConstantChar (location, CharKey (key, arrayIndex)) ;
            RETURN TRUE
         END
      ELSIF IsConst (element) AND (SkipType (GetType (element)) = Char) AND IsValueSolved (element)
      THEN
         INC (totalLength) ;
         IF totalLength > arrayIndex
         THEN
            PushValue (element) ;
            value := ConvertConstantAndCheck (location, GetM2CharType (), PopIntegerTree ()) ;
            RETURN TRUE
         END
      ELSE
         INC (totalLength) ;
         IF totalLength > arrayIndex
         THEN
            MetaErrorT3 (tokenno,
                         'expecting {%kCHAR} datatype and not {%1Ea} a {%1tad} in the {%2N} component of the {%3a} {%3d}',
                         element, arrayIndex, consType) ;
            value := GetCardinalZero (location) ;
            RETURN FALSE
         END
      END ;
      INC (elementIndex)
   UNTIL element = NulSym ;
   value := GetCardinalZero (location) ;
   MetaErrorT2 (tokenno,
                'unable to obtain a {%kCHAR} at the {%1EN} position in {%2ad}',
                arrayIndex, consType) ;
   RETURN FALSE
END CheckGetCharFromString ;


(*
   InitialiseArrayOfCharWith -
*)

PROCEDURE InitialiseArrayOfCharWith (tokenno: CARDINAL; cons: Tree;
                                     constDecl: PtrToValue;
                                     el, high, low, consType, arrayType: CARDINAL) : Tree ;
VAR
   location  : location_t ;
   arrayIndex: CARDINAL ;      (* arrayIndex is the char position index of the final string.  *)
   indice,
   value     : Tree ;
BEGIN
   location := TokenToLocation (tokenno) ;
   arrayIndex := 0 ;
   WHILE el#NulSym DO
      PushValue (low) ;
      ConvertToInt ;
      PushInt (arrayIndex) ;
      Addn ;
      indice := PopIntegerTree () ;
      IF NOT CheckGetCharFromString (location, tokenno, constDecl, consType, arrayIndex, value)
      THEN
         (*
         MetaErrorT2 (tokenno,
                      'unable to obtain a {%kCHAR} at the {%1EN} position in {%2ad}',
                      arrayIndex, consType) ;
         *)
      END ;
      value := ConvertConstantAndCheck (location, Mod2Gcc (arrayType), value) ;
      BuildArrayConstructorElement (cons, value, indice) ;
      PushValue (low) ;
      ConvertToInt ;
      PushInt (arrayIndex) ;
      Addn ;
      PushValue (high) ;
      ConvertToInt ;
      IF GreEqu (tokenno)
      THEN
         RETURN BuildEndArrayConstructor (cons)
      END ;
      INC (arrayIndex)
   END ;
   RETURN BuildEndArrayConstructor (cons)
END InitialiseArrayOfCharWith ;


(*
   ConstructArrayConstant - builds a struct initializer, as defined by, v.
*)

PROCEDURE ConstructArrayConstant (tokenno: CARDINAL; v: PtrToValue) : Tree ;
VAR
   n1, n2    : Name ;
   el1, el2,
   baseType,
   arrayType,
   high, low : CARDINAL ;
   cons      : Constructor ;
BEGIN
   WITH v^ DO
      IF constructorType=NulSym
      THEN
         InternalError ('array type must be known in order to generate a constant')
      ELSE
         baseType := SkipType(constructorType) ;
         IF Debugging
         THEN
            n1 := GetSymName(constructorType) ;
            n2 := GetSymName(baseType) ;
            printf2 ('ConstructArrayConstant of type %a and baseType %a\n', n1, n2)
         END ;
         cons := BuildStartArrayConstructor(Mod2Gcc(baseType)) ;

         GetArrayLimits(baseType, low, high) ;
         arrayType := GetType(baseType) ;

         el1 := GetConstructorElement(tokenno, v, 1) ;
         el2 := GetConstructorElement(tokenno, v, 2) ;
         IF (el2 = NulSym) AND IsString(baseType) AND IsString(el1)
         THEN
            (* constructorType is ARRAY [low..high] OF CHAR and using a string to initialise it *)
            RETURN InitialiseArrayOfCharWithString (tokenno, cons, el1, baseType, arrayType)
         ELSIF SkipType(arrayType)=Char
         THEN
            RETURN InitialiseArrayOfCharWith (tokenno, cons, v, el1, high, low, baseType, arrayType)
         ELSE
            RETURN InitialiseArrayWith (tokenno, cons, v, el1, high, low, arrayType)
         END
      END
   END
END ConstructArrayConstant ;


(*
   BuildRange - returns a integer sized constant which represents the
                value  {e1..e2}.
*)

PROCEDURE BuildRange (tokenno: CARDINAL; e1, e2: Tree) : Tree ;
VAR
   c, i, t : Tree ;
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   PushIntegerTree(e1) ;
   PushIntegerTree(e2) ;
   IF Gre(tokenno)
   THEN
      c := e1 ;
      e1 := e2 ;
      e2 := c
   END ;
   t := Tree(NIL) ;
   PushIntegerTree(e1) ;
   i := PopIntegerTree() ;
   REPEAT
      IF t=Tree(NIL)
      THEN
         t := BuildLSL(location, GetWordOne(location), ToWord(location, i), FALSE)
      ELSE
         t := BuildLogicalOr(location, t, BuildLSL(location, GetWordOne(location), ToWord(location, i), FALSE), FALSE)
      END ;
      PushIntegerTree(i) ;
      PushIntegerTree(GetIntegerOne(location)) ;
      Addn ;
      i := PopIntegerTree() ;
      PushIntegerTree(i) ;
      PushIntegerTree(e2) ;
   UNTIL Gre(tokenno) ;
   RETURN( t )
END BuildRange ;


(*
   BuildBitset - given a set, v, construct the bitmask for its
                 constant value which lie in the range low..high.
*)

PROCEDURE BuildBitset (tokenno: CARDINAL;
                       v: PtrToValue; low, high: Tree) : Tree ;
VAR
   tl, th,
   t       : Tree ;
   n       : CARDINAL ;
   r1, r2  : CARDINAL ;
   location: location_t ;
BEGIN
   location := TokenToLocation(tokenno) ;
   low := ToInteger(location, low) ;
   high := ToInteger(location, high) ;
   n := 1 ;
   t := GetCardinalZero(location) ;
   WHILE GetRange(v, n, r1, r2) DO
      PushValue(r1) ;
      tl := ToInteger(location, PopIntegerTree()) ;
      PushValue(r2) ;
      th := ToInteger(location, PopIntegerTree()) ;
      IF IsIntersectionTree(tokenno, tl, th, low, high)
      THEN
         tl := ToCardinal(location, SubTree(MaxTree(tokenno, tl, low), low)) ;
         th := ToCardinal(location, SubTree(MinTree(tokenno, th, high), low)) ;
         t := BuildLogicalOr(location, t, BuildRange(tokenno, tl, th), FALSE)
      END ;
      INC(n)
   END ;
   RETURN( ToBitset(location, t) )
END BuildBitset ;


(*
   IsValueAndTreeKnown - returns TRUE if the value is known and the gcc tree
                         is defined.

                         The Stack:

                                Entry             Exit

                         Ptr ->
                                +------------+
                                | Op1        |                   <- Ptr
                                |------------|    +------------+
*)

PROCEDURE IsValueAndTreeKnown () : BOOLEAN ;
VAR
   v: PtrToValue ;
BEGIN
   v := Pop() ;
   IF v#NIL
   THEN
      WITH v^ DO
         IF solved
         THEN
            CASE type OF

            integer,
            real,
            complex:  IF numberValue=NIL
                      THEN
                         Dispose(v) ;
                         RETURN( FALSE )
                      END
            ELSE
            END
         ELSE
            Dispose(v) ;
            RETURN( FALSE )
         END
      END ;
      Dispose(v)
   END ;
   RETURN( TRUE )
END IsValueAndTreeKnown ;


(*
   CheckOverflow - tests to see whether the tree, t, has caused
                   an overflow error and if so it generates an
                   error message.
*)

PROCEDURE CheckOverflow (tokenno: CARDINAL; t: Tree) ;
BEGIN
   IF TreeOverflow (t)
   THEN
      MetaErrorT0 (tokenno, 'constant overflow error') ;
      FlushErrors
   END
END CheckOverflow ;


(*
   CheckOrResetOverflow - tests to see whether the tree, t, has caused
                          an overflow error and if so it generates an
                          error message.
*)

PROCEDURE CheckOrResetOverflow (tokenno: CARDINAL; t: Tree; check: BOOLEAN) ;
BEGIN
   IF check
   THEN
      CheckOverflow (tokenno, t)
   ELSE
      t := RemoveOverflow (t)
   END
END CheckOrResetOverflow ;


(*
   PushGCCArrayTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushGCCArrayTree (gcc: Tree; t: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      constructorType := t ;
      type            := array ;
      numberValue     := gcc ;
      arrayValues     := NIL ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushGCCArrayTree ;


(*
   PushGCCSetTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushGCCSetTree (gcc: Tree; t: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      constructorType := t ;
      type            := set ;
      numberValue     := gcc ;
      setValue        := NIL ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushGCCSetTree ;


(*
   PushGCCRecordTree - pushes a gcc tree value onto the ALU stack.
*)

PROCEDURE PushGCCRecordTree (gcc: Tree; t: CARDINAL) ;
VAR
   v: PtrToValue ;
BEGIN
   v := New() ;
   WITH v^ DO
      constructorType := t ;
      type            := record ;
      numberValue     := gcc ;
      fieldValues     := NIL ;
      areAllConstants := TRUE ;
      solved          := TRUE
   END ;
   Push(v)
END PushGCCRecordTree ;


(*
   PushTypeOfTree - pushes tree, gcc, to the stack and records the
                    front end type.
*)

PROCEDURE PushTypeOfTree (sym: CARDINAL; gcc: Tree) ;
VAR
   t: CARDINAL ;
BEGIN
   t := SkipType(GetType(sym)) ;
   IF t=NulSym
   THEN
      PushIntegerTree(gcc)
   ELSIF IsComplexType(t)
   THEN
      PushComplexTree(gcc)
   ELSIF IsArray(t)
   THEN
      PushGCCArrayTree(gcc, t)
   ELSIF IsSet(t)
   THEN
      PushGCCSetTree(gcc, t)
   ELSIF IsRecord(t)
   THEN
      PushGCCRecordTree(gcc, t)
   ELSIF IsRealType(t)
   THEN
      PushRealTree(gcc)
   ELSE
      PushIntegerTree(gcc)
   END
END PushTypeOfTree ;


(*
   Init - initialises the stack and the free list.
*)

PROCEDURE Init ;
BEGIN
   FreeList        := NIL ;
   TopOfStack      := NIL ;
   RangeFreeList   := NIL ;
   FieldFreeList   := NIL ;
   ElementFreeList := NIL
END Init ;


BEGIN
   Init
END M2ALU.
(*
 * Local variables:
 *  compile-command: "gm2 -c -g -I.:../gm2-libs:../gm2-libs-ch:../gm2-libiberty/ M2ALU.mod"
 * End:
 *)
