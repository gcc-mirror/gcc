(* M2System.mod defines the SYSTEM builtin types.

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

IMPLEMENTATION MODULE M2System ;

(*
    Title      : M2System
    Author     : Gaius Mulley
    System     : UNIX (gm2)
    Date       : Mon Jul 10 20:24:31 2000
    Description: gcc version of M2System. It defines the builtin types within the
                 SYSTEM.def module. Remember that these modules (SYSTEM.def, SYSTEM.mod)
                 really exist, but not all type definitions are expressed in this file.
                 We also need to tell the compiler the size of the data types.
*)

FROM SymbolTable IMPORT NulSym,
                        StartScope,
                        EndScope,
      	       	     	MakeConstLit,
                        MakeConstVar,
                        MakePointer,
                        MakeType,
                        MakeProcedure,
      	       	     	MakeSet,
      	       	     	MakeSubrange,
                        PutFunction,
                        PutType, PutPointer,
      	       	     	PutSet, PutVar,
      	       	     	PutSubrange,
                        PutExportQualified,
                        PutProcedureNoReturn,
                        GetSym, GetSymName,
                        GetCurrentModule, SetCurrentModule,
                        IsLegal, ProcedureKind,
                        PopValue,
                        PopSize ;

FROM Assertion IMPORT Assert ;
FROM M2LexBuf IMPORT BuiltinTokenNo ;
FROM M2Options IMPORT Iso, Pim2, Pedantic, DumpSystemExports ;
FROM NameKey IMPORT Name, MakeKey, NulName ;
FROM M2Batch IMPORT MakeDefinitionSource ;
FROM M2Base IMPORT Cardinal, ZType ;
FROM M2Size IMPORT Size, MakeSize ;
FROM M2ALU IMPORT PushCard, PushIntegerTree, DivTrunc ;
FROM M2Error IMPORT InternalError ;
FROM Lists IMPORT List, InitList, IsItemInList, PutItemIntoList, GetItemFromList, NoOfItemsInList ;
FROM SymbolKey IMPORT SymbolTree, InitTree, GetSymKey, PutSymKey ;
FROM StrLib IMPORT StrEqual ;
FROM M2Printf IMPORT printf1 ;
FROM SymbolConversion IMPORT Mod2Gcc ;

FROM M2Base IMPORT Real, Cardinal, Integer, Complex,
                   LongReal, LongCard, LongInt, LongComplex,
                   ShortReal, ShortCard, ShortInt, ShortComplex ;

FROM gcctypes IMPORT tree ;
FROM m2linemap IMPORT BuiltinsLocation ;
FROM m2decl IMPORT GetBitsPerBitset, GetBitsPerUnit ;

FROM m2type IMPORT GetMaxFrom, GetMinFrom,
                   GetWordType, GetPointerType, GetByteType, GetISOLocType,
                   GetM2Integer8, GetM2Integer16, GetM2Integer32, GetM2Integer64,
                   GetM2Cardinal8, GetM2Cardinal16, GetM2Cardinal32, GetM2Cardinal64,
                   GetM2Word16, GetM2Word32, GetM2Word64,
                   GetM2Bitset8, GetM2Bitset16, GetM2Bitset32,
                   GetM2Real32, GetM2Real64, GetM2Real96, GetM2Real128,
                   GetM2Complex32, GetM2Complex64, GetM2Complex96, GetM2Complex128,
                   GetBitsetType, GetISOByteType, GetISOWordType,
		   GetCSizeTType, GetCSSizeTType, InitSystemTypes ;

FROM m2expr IMPORT BuildSize, GetSizeOf, AreConstantsEqual ;


TYPE
   IsP = PROCEDURE (CARDINAL) : BOOLEAN ;

VAR
   MinValues,
   MaxValues  : SymbolTree ;
   SystemTypes: List ;


(*
   Init -
*)

PROCEDURE Init ;
BEGIN
   InitList(SystemTypes) ;
   InitTree(MinValues) ;
   InitTree(MaxValues)
END Init ;


(*
   CreateMinMaxFor - creates the min and max values for, type, given gccType.
*)

PROCEDURE CreateMinMaxFor (type: CARDINAL; min, max: ARRAY OF CHAR; gccType: tree) ;
VAR
   maxval, minval: CARDINAL ;
BEGIN
   maxval := MakeConstVar (BuiltinTokenNo, MakeKey(max)) ;
   PushIntegerTree (GetMaxFrom (BuiltinsLocation (), gccType)) ;
   PopValue (maxval) ;
   PutVar (maxval, type) ;
   PutSymKey (MaxValues, GetSymName (type), maxval) ;

   minval := MakeConstVar (BuiltinTokenNo, MakeKey(min)) ;
   PushIntegerTree (GetMinFrom (BuiltinsLocation (), gccType)) ;
   PopValue (minval) ;
   PutVar (minval, type) ;
   PutSymKey (MinValues, GetSymName (type), minval)
END CreateMinMaxFor ;


(*
   MapType -
*)

PROCEDURE MapType (type: CARDINAL;
                   name, min, max: ARRAY OF CHAR;
                   needsExporting: BOOLEAN; t: tree) ;
VAR
   n: Name ;
BEGIN
   PushIntegerTree(BuildSize(BuiltinsLocation(), t, FALSE)) ;
   PopSize(type) ;
   IF IsItemInList(SystemTypes, type)
   THEN
      InternalError ('not expecting system type to already be declared')
   END ;
   PutItemIntoList(SystemTypes, type) ;

   (* create min, max constants if type is ordinal *)
   IF (NOT StrEqual(min, '')) AND (NOT StrEqual(max, ''))
   THEN
      CreateMinMaxFor(type, min, max, t)
   END ;
   IF needsExporting AND DumpSystemExports
   THEN
      n := GetSymName(type) ;
      printf1('SYSTEM module creates type: %a\n', n)
   END
END MapType ;


(*
   CreateType - create and return a frontend type which matches the GCC tree type.
*)

PROCEDURE CreateType (name, min, max: ARRAY OF CHAR;
                      needsExporting: BOOLEAN; gccType: tree) : CARDINAL ;
VAR
   type: CARDINAL ;
BEGIN
   IF gccType=NIL
   THEN
      (* GCC backend does not support this type.  *)
      RETURN NulSym
   ELSE
      (* Create base type.  *)
      type := MakeType (BuiltinTokenNo, MakeKey (name)) ;
      PutType (type, NulSym) ;  (* a Base Type *)
      MapType (type, name, min, max, needsExporting, gccType) ;
      RETURN type
   END
END CreateType ;


(*
   AttemptToCreateType - attempts to create a frontend type which matches the
                         GCC tree type.
*)

PROCEDURE AttemptToCreateType (name, min, max: ARRAY OF CHAR;
                               needsExporting: BOOLEAN; gccType: tree) ;
BEGIN
   Assert (IsLegal (CreateType (name, min, max, needsExporting, gccType)))
END AttemptToCreateType ;


(*
   CreateSetType - creates and returns a, SET OF [0..highBit], type.
                   It maps this type onto the GCC type.
*)

PROCEDURE CreateSetType (name, highBit: ARRAY OF CHAR;
                         needsExporting: BOOLEAN; gccType: tree) : CARDINAL ;
VAR
   low,
   high,
   subrange,
   type    : CARDINAL ;
BEGIN
   IF gccType=NIL
   THEN
      (* GCC backend does not support this type *)
      RETURN NulSym
   ELSE
      (* create base type *)
      type := MakeSet (BuiltinTokenNo, MakeKey (name)) ;
      low := MakeConstLit (BuiltinTokenNo, MakeKey ('0'), Cardinal) ;
      high := MakeConstLit (BuiltinTokenNo, MakeKey (highBit), Cardinal) ;
      subrange := MakeSubrange (BuiltinTokenNo, NulName) ;
      PutSubrange (subrange, low, high, Cardinal) ;
      PutSet (type, subrange, FALSE) ;
      MapType (type, name, '', '', needsExporting, gccType) ;
      RETURN type
   END
END CreateSetType ;


(*
   AttemptToCreateSetType - creates and returns a, SET OF [0..highBit], type.
                            It maps this type onto the GCC type.
*)

PROCEDURE AttemptToCreateSetType (name, highBit: ARRAY OF CHAR;
                                  needsExporting: BOOLEAN; gccType: tree) ;
BEGIN
   Assert (IsLegal (CreateSetType (name, highBit, needsExporting, gccType)))
END AttemptToCreateSetType ;


(*
   MakeFixedSizedTypes - creates the SYSTEM fixed sized types providing the
                         gcc backend supports them.
*)

PROCEDURE MakeFixedSizedTypes ;
BEGIN
   AttemptToCreateType ('INTEGER8', 'MinInteger8', 'MaxInteger8', TRUE, GetM2Integer8 ()) ;
   AttemptToCreateType ('INTEGER16', 'MinInteger16', 'MaxInteger16', TRUE, GetM2Integer16 ()) ;
   AttemptToCreateType ('INTEGER32', 'MinInteger32', 'MaxInteger32', TRUE, GetM2Integer32 ()) ;
   AttemptToCreateType ('INTEGER64', 'MinInteger64', 'MaxInteger64', TRUE, GetM2Integer64 ()) ;

   AttemptToCreateType ('CARDINAL8', 'MinCardinal8', 'MaxCardinal8', TRUE, GetM2Cardinal8 ()) ;
   AttemptToCreateType ('CARDINAL16', 'MinCardinal16', 'MaxCardinal16', TRUE, GetM2Cardinal16 ()) ;
   AttemptToCreateType ('CARDINAL32', 'MinCardinal32', 'MaxCardinal32', TRUE, GetM2Cardinal32 ()) ;
   AttemptToCreateType ('CARDINAL64', 'MinCardinal64', 'MaxCardinal64', TRUE, GetM2Cardinal64 ()) ;

   AttemptToCreateType ('WORD16', '', '', TRUE, GetM2Word16 ()) ;
   AttemptToCreateType ('WORD32', '', '', TRUE, GetM2Word32 ()) ;
   AttemptToCreateType ('WORD64', '', '', TRUE, GetM2Word64 ()) ;

   AttemptToCreateSetType ('BITSET8' , '7' , TRUE, GetM2Bitset8 ()) ;
   AttemptToCreateSetType ('BITSET16', '15', TRUE, GetM2Bitset16 ()) ;
   AttemptToCreateSetType ('BITSET32', '31', TRUE, GetM2Bitset32 ()) ;

   AttemptToCreateType ('REAL32', '', '', TRUE, GetM2Real32 ()) ;
   AttemptToCreateType ('REAL64', '', '', TRUE, GetM2Real64 ()) ;
   AttemptToCreateType ('REAL96', '', '', TRUE, GetM2Real96 ()) ;
   AttemptToCreateType ('REAL128', '', '', TRUE, GetM2Real128 ()) ;

   AttemptToCreateType ('COMPLEX32', '', '', TRUE, GetM2Complex32 ()) ;
   AttemptToCreateType ('COMPLEX64', '', '', TRUE, GetM2Complex64 ()) ;
   AttemptToCreateType ('COMPLEX96', '', '', TRUE, GetM2Complex96 ()) ;
   AttemptToCreateType ('COMPLEX128', '', '', TRUE, GetM2Complex128 ())
END MakeFixedSizedTypes ;


(*
   InitPIMTypes -
*)

PROCEDURE InitPIMTypes ;
BEGIN
   Loc := CreateType ('LOC', '', '', TRUE, GetISOLocType()) ;
   InitSystemTypes(BuiltinsLocation(), Loc) ;
   Word := CreateType ('WORD', '', '', TRUE, GetWordType()) ;
   Byte := CreateType ('BYTE', '', '', TRUE, GetByteType()) ;

   (* ADDRESS = POINTER TO BYTE *)

   Address := MakePointer (BuiltinTokenNo, MakeKey('ADDRESS')) ;
   PutPointer (Address, Byte) ;                (* Base Type       *)
   MapType (Address, 'ADDRESS', '', '', TRUE, GetPointerType())
END InitPIMTypes ;


(*
   InitISOTypes -
*)

PROCEDURE InitISOTypes ;
BEGIN
   Loc := CreateType ('LOC', 'MinLoc', 'MaxLoc', TRUE, GetISOLocType ()) ;
   InitSystemTypes (BuiltinsLocation (), Loc) ;

   Address := MakePointer (BuiltinTokenNo, MakeKey ('ADDRESS')) ;
   PutPointer (Address, Loc) ;                (* Base Type       *)
   MapType (Address, 'ADDRESS', '', '', TRUE, GetPointerType()) ;

   Byte := CreateType ('BYTE', '', '', TRUE, GetISOByteType()) ;
   Word := CreateType ('WORD', '', '', TRUE, GetISOWordType()) ;

   (* CreateMinMaxFor(Loc, 'MinLoc', 'MaxLoc', GetISOLocType()) *)
END InitISOTypes ;


(*
   MakeExtraSystemTypes - create any extra system types required
                          for portability.
*)

PROCEDURE MakeExtraSystemTypes ;
BEGIN
   CSizeT  := CreateType ('CSIZE_T' , '', '', TRUE, GetCSizeTType ()) ;
   CSSizeT := CreateType ('CSSIZE_T', '', '', TRUE, GetCSSizeTType ())
END MakeExtraSystemTypes ;


(*
   InitSystem - creates the system dependant types and procedures.
                Note that they are not exported here, but they are
                exported in the textual module: SYSTEM.def.
                We build our system types from those given in the gcc
                backend. Essentially we perform double book keeping.
*)

PROCEDURE InitSystem ;
VAR
   Previous: CARDINAL ;
BEGIN
   Init ;

   (* create SYSTEM module *)
   System := MakeDefinitionSource(BuiltinTokenNo, MakeKey('SYSTEM')) ;
   StartScope(System) ;
   Previous := GetCurrentModule() ;
   SetCurrentModule(System) ;

   IF Iso
   THEN
      InitISOTypes ;
      MakeSize ;
      PutExportQualified(BuiltinTokenNo, MakeKey('SIZE'))
   ELSE
      InitPIMTypes ;
      (* SIZE is declared in SYSTEM.def in PIM-2 but not PIM-[34] *)
      IF Pedantic
      THEN
         IF Pim2
         THEN
            MakeSize ;
            PutExportQualified(BuiltinTokenNo, MakeKey('SIZE'))
         END
      ELSE
         MakeSize ;
         PutExportQualified(BuiltinTokenNo, MakeKey('SIZE'))
      END
   END ;

   (* The predefined pseudo functions.  *)

   Adr := MakeProcedure(BuiltinTokenNo,
                        MakeKey('ADR')) ;           (* Function        *)
   PutFunction (BuiltinTokenNo, Adr, DefProcedure, Address) ;
                                                    (* Return Type     *)
                                                    (* Address         *)
   TSize := MakeProcedure(BuiltinTokenNo,
                          MakeKey('TSIZE')) ;       (* Function        *)
   PutFunction (BuiltinTokenNo, TSize, DefProcedure, ZType) ;
                                                    (* Return Type     *)
                                                    (* ZType           *)
   TBitSize := MakeProcedure(BuiltinTokenNo,
                             MakeKey('TBITSIZE')) ; (* GNU extension   *)
                                                    (* Function        *)
   PutFunction (BuiltinTokenNo, TBitSize, DefProcedure, ZType) ;
                                                    (* Return Type     *)
                                                    (* ZType           *)
   (* The ISO specific predefined pseudo functions.  *)

   AddAdr := MakeProcedure(BuiltinTokenNo,
                           MakeKey('ADDADR')) ;     (* Function        *)
   PutFunction (BuiltinTokenNo, AddAdr, DefProcedure, Address) ;
                                                    (* Return Type     *)
   SubAdr := MakeProcedure(BuiltinTokenNo,
                           MakeKey('SUBADR')) ;     (* Function        *)
   PutFunction (BuiltinTokenNo, SubAdr, DefProcedure, Address) ;
                                                    (* Return Type     *)
   DifAdr := MakeProcedure (BuiltinTokenNo,
                            MakeKey ('DIFADR')) ;   (* Function        *)
   PutFunction (BuiltinTokenNo, DifAdr, DefProcedure, Address) ;
                                                    (* Return Type     *)
   MakeAdr := MakeProcedure (BuiltinTokenNo,
                             MakeKey ('MAKEADR')) ; (* Function        *)
   PutFunction (BuiltinTokenNo, MakeAdr, DefProcedure, Address) ;
                                                    (* Return Type     *)

   (* The return value for ROTATE, SHIFT and CAST is the
      same as the first parameter and is faked in M2Quads.  *)

   Rotate := MakeProcedure(BuiltinTokenNo,
                           MakeKey('ROTATE')) ;     (* Function        *)
   Shift := MakeProcedure(BuiltinTokenNo,
                          MakeKey('SHIFT')) ;       (* Function        *)
   Cast := MakeProcedure(BuiltinTokenNo,
                         MakeKey('CAST')) ;         (* Function        *)

   Throw := MakeProcedure(BuiltinTokenNo,
                          MakeKey('THROW')) ;       (* Procedure       *)
   PutProcedureNoReturn (Throw, DefProcedure, TRUE) ;

   CreateMinMaxFor(Word, 'MinWord', 'MaxWord', GetWordType()) ;
   CreateMinMaxFor(Address, 'MinAddress', 'MaxAddress', GetPointerType()) ;
   CreateMinMaxFor(Byte, 'MinByte', 'MaxByte', GetByteType()) ;

   MakeFixedSizedTypes ;
   MakeExtraSystemTypes ;

   EndScope ;
   SetCurrentModule(Previous)
END InitSystem ;


(*
   GetSystemTypeMinMax - returns the minimum and maximum values for a given system type.
*)

PROCEDURE GetSystemTypeMinMax (type: CARDINAL; VAR min, max: CARDINAL) ;
BEGIN
   IF IsItemInList(SystemTypes, type)
   THEN
      min := GetSymKey(MinValues, GetSymName(type)) ;
      max := GetSymKey(MaxValues, GetSymName(type))
   ELSE
      InternalError ('system does not know about this type')
   END
END GetSystemTypeMinMax ;


(*
   IsISOPseudoSystemFunction -
*)

PROCEDURE IsISOPseudoSystemFunction (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN Iso AND ((sym=AddAdr) OR (sym=SubAdr) OR (sym=DifAdr) OR
                   (sym=MakeAdr) OR (sym=Rotate) OR (sym=Shift) OR
                   (sym=Cast))
END IsISOPseudoSystemFunction ;


(*
   IsPIMPseudoSystemFunction - returns TRUE if sym is specifically a PIM
                               system function.
*)

PROCEDURE IsPIMPseudoSystemFunction (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN (NOT Iso) AND ((sym=Size) OR (sym=Shift) OR (sym=Rotate))
END IsPIMPseudoSystemFunction ;


(*
   IsPseudoSystemFunction - returns true if sym is a SYSTEM pseudo function.
*)

PROCEDURE IsPseudoSystemFunction (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( (sym=Adr) OR (sym=TSize) OR (sym=TBitSize) OR
           IsPIMPseudoSystemFunction(sym) OR
           IsISOPseudoSystemFunction(sym) )
END IsPseudoSystemFunction ;


(*
   IsPseudoSystemFunctionConstExpression - returns TRUE if this procedure
                                           is legal in a constant expression.
*)

PROCEDURE IsPseudoSystemFunctionConstExpression (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (sym=Size) OR (sym=TSize) OR (sym=Rotate) OR (sym=Shift) OR
          (Iso AND ((sym=Cast) OR (sym=MakeAdr)))
         )
END IsPseudoSystemFunctionConstExpression ;


(*
   IsPseudoSystemProcedure - returns true if sym is a SYSTEM pseudo procedure.
*)

PROCEDURE IsPseudoSystemProcedure (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( sym=Throw )
END IsPseudoSystemProcedure ;


(*
   IsSystemType - returns TRUE if sym is a SYSTEM (inbuilt) type.
                  It does not search your SYSTEM implementation module.
*)

PROCEDURE IsSystemType (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsItemInList(SystemTypes, sym) )
END IsSystemType ;


(*
   GetSafeSystem -
*)

PROCEDURE GetSafeSystem (name: Name) : CARDINAL ;
VAR
   sym,
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(SystemTypes) ;
   i := 1 ;
   WHILE i<=n DO
      sym := GetItemFromList(SystemTypes, i) ;
      IF GetSymName(sym)=name
      THEN
         RETURN( sym )
      END ;
      INC(i)
   END ;
   RETURN( NulSym )
END GetSafeSystem ;


(*
   IntegerN - returns the symbol associated with INTEGER[N].
              NulSym is returned if the type does not exist.
*)

PROCEDURE IntegerN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   8 :  RETURN( GetSafeSystem(MakeKey('INTEGER8')) ) |
   16:  RETURN( GetSafeSystem(MakeKey('INTEGER16')) ) |
   32:  RETURN( GetSafeSystem(MakeKey('INTEGER32')) ) |
   64:  RETURN( GetSafeSystem(MakeKey('INTEGER64')) )

   ELSE
      InternalError ('system does not know about this type')
   END
END IntegerN ;


(*
   CardinalN - returns the symbol associated with CARDINAL[N].
               NulSym is returned if the type does not exist.
*)

PROCEDURE CardinalN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   8 :  RETURN( GetSafeSystem(MakeKey('CARDINAL8')) ) |
   16:  RETURN( GetSafeSystem(MakeKey('CARDINAL16')) ) |
   32:  RETURN( GetSafeSystem(MakeKey('CARDINAL32')) ) |
   64:  RETURN( GetSafeSystem(MakeKey('CARDINAL64')) )

   ELSE
      InternalError ('system does not know about this type')
   END
END CardinalN ;


(*
   WordN - returns the symbol associated with WORD[N].
           NulSym is returned if the type does not exist.
*)

PROCEDURE WordN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   16:  RETURN( GetSafeSystem(MakeKey('WORD16')) ) |
   32:  RETURN( GetSafeSystem(MakeKey('WORD32')) ) |
   64:  RETURN( GetSafeSystem(MakeKey('WORD64')) )

   ELSE
      InternalError ('system does not know about this type')
   END
END WordN ;


(*
   SetN - returns the symbol associated with SET[N].
          NulSym is returned if the type does not exist.
*)

PROCEDURE SetN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   8 :  RETURN( GetSafeSystem(MakeKey('BITSET8')) ) |
   16:  RETURN( GetSafeSystem(MakeKey('BITSET16')) ) |
   32:  RETURN( GetSafeSystem(MakeKey('BITSET32')) )

   ELSE
      InternalError ('system does not know about this type')
   END
END SetN ;


(*
   RealN - returns the symbol associated with REAL[N].
           NulSym is returned if the type does not exist.
*)

PROCEDURE RealN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   32 :  RETURN( GetSafeSystem(MakeKey('REAL32')) ) |
   64 :  RETURN( GetSafeSystem(MakeKey('REAL64')) ) |
   96 :  RETURN( GetSafeSystem(MakeKey('REAL96')) ) |
   128:  RETURN( GetSafeSystem(MakeKey('REAL128')) )

   ELSE
      InternalError ('system does not know about this type')
   END
END RealN ;


(*
   ComplexN - returns the symbol associated with COMPLEX[N].
              NulSym is returned if the type does not exist.
*)

PROCEDURE ComplexN (bitlength: CARDINAL) : CARDINAL ;
BEGIN
   CASE bitlength OF

   32 :  RETURN( GetSafeSystem(MakeKey('COMPLEX32')) ) |
   64 :  RETURN( GetSafeSystem(MakeKey('COMPLEX64')) ) |
   96 :  RETURN( GetSafeSystem(MakeKey('COMPLEX96')) ) |
   128:  RETURN( GetSafeSystem(MakeKey('COMPLEX128')) )

   ELSE
      InternalError ('system does not know about this type')
   END
END ComplexN ;


(*
   IsIntegerN - returns the TRUE if, sym, is one of the SYSTEM
                INTEGER types (not the base INTEGER type).
*)

PROCEDURE IsIntegerN (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (sym#NulSym) AND
          ((sym=IntegerN(8)) OR (sym=IntegerN(16)) OR
           (sym=IntegerN(32)) OR (sym=IntegerN(64)))
         )
END IsIntegerN ;


(*
   IsCardinalN - returns the TRUE if, sym, is one of the SYSTEM
                 CARDINAL types (not the base CARDINAL type).
*)

PROCEDURE IsCardinalN (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (sym#NulSym) AND
          ((sym=CardinalN(8)) OR (sym=CardinalN(16)) OR
           (sym=CardinalN(32)) OR (sym=CardinalN(64)))
         )
END IsCardinalN ;


(*
   IsWordN - returns the TRUE if, sym, is one of the SYSTEM
             WORD[n] types (not the default SYSTEM WORD type).
*)

PROCEDURE IsWordN (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (sym#NulSym) AND
          ((sym=WordN(16)) OR
           (sym=WordN(32)) OR (sym=WordN(64)))
         )
END IsWordN ;


(*
   IsSetN - returns the TRUE if, sym, is one of the SYSTEM
            SET[n] types (not the default SYSTEM BITSET type).
*)

PROCEDURE IsSetN (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (sym#NulSym) AND
          ((sym=SetN(8)) OR (sym=SetN(16)) OR (sym=SetN(32)))
         )
END IsSetN ;


(*
   IsRealN - returns the TRUE if, sym, is one of the SYSTEM
             REAL[n] types (not the default base REAL type).
*)

PROCEDURE IsRealN (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (sym#NulSym) AND
          ((sym=RealN(32)) OR (sym=RealN(64)) OR
           (sym=RealN(96)) OR (sym=RealN(128)))
         )
END IsRealN ;


(*
   IsComplexN - returns the TRUE if, sym, is one of the SYSTEM
                COMPLEX[n] types (not the default base COMPLEX,
                LONGCOMPLEX or SHORTCOMPLEX types).
*)

PROCEDURE IsComplexN (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (sym#NulSym) AND
          ((sym=ComplexN(32)) OR (sym=ComplexN(64)) OR
           (sym=ComplexN(96)) OR (sym=ComplexN(128)))
         )
END IsComplexN ;


(*
   IsGenericSystemType - returns TRUE if, sym, is of type
                         BYTE, WORD or any other length.
*)

PROCEDURE IsGenericSystemType (sym: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN(
          (sym#NulSym) AND
          (IsWordN(sym) OR (sym=Word) OR (sym=Byte) OR (sym=Loc))
         )
END IsGenericSystemType ;


(*
   IsSameSize - return TRUE if SIZE(a)=SIZE(b)
*)

PROCEDURE IsSameSize (a, b: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( AreConstantsEqual(BuildSize(BuiltinsLocation(), Mod2Gcc(a), FALSE),
                             BuildSize(BuiltinsLocation(), Mod2Gcc(b), FALSE)) )
END IsSameSize ;


(*
   IsSameType - returns TRUE if, t, is the same type as a or b
                and a or b are a type, p.
*)

PROCEDURE IsSameType (t: CARDINAL; p: IsP; a, b: CARDINAL) : BOOLEAN ;
BEGIN
   IF t=a
   THEN
      RETURN( p(b) AND IsSameSize(a, b) )
   ELSIF t=b
   THEN
      RETURN( p(a) AND IsSameSize(a, b) )
   ELSE
      RETURN( FALSE )
   END
END IsSameType ;


(*
   IsSameSizePervasiveType - returns TRUE if a or b are CARDINAL, INTEGER, REAL,
                             LONGREAL, SHORTREAL and the other type is the same
                             size and of the same type.
*)

PROCEDURE IsSameSizePervasiveType (a, b: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN( IsSameType(Integer, IsIntegerN, a, b) OR
           IsSameType(Cardinal, IsCardinalN, a, b) OR
           IsSameType(Word, IsWordN, a, b) OR
           IsSameType(Real, IsRealN, a, b) OR
           IsSameType(Complex, IsComplexN, a, b) OR
           IsSameType(LongInt, IsIntegerN, a, b) OR
           IsSameType(LongCard, IsCardinalN, a, b) OR
           IsSameType(LongComplex, IsComplexN, a, b) OR
           IsSameType(LongReal, IsRealN, a, b) OR
           IsSameType(ShortInt, IsIntegerN, a, b) OR
           IsSameType(ShortCard, IsCardinalN, a, b) OR
           IsSameType(ShortComplex, IsComplexN, a, b) OR
           IsSameType(ShortReal, IsRealN, a, b) )
END IsSameSizePervasiveType ;


END M2System.
