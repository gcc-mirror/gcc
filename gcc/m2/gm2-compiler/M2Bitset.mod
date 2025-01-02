(* M2Bitset.mod provides the BITSET type.

Copyright (C) 2003-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2Bitset ;


FROM M2Debug IMPORT Assert ;
FROM m2linemap IMPORT BuiltinsLocation ;
FROM m2type IMPORT GetWordType ;
FROM m2decl IMPORT GetBitsPerBitset ;
FROM m2expr IMPORT GetSizeOf ;
FROM M2ALU IMPORT PushCard, PushIntegerTree ;
FROM NameKey IMPORT MakeKey ;
FROM M2System IMPORT Word ;
FROM M2Base IMPORT Cardinal ;
FROM M2LexBuf IMPORT BuiltinTokenNo ;

FROM SymbolTable IMPORT NulSym,
      	       	     	MakeConstLit,
                        MakeConstVar,
      	       	     	MakeSet,
      	       	     	MakeSubrange,
      	       	     	PutSet,
      	       	     	PutSubrange,
                        PopValue,
                        PopSize ;


VAR
   MinBitset, MaxBitset : CARDINAL ;


(*
   MakeBitset - creates and declares the type BITSET.
*)

PROCEDURE MakeBitset ;
BEGIN
   Bitset := MakeSet (BuiltinTokenNo, MakeKey('BITSET')) ;     (* Base Type       *)

   (* MinBitset *)
   MinBitset := MakeConstLit (BuiltinTokenNo, MakeKey('0'), Cardinal) ;

   (* MaxBitset *)
   MaxBitset := MakeConstVar (BuiltinTokenNo, MakeKey('MaxBitset')) ;
   PushCard (GetBitsPerBitset()-1) ;
   PopValue (MaxBitset) ;

   Assert (Word#NulSym) ;
   Bitnum := MakeSubrange (BuiltinTokenNo, MakeKey('BITNUM')) ;
   PutSubrange (Bitnum, MinBitset, MaxBitset, Cardinal) ;
   PutSet (Bitset, Bitnum, FALSE) ;

   PushIntegerTree (GetSizeOf(BuiltinsLocation(), GetWordType())) ;
   PopSize (Bitset)
END MakeBitset ;


(*
   GetBitsetMinMax - assigns min and max to the minimum and maximum values of BITSET.
*)

PROCEDURE GetBitsetMinMax (VAR min, max: CARDINAL) ;
BEGIN
   min := MinBitset ;
   max := MaxBitset
END GetBitsetMinMax ;


END M2Bitset.
