(* SYSTEM.mod provides access to the SYSTEM dependent module.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE SYSTEM ;

FROM libc IMPORT memcpy, memcpy, memset ;

CONST
   BitsPerBitset = MAX(BITSET)+1 ;


(*
   Max - returns the maximum of a and b.
*)

PROCEDURE Max (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a>b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Max ;


(*
   Min - returns the minimum of a and b.
*)

PROCEDURE Min (a, b: CARDINAL) : CARDINAL ;
BEGIN
   IF a<b
   THEN
      RETURN( a )
   ELSE
      RETURN( b )
   END
END Min ;


(*
   ShiftVal - is a runtime procedure whose job is to implement
              the SHIFT procedure of ISO SYSTEM. GNU Modula-2 will
              inline a SHIFT of a single WORD sized set and will only
              call this routine for larger sets.
*)

PROCEDURE ShiftVal (VAR s, d: ARRAY OF BITSET;
                    SetSizeInBits: CARDINAL;
                    ShiftCount: INTEGER) ;
VAR
   a: ADDRESS ;
BEGIN
   IF ShiftCount>0
   THEN
      ShiftCount := ShiftCount MOD VAL(INTEGER, SetSizeInBits) ;
      ShiftLeft(s, d, SetSizeInBits, ShiftCount)
   ELSIF ShiftCount<0
   THEN
      ShiftCount := (-ShiftCount) MOD VAL(INTEGER, SetSizeInBits) ;
      ShiftRight(s, d, SetSizeInBits, ShiftCount)
   ELSE
      a := memcpy(ADR(d), ADR(s), (HIGH(d)+1)*SIZE(BITSET))
   END
END ShiftVal ;


(*
   ShiftLeft - performs the shift left for a multi word set.
               This procedure might be called by the back end of
               GNU Modula-2 depending whether amount is known at compile
               time.
*)

PROCEDURE ShiftLeft (VAR s, d: ARRAY OF BITSET;
                     SetSizeInBits: CARDINAL;
                     ShiftCount: CARDINAL) ;
VAR
   lo, hi : BITSET ;
   i, j, h: CARDINAL ;
   a      : ADDRESS ;
BEGIN
   h := HIGH(s)+1 ;
   IF ShiftCount MOD BitsPerBitset=0
   THEN
      i := ShiftCount DIV BitsPerBitset ;
      a := ADR(d[i]) ;
      a := memcpy(a, ADR(s), (h-i)*SIZE(BITSET)) ;
      a := memset(ADR(d), 0, i*SIZE(BITSET))
   ELSE
      i := h ;
      WHILE i>0 DO
         DEC(i) ;
         lo := SHIFT(s[i], ShiftCount MOD BitsPerBitset) ;
         hi := SHIFT(s[i], -(BitsPerBitset - (ShiftCount MOD BitsPerBitset))) ;
         d[i] := BITSET{} ;
         j := i + ShiftCount DIV BitsPerBitset ;
         IF j<h
         THEN
            d[j] := d[j] + lo ;
            INC(j) ;
            IF j<h
            THEN
               d[j] := d[j] + hi
            END
         END
      END
   END
END ShiftLeft ;


(*
   ShiftRight - performs the shift left for a multi word set.
                This procedure might be called by the back end of
                GNU Modula-2 depending whether amount is known at compile
                time.
*)

PROCEDURE ShiftRight (VAR s, d: ARRAY OF BITSET;
                      SetSizeInBits: CARDINAL;
                      ShiftCount: CARDINAL) ;
VAR
   lo, hi : BITSET ;
   j, i, h: INTEGER ;
   a      : ADDRESS ;
BEGIN
   h := HIGH (s) + 1 ;
   IF ShiftCount MOD BitsPerBitset = 0
   THEN
      i := ShiftCount DIV BitsPerBitset ;
      a := ADR (s[i]) ;
      j := h-i ;
      a := memcpy (ADR (d), a, j * VAL (INTEGER, SIZE (BITSET))) ;
      a := ADR (d[j]) ;
      a := memset (a, 0, i * VAL (INTEGER, SIZE (BITSET)))
   ELSE
      i := 0 ;
      WHILE i<h DO
         lo := SHIFT(s[i], BitsPerBitset - (ShiftCount MOD BitsPerBitset)) ;
         hi := SHIFT(s[i], -(ShiftCount MOD BitsPerBitset)) ;
         d[i] := BITSET{} ;
         j := i - VAL (INTEGER, ShiftCount DIV BitsPerBitset) ;
         IF j>=0
         THEN
            d[j] := d[j] + hi ;
            DEC(j) ;
            IF j>=0
            THEN
               d[j] := d[j] + lo
            END
         END ;
         INC(i)
      END
   END
END ShiftRight ;


(*
   RotateVal - is a runtime procedure whose job is to implement
               the ROTATE procedure of ISO SYSTEM. GNU Modula-2 will
               inline a ROTATE of a single WORD (or less)
               sized set and will only call this routine for larger sets.
*)

PROCEDURE RotateVal (VAR s, d: ARRAY OF BITSET;
                     SetSizeInBits: CARDINAL;
                     RotateCount: INTEGER) ;
VAR
   a: ADDRESS ;
BEGIN
   IF RotateCount>0
   THEN
      RotateCount := RotateCount MOD VAL(INTEGER, SetSizeInBits)
   ELSIF RotateCount<0
   THEN
      RotateCount := -VAL(INTEGER, VAL(CARDINAL, -RotateCount) MOD SetSizeInBits)
   END ;
   IF RotateCount>0
   THEN
      RotateLeft(s, d, SetSizeInBits, RotateCount)
   ELSIF RotateCount<0
   THEN
      RotateRight(s, d, SetSizeInBits, -RotateCount)
   ELSE
      (* no rotate required, but we must copy source to dest.  *)
      a := memcpy(ADR(d), ADR(s), (HIGH(d)+1)*SIZE(BITSET))
   END
END RotateVal ;


(*
   RotateLeft - performs the rotate left for a multi word set.
                This procedure might be called by the back end of
                GNU Modula-2 depending whether amount is known at compile
                time.
*)

PROCEDURE RotateLeft (VAR s, d: ARRAY OF BITSET;
                      SetSizeInBits: CARDINAL;
                      RotateCount: CARDINAL) ;
VAR
   lo, hi : BITSET ;
   b, i, j, h: CARDINAL ;
BEGIN
   h := HIGH(s) ;
   (* firstly we set d := {} *)
   i := 0 ;
   WHILE i<=h DO
      d[i] := BITSET{} ;
      INC(i)
   END ;
   i := h+1 ;
   RotateCount := RotateCount MOD SetSizeInBits ;
   b := SetSizeInBits MOD BitsPerBitset ;
   IF b=0
   THEN
      b := BitsPerBitset
   END ;
   WHILE i>0 DO
      DEC(i) ;
      lo := SHIFT(s[i], RotateCount MOD BitsPerBitset) ;
      hi := SHIFT(s[i], -(b - (RotateCount MOD BitsPerBitset))) ;
      j := ((i*BitsPerBitset + RotateCount) MOD
            SetSizeInBits) DIV BitsPerBitset ;
      d[j] := d[j] + lo ;
      j := (((i+1)*BitsPerBitset + RotateCount) MOD
            SetSizeInBits) DIV BitsPerBitset ;
      d[j] := d[j] + hi ;
      b := BitsPerBitset
   END
END RotateLeft ;


(*
   RotateRight - performs the rotate right for a multi word set.
                 This procedure might be called by the back end of
                 GNU Modula-2 depending whether amount is known at compile
                 time.
*)

PROCEDURE RotateRight (VAR s, d: ARRAY OF BITSET;
                       SetSizeInBits: CARDINAL;
                       RotateCount: CARDINAL) ;
BEGIN
   RotateLeft(s, d, SetSizeInBits, SetSizeInBits-RotateCount)
END RotateRight ;


END SYSTEM.
