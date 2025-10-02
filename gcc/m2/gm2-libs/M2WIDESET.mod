(* M2WIDESET.mod runtime support procedures for wide sets.

Copyright (C) 2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE M2WIDESET ;

FROM SYSTEM IMPORT TBITSIZE, ADDRESS, ADR, SHIFT ;
FROM Builtins IMPORT memcpy, memset ;
FROM libc IMPORT printf ;

TYPE
   BYTESET = PACKEDSET OF [0..7] ;
   PtrToByteset = POINTER TO BYTESET ;
   PtrToBitset = POINTER TO BITSET ;


CONST
   EnableOptimizeBitset = TRUE ;
   EnableDebugging      = FALSE ;


(*
   BitsPerByteset = TSIZE (BYTESET) * 8 ;
*)


(*
   DumpSet -
*)

PROCEDURE DumpSet (set: ARRAY OF BYTE; highbit: CARDINAL) ;
VAR
   count,
   i, high: CARDINAL ;
   last   : BYTE ;
BEGIN
   high := HIGH (set) ;
   printf ("set highbit = %d, high indice = %d\n", highbit, high) ;
   printf ("{ ") ;
   last := set[0] ;
   i := 1 ;
   count := 1 ;
   printf (" 0: 0x%02x", VAL (CARDINAL, last)) ;
   WHILE i <= high DO
      IF last = set[i]
      THEN
         INC (count)
      ELSE
         IF count > 1
         THEN
            printf (" x %d, %d: 0x%02x", count, i, VAL (CARDINAL, set[i]))
         ELSE
            IF i > 0
            THEN
               printf (",")
            END ;
            printf (" %d: 0x%02x", i, VAL (CARDINAL, set[i]))
         END ;
         last := set[i] ;
         count := 1
      END ;
      INC (i)
   END ;
   IF count > 1
   THEN
      printf (" x %d ", count)
   END ;
   printf (" }\n")
END DumpSet ;


(*
   Or - dest = left | right
        implement OR for a wide set type.
*)

PROCEDURE Or (VAR dest: ARRAY OF BYTE; left, right: ARRAY OF BYTE;
              highbit: CARDINAL) ;
VAR
   i,
   bit,
   high,
   lastbit: CARDINAL ;
   byteset: BYTESET ;
BEGIN
   IF EnableDebugging
   THEN
      printf ("left\n");
      DumpSet (left, highbit) ;
      printf ("right\n");
      DumpSet (right, highbit)
   END ;
   high := HIGH (dest) ;
   i := 0 ;
   WHILE i < high DO
      IF EnableDebugging
      THEN
         printf ("%02x or %02x", left[i], right[i])
      END ;
      dest[i] := BYTESET (left[i]) + BYTESET (right[i]) ;
      IF EnableDebugging
      THEN
         printf (" -> %02x\n", dest[i])
      END ;
      INC (i)
   END ;
   IF i = high
   THEN
      lastbit := highbit MOD TBITSIZE (BYTE) ;
      IF lastbit = 0
      THEN
         dest[i] := BYTESET (left[i]) + BYTESET (right[i])
      ELSE
         byteset := dest[i] ;
         FOR bit := 0 TO lastbit DO
            IF (bit IN BYTESET (left[i])) OR (bit IN BYTESET (right[i]))
            THEN
               INCL (byteset, bit)
            ELSE
               EXCL (byteset, bit)
            END
         END ;
         dest[i] := byteset
      END
   ELSE
      HALT
   END
END Or ;


(*
   And - dest = left & right
         implement AND for a wide set type.
*)

PROCEDURE And (VAR dest: ARRAY OF BYTE; left, right: ARRAY OF BYTE;
               highbit: CARDINAL) ;
VAR
   i,
   bit,
   high,
   lastbit: CARDINAL ;
   byteset: BYTESET ;
BEGIN
   high := HIGH (dest) ;
   i := 0 ;
   WHILE i < high DO
      dest[i] := BYTESET (left[i]) * BYTESET (right[i]) ;
      INC (i)
   END ;
   IF i = high
   THEN
      lastbit := highbit MOD TBITSIZE (BYTE) ;
      IF lastbit = 0
      THEN
         dest[i] := BYTESET (left[i]) * BYTESET (right[i])
      ELSE
         byteset := dest[i] ;
         FOR bit := 0 TO lastbit DO
            IF (bit IN BYTESET (left[i])) AND (bit IN BYTESET (right[i]))
            THEN
               INCL (byteset, bit)
            ELSE
               EXCL (byteset, bit)
            END
         END ;
         dest[i] := byteset
      END
   ELSE
      HALT
   END
END And ;


(*
   Not - dest = ~ expr
         implement NOT for a wide set type.
*)

PROCEDURE Not (VAR dest: ARRAY OF BYTE; expr: ARRAY OF BYTE;
               highbit: CARDINAL) ;
VAR
   i,
   bit,
   high,
   lastbit: CARDINAL ;
   byteset: BYTESET ;
BEGIN
   high := HIGH (dest) ;
   i := 0 ;
   WHILE i < high DO
      dest[i] := - BYTESET (expr[i]) ;
      INC (i)
   END ;
   IF i = high
   THEN
      lastbit := highbit MOD TBITSIZE (BYTE) ;
      IF lastbit = 0
      THEN
         dest[i] := - BYTESET (expr[i])
      ELSE
         byteset := dest[i] ;
         FOR bit := 0 TO lastbit DO
            IF bit IN BYTESET (expr[i])
            THEN
               EXCL (byteset, bit)
            ELSE
               INCL (byteset, bit)
            END
         END ;
         dest[i] := byteset
      END
   ELSE
      HALT
   END
END Not ;


(*
   Incl - dest |= bit
          implement INCL for a wide set type.
*)

PROCEDURE Incl (VAR dest: ARRAY OF BYTE; bit: CARDINAL) ;
VAR
   byteset: BYTESET ;
   byteno,
   bitno,
   high   : CARDINAL ;
BEGIN
   high := HIGH (dest) ;
   byteno := bit DIV TBITSIZE (BYTE) ;
   bitno := bit MOD TBITSIZE (BYTE) ;
   IF byteno <= high
   THEN
      byteset := BYTESET (dest[byteno]) ;
      INCL (byteset, bitno) ;
      dest[byteno] := byteset
   ELSE
      HALT
   END
END Incl ;


(*
   Excl - dest &= (~ bit)
          implement EXCL for a wide set type.
*)

PROCEDURE Excl (VAR dest: ARRAY OF BYTE; bit: CARDINAL) ;
VAR
   byteset: BYTESET ;
   byteno,
   bitno,
   high   : CARDINAL ;
BEGIN
   high := HIGH (dest) ;
   byteno := bit DIV TBITSIZE (BYTE) ;
   bitno := bit MOD TBITSIZE (BYTE) ;
   IF byteno <= high
   THEN
      byteset := BYTESET (dest[byteno]) ;
      EXCL (byteset, bitno) ;
      dest[byteno] := byteset
   ELSE
      HALT
   END
END Excl ;


(*
   In - return bit IN expr.
*)

PROCEDURE In (VAR expr: ARRAY OF BYTE; bit: CARDINAL) : BOOLEAN ;
VAR
   byteno,
   bitno,
   high  : CARDINAL ;
BEGIN
   high := HIGH (expr) ;
   byteno := bit DIV TBITSIZE (BYTE) ;
   bitno := bit MOD TBITSIZE (BYTE) ;
   IF byteno <= high
   THEN
      RETURN bitno IN BYTESET (expr[byteno])
   ELSE
      HALT
   END
END In ;


(*
   Empty - return TRUE if expr = {}.
*)

PROCEDURE Empty (expr: ARRAY OF BYTE; highbit: CARDINAL) : BOOLEAN ;
VAR
   i,
   bit,
   high,
   lastbit: CARDINAL ;
BEGIN
   high := HIGH (expr) ;
   i := 0 ;
   WHILE i < high DO
      IF expr[i] # BYTE (0)
      THEN
         RETURN FALSE
      END ;
      INC (i)
   END ;
   IF i = high
   THEN
      lastbit := highbit MOD TBITSIZE (BYTE) ;
      IF lastbit = 0
      THEN
         RETURN expr[i] = BYTE (0)
      ELSE
         FOR bit := 0 TO lastbit DO
            IF bit IN BYTESET (expr[i])
            THEN
               RETURN FALSE
            END
         END
      END
   ELSE
      HALT
   END ;
   RETURN TRUE
END Empty ;


(*
   Clear - set dest := {}.
*)

PROCEDURE Clear (VAR dest: ARRAY OF BYTE; highbit: CARDINAL) ;
VAR
   i,
   bit,
   high,
   lastbit: CARDINAL ;
   byteset: BYTESET ;
BEGIN
   high := HIGH (dest) ;
   IF EnableOptimizeBitset
   THEN
      IF memset (ADR (dest), 0, high) = NIL
      THEN
      END ;
      i := high
   ELSE
      i := 0 ;
      WHILE i < high DO
         dest[i] := BYTE (0) ;
         INC (i)
      END
   END ;
   IF i = high
   THEN
      lastbit := highbit MOD TBITSIZE (BYTE) ;
      IF lastbit = 0
      THEN
         dest[i] := BYTE (0)
      ELSE
         byteset := dest[i] ;
         FOR bit := 0 TO lastbit DO
            EXCL (byteset, bit)
         END ;
         dest[i] := byteset
      END
   ELSE
      HALT
   END
END Clear ;


(*
   Equal - return left = right.
*)

PROCEDURE Equal (VAR left, right: ARRAY OF BYTE; highbit: CARDINAL) : BOOLEAN ;
VAR
   i,
   bit,
   high,
   lastbit   : CARDINAL ;
   rptr, lptr: PtrToByteset ;
   lb, rb    : BOOLEAN ;
BEGIN
   IF EnableDebugging
   THEN
      printf ("Equal left : ");
      DumpSet (left, highbit) ;
      printf ("      right: ");
      DumpSet (right, highbit) ;
   END ;

   high := HIGH (left) ;
   IF high = HIGH (right)
   THEN
      i := 0 ;
      WHILE i < high DO
         IF left[i] # right[i]
         THEN
            RETURN FALSE
         END ;
         INC (i)
      END ;
      IF i = high
      THEN
         lastbit := highbit MOD TBITSIZE (BYTE) ;
         IF lastbit = 7
         THEN
            (* All bits 0..7 inclusive can be tested.  *)
            RETURN left[i] = right[i]
         END ;
         rptr := ADR (right[i]) ;
         lptr := ADR (left[i]) ;
         (* Only check the bits in the set which are used in the last byte.  *)
         FOR bit := 0 TO lastbit DO
            (*
            IF (bit IN rptr^) # (bit IN lptr^)
            THEN
               RETURN FALSE
            END
            *)
            lb := bit IN lptr^ ;    (* Replace with the above - when the bug is fixed.  *)
            rb := bit IN rptr^ ;
            IF lb # rb
            THEN
               RETURN FALSE
            END
         END
      END
   ELSE
      HALT
   END ;
   RETURN TRUE
END Equal ;


(*
   ShiftLeft - performs the shift left for a multi word set.
*)

PROCEDURE ShiftLeft (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE;
                     highbit: CARDINAL;
                     ShiftCount: CARDINAL) ;
VAR
   byteshift,
   bitshift : CARDINAL ;
BEGIN
   byteshift := ShiftCount DIV TBITSIZE (BYTESET) ;
   bitshift := ShiftCount MOD TBITSIZE (BYTESET) ;
   ShiftLeftByteBit (dest, src, highbit, byteshift, bitshift)
END ShiftLeft ;


(*
   ShiftLeftByteBit - shifts src left by byteshift and bitshift.  It
                      moves the bottom bitshift bits from lo into the
                      first byte.
*)

PROCEDURE ShiftLeftByteBit (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE;
                            highbit: CARDINAL;
                            byteshift, bitshift: CARDINAL) ;
VAR
   top, bot, mid       : BYTESET ;
   i, h, fromIdx, toIdx: CARDINAL ;
BEGIN
   (* Copy the bytes into dest at the mostly correct position
      (modulo byte position).  *)
   toIdx := 0 ;
   fromIdx := 0 ;
   WHILE toIdx < byteshift DO
      dest[toIdx] := BYTE (0) ;
      INC (toIdx)
   END ;
   WHILE toIdx <= HIGH (dest) DO
      dest[toIdx] := src[fromIdx] ;
      INC (toIdx) ;
      INC (fromIdx)
   END ;
   (* And adjust by bit shifting.  *)
   IF bitshift > 0
   THEN
      bot := BYTE (0) ;
      h := HIGH (dest) ;
      i := 0 ;
      WHILE i < h DO
         mid := dest[i] ;
         (* Shift right by TBITSIZE (BYTE) - bitshift.  *)
         top := SHIFT (mid, - INTEGER ((TBITSIZE (BYTE) - bitshift))) ;  (* Right must be negative.  *)
         mid := SHIFT (mid, bitshift) ;   (* Shift left.  *)
         dest[i] := mid + bot ;
         bot := top ;
         INC (i)
      END ;
      mid := dest[h] ;
      mid := SHIFT (mid, bitshift) ;   (* Shift left.  *)
      dest[h] := mid + bot
   END
END ShiftLeftByteBit ;


(*
   ShiftRight - performs the shift rightt for a multi word set.
*)

PROCEDURE ShiftRight (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE;
                     highbit: CARDINAL;
                     ShiftCount: CARDINAL) ;
VAR
   byteshift,
   bitshift : CARDINAL ;
BEGIN
   IF EnableDebugging
   THEN
      printf ("highbit = %d, ShiftCount = %d\n",
              highbit, ShiftCount)
   END ;
   byteshift := ShiftCount DIV TBITSIZE (BYTESET) ;
   bitshift := ShiftCount MOD TBITSIZE (BYTESET) ;
   IF EnableDebugging
   THEN
      printf ("SIZE (byteset) = %d, TBITSIZE (byteset) = %d\n",
              SIZE (BYTESET), TBITSIZE (BYTESET));
      printf ("  byteshift = %d, bitshift = %d\n",
              byteshift, bitshift)
   END ;
   ShiftRightByteBit (dest, src, highbit, byteshift, bitshift)
END ShiftRight ;


(*
   ShiftRightByteBit - shifts src left by byteshift and bitshift.  It
                      moves the bottom bitshift bits from lo into the
                      first byte.
*)

PROCEDURE ShiftRightByteBit (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE;
                             highbit: CARDINAL;
                             byteshift, bitshift: CARDINAL) ;
VAR
   top, bot, mid       : BYTESET ;
   i, h, toIdx, fromIdx: CARDINAL ;
BEGIN
   (* Copy the bytes.  *)
   toIdx := 0 ;
   fromIdx := byteshift ;
   IF EnableDebugging
   THEN
      printf ("HIGH (dest) = %d\n", HIGH (dest))
   END ;
   IF byteshift <= HIGH (dest)
   THEN
      h := HIGH (dest) - byteshift ;
      WHILE toIdx <= h DO
         dest[toIdx] := src[fromIdx] ;
         INC (toIdx) ;
         INC (fromIdx)
      END
   END ;
   WHILE toIdx <= HIGH (dest) DO
      dest[toIdx] := BYTE (0) ;
      INC (toIdx)
   END ;
   (* And bit shift the remainder.  *)
   IF EnableDebugging
   THEN
      printf ("bitshift = %d\n", bitshift)
   END ;
   IF bitshift > 0
   THEN
      top := BYTE (0) ;
      i := HIGH (dest) ;
      WHILE i > 0 DO
         mid := dest[i] ;
         bot := SHIFT (mid, TBITSIZE (BYTE) - bitshift) ;   (* Shift left.  *)
         mid := SHIFT (mid, - INTEGER (bitshift)) ;  (* Shift right by bitshift.  *)
         dest[i] := top + mid ;
         top := bot ;
         DEC (i)
      END ;
      mid := dest[0] ;
      mid := SHIFT (mid, - INTEGER (bitshift)) ;  (* Shift right by bitshift.  *)
      dest[0] := top + mid
   END
END ShiftRightByteBit ;


(*
   Shift - dest := SHIFT (src, ShiftCount).
*)

PROCEDURE Shift (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE;
                 highbit: CARDINAL; ShiftCount: INTEGER) ;
BEGIN
   IF ShiftCount > 0
   THEN
      IF EnableDebugging
      THEN
         printf ("Shift Left: ") ;
         DumpSet (src, highbit)
      END ;
      ShiftCount := ShiftCount MOD VAL (INTEGER, (highbit + 1)) ;
      ShiftLeft (dest, src, highbit, ShiftCount) ;
      IF EnableDebugging
      THEN
         printf ("  Result of shift Left: ") ;
         DumpSet (dest, highbit)
      END
   ELSIF ShiftCount < 0
   THEN
      IF EnableDebugging
      THEN
         printf ("Shift Right: ") ;
         DumpSet (src, highbit)
      END ;
      ShiftCount := (-ShiftCount) MOD VAL (INTEGER, (highbit + 1)) ;
      ShiftRight (dest, src, highbit, ShiftCount) ;
      IF EnableDebugging
      THEN
         printf ("  Result of shift right: ") ;
         DumpSet (dest, highbit)
      END
   ELSE
      IF memcpy (ADR (dest), ADR (src), (HIGH (dest) + 1) * SIZE (BYTE)) = NIL
      THEN
      END
   END
END Shift ;


(*
   ArithShift - dest := ArithShift (dest, ShiftCount, carry).  This is an
                arithmetic shift all the new bit values will
                be set to carry.
*)

PROCEDURE ArithShiftLeft (VAR dest: ARRAY OF BYTE;
                          highbit: CARDINAL; ShiftCount: CARDINAL;
                          carry: BOOLEAN) ;
BEGIN
   WHILE ShiftCount > 0 DO
      ArithShiftLeftBit (dest, highbit, carry) ;
      DEC (ShiftCount)
   END
END ArithShiftLeft ;


(*
   ArithShiftLeftBit - shift set left by one bit.  Carry will appear at
                       bit position 0.  Any unused bits on the high byte
                       are unaffected.
*)

PROCEDURE ArithShiftLeftBit (VAR dest: ARRAY OF BYTE; highbit: CARDINAL;
                             carry: BOOLEAN) ;
CONST
   MSB = TBITSIZE (BYTE) - 1 ;
VAR
   topbit,
   i,
   high  : CARDINAL ;
   next  : BOOLEAN ;
   mask,
   unused,
   setb  : BYTESET ;
BEGIN
   IF EnableDebugging
   THEN
      printf ("ArithShiftLeft enter\n");
      DumpSet (dest, highbit)
   END ;

   high := HIGH (dest) ;
   (* We ripple through the bytes 0..high-1, propagating local carry between
      bytes.  *)
   i := 0 ;
   WHILE i < high DO
      setb := dest[i] ;
      next := MSB IN setb ;
      setb := SHIFT (setb, 1) ;  (* Shift left.  *)
      IF carry
      THEN
         INCL (setb, 0)   (* Set bit 0.  *)
      END ;
      dest[i] := setb ;
      carry := next ;
      IF EnableDebugging
      THEN
         printf ("ArithShiftLeft shifted byte dest[%d]\n", i);
         DumpSet (dest, highbit)
      END ;
      INC (i)
   END ;
   (* Last byte special case as there may be some unused bits which must be
      preserved.  *)
   setb := dest[high] ;
   unused := BYTESET {} ;  (* Will contain all top unused bits of dest[high].  *)
   mask := - BYTESET {} ;
   topbit := (highbit+1) MOD TBITSIZE (BYTE) ;
   WHILE topbit # 0 DO
      EXCL (mask, topbit) ;
      IF topbit IN setb
      THEN
         EXCL (setb, topbit) ;
         INCL (unused, topbit)
      END ;
      topbit := (topbit+1) MOD TBITSIZE (BYTE)
   END ;
   setb := SHIFT (setb, 1) ;  (* Left shift.  *)
   IF carry
   THEN
      INCL (setb, 0)   (* Set bit 0.  *)
   END ;
   setb := setb * mask ;  (* Remove all unused bits.  *)
   setb := setb + unused ;  (* Restore original unused bits.  *)
   dest[high] := setb ;
   IF EnableDebugging
   THEN
      printf ("ArithShiftLeft shifted byte dest[%d]\n", high);
      DumpSet (dest, highbit)
   END
END ArithShiftLeftBit ;


(*
   ArithShiftRight - dest := ArithShiftRight (dest, ShiftCount, carry).
                     This is an arithmetic shift all the new bit values
                     will be set to carry.
*)

PROCEDURE ArithShiftRight (VAR dest: ARRAY OF BYTE;
                           highbit: CARDINAL; ShiftCount: CARDINAL;
                           carry: BOOLEAN) ;
BEGIN
   WHILE ShiftCount > 0 DO
      ArithShiftRightBit (dest, highbit, carry) ;
      DEC (ShiftCount)
   END
END ArithShiftRight ;


(*
   ArithShiftRightBit - shift set right by one bit and place carry in the
                        top most bit.
*)

PROCEDURE ArithShiftRightBit (VAR dest: ARRAY OF BYTE; highbit: CARDINAL;
                              carry: BOOLEAN) ;
CONST
   MSB = TBITSIZE (BYTE) - 1 ;
VAR
   topbit,
   i,
   high  : CARDINAL ;
   prev,
   next  : BOOLEAN ;
   mask,
   unused,
   setb  : BYTESET ;
BEGIN
   high := HIGH (dest) ;
   (* Clear any unused bits in the highest byte, but save them into unused.  *)
   setb := dest[high] ;
   unused := BYTESET {} ;
   topbit := (highbit+1) MOD TBITSIZE (BYTE) ;
   mask := - BYTESET {} ;
   WHILE topbit # 0 DO
      EXCL (mask, topbit) ;
      IF topbit IN setb
      THEN
         EXCL (setb, topbit) ;
         INCL (unused, topbit)
      END ;
      topbit := (topbit+1) MOD TBITSIZE (BYTE)
   END ;
   (* Start at the top and work down to byte 0.  *)
   setb := setb * mask ;  (* Ignore unused bits.  *)
   next := 0 IN setb ;   (* Next carry.  *)
   setb := SHIFT (setb, -1) ;   (* Shift right by 1 bit.  *)
   IF carry
   THEN
      INCL (setb, highbit MOD TBITSIZE (BYTE))
   END ;
   dest[high] := setb + unused ;  (* First byte is a special case as we
                                    have to preserve the unused bits.  *)
   (* Now we ripple through the remaining bytes, propagating local
      carry between bytes.  *)
   i := high ;
   WHILE i > 0 DO
      prev := next ;
      DEC (i) ;
      setb := dest[i] ;
      next := 0 IN setb ;
      setb := SHIFT (setb, -1) ;
      IF prev
      THEN
         INCL (setb, MSB)
      END ;
      dest[i] := setb
   END
END ArithShiftRightBit ;


(*
   ArithShift - dest := ArithShift (dest, ShiftCount, carry).  This is an
                arithmetic shift all the new bit values will
                be set to carry.
*)

PROCEDURE ArithShift (VAR dest: ARRAY OF BYTE;
                      highbit: CARDINAL; ShiftCount: INTEGER;
                      carry: BOOLEAN) ;
BEGIN
   IF EnableDebugging
   THEN
      printf ("Arith enter\n");
      DumpSet (dest, highbit)
   END ;
   IF ShiftCount > 0
   THEN
      ShiftCount := ShiftCount MOD VAL (INTEGER, (highbit + 1)) ;
      ArithShiftLeft (dest, highbit, ShiftCount, carry)
   ELSIF ShiftCount < 0
   THEN
      ShiftCount := (-ShiftCount) MOD VAL (INTEGER, (highbit + 1)) ;
      ArithShiftRight (dest, highbit, ShiftCount, carry)
   END ;
   IF EnableDebugging
   THEN
      printf ("Arith exit\n");
      DumpSet (dest, highbit)
   END
END ArithShift ;


(*
   Rotate - is a runtime procedure whose job is to implement
            the ROTATE procedure of ISO SYSTEM.
*)

PROCEDURE Rotate (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE;
                  highbit: CARDINAL; RotateCount: INTEGER) ;
BEGIN
   IF EnableDebugging
   THEN
      printf ("Rotate enter\n");
      DumpSet (src, highbit)
   END ;
   IF RotateCount > 0
   THEN
      RotateCount := RotateCount MOD VAL (INTEGER, highbit + 1)
   ELSIF RotateCount < 0
   THEN
      RotateCount := -VAL (INTEGER, VAL (CARDINAL, -RotateCount) MOD (highbit + 1))
   END ;
   IF RotateCount > 0
   THEN
      RotateLeft (dest, src, highbit, RotateCount)
   ELSIF RotateCount < 0
   THEN
      RotateRight (dest, src, highbit, -RotateCount)
   ELSE
      (* No rotate required, but we must copy source to dest.  *)
      IF memcpy (ADR (dest), ADR (src), (HIGH (dest) + 1) * SIZE (BYTE)) = NIL
      THEN
      END
   END ;
   IF EnableDebugging
   THEN
      printf ("Rotate exit\n");
      DumpSet (dest, highbit)
   END
END Rotate ;


(*
   RotateLeft - performs the rotate left for a multi word set.
*)

PROCEDURE RotateLeft (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE;
                      highbit: CARDINAL; RotateCount: CARDINAL) ;
VAR
   bit,  carry : BOOLEAN ;
   count,
   high,
   highplus1,
   highbitplus1,
   fromIdx, toIdx: CARDINAL ;
BEGIN
   IF EnableDebugging
   THEN
      printf ("RotateLeft enter\n");
      DumpSet (src, highbit)
   END ;

   (* Copy the contents rotating on byte granularity, then
      arithmetically shift the remaining number of bits.  *)
   high := HIGH (dest) ;
   fromIdx := 0 ;
   highplus1 := high + 1 ;
   highbitplus1 := highbit + 1 ;
   toIdx := RotateCount DIV TBITSIZE (BYTE) ;  (* Byte level granularity.  *)
   REPEAT
      dest[toIdx] := src[fromIdx] ;
      IF EnableDebugging
      THEN
         printf ("RotateLeft after partial byte movement: dest[%d] := src[%d]\n",
                 toIdx, fromIdx);
         DumpSet (dest, highbit)
      END ;
      fromIdx := (fromIdx + 1) MOD highplus1 ;
      toIdx := (toIdx + 1) MOD highplus1 ;
   UNTIL fromIdx = 0 ;

   IF EnableDebugging
   THEN
      printf ("RotateLeft after byte placement\n");
      DumpSet (dest, highbit)
   END ;

   (* Now ArithShiftLeft the remainder number of bits.  *)
   count := RotateCount MOD (TBITSIZE (BYTE)) ;
   WHILE count > 0 DO
      (* Get last bit.  *)
      bit := (highbit MOD TBITSIZE (BYTE)) IN BYTESET (dest[high]) ;
      (* Shift everything left wards and the last bit goes to bit
         position 0.  *)
      ArithShiftLeft (dest, highbit, 1, bit) ;
      DEC (count)
   END ;
   IF EnableDebugging
   THEN
      printf ("RotateLeft after bit shifting final placement\n");
      DumpSet (dest, highbit)
   END
END RotateLeft ;


(*
   RotateRight - performs the rotate right for a multi word set.
*)

PROCEDURE RotateRight (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE;
                       highbit: CARDINAL; RotateCount: CARDINAL) ;
BEGIN
   RotateLeft (dest, src, highbit, (highbit + 1) - RotateCount)
END RotateRight ;


(*
   Less - performs the set comparison for a wide set.
          Less returns ProperSubset (left, right, highbit).
*)

PROCEDURE Less (VAR left, right: ARRAY OF BYTE;
                highbit: CARDINAL) : BOOLEAN ;

BEGIN
   RETURN ProperSubset (left, right, highbit)
END Less ;


(*
   LessEqu - performs the set comparison for a wide set.
             LessEqu returns Equal (left, right, highbit) OR
                             ProperSubset (left, right, highbit).
*)

PROCEDURE LessEqu (VAR left, right: ARRAY OF BYTE;
                   highbit: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN Equal (left, right, highbit) OR
          ProperSubset (left, right, highbit)
END LessEqu ;


(*
   Gre - performs the set comparison for a wide set.
         Gre returns the result of
         ProperSuperet (left, right, highbit).
*)

PROCEDURE Gre (VAR left, right: ARRAY OF BYTE;
               highbit: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN ProperSuperset (left, right, highbit)
END Gre ;


(*
   GreEqu - performs the set comparison for a wide set.
            GreEqu returns Equal (left, right, highbit) OR
                           ProperSuperet (left, right, highbit).
*)

PROCEDURE GreEqu (VAR left, right: ARRAY OF BYTE;
                  highbit: CARDINAL) : BOOLEAN ;
BEGIN
   RETURN Equal (left, right, highbit) OR
          ProperSuperset (left, right, highbit)
END GreEqu ;


(*
   ProperSubset - return TRUE if left is a proper subset of right.
                  If true the left set will have at least one element
                  less than set right.
*)

PROCEDURE ProperSubset (VAR left, right: ARRAY OF BYTE;
                        highbit: CARDINAL) : BOOLEAN ;
VAR
   diffbits,
   diffright,
   diffleft : BYTESET ;
   rightmore: BOOLEAN ;
   i,
   bit,
   high,
   lastbit: CARDINAL ;
   lptr,
   rptr   : PtrToByteset ;
BEGIN
   high := HIGH (left) ;
   lptr := ADR (left) ;
   rptr := ADR (right) ;
   i := 0 ;
   rightmore := FALSE ;
   WHILE i < high DO
      diffbits :=  lptr^ / rptr^ ;  (* / in M2 is xor.  *)
      diffright := diffbits * rptr^ ;  (* * in M2 is and.  *)
      IF diffright # BYTESET {}
      THEN
         rightmore := TRUE
      END ;
      diffleft := diffbits * lptr^ ;
      IF diffleft # BYTESET {}
      THEN
         (* Not a subset, so we early return.  *)
         RETURN FALSE
      END ;
      INC (rptr) ;
      INC (lptr) ;
      INC (i)
   END ;
   lastbit := highbit MOD TBITSIZE (BYTE) ;
   IF lastbit > 0
   THEN
      FOR bit := 0 TO lastbit DO
         IF (NOT (bit IN lptr^)) AND (bit IN rptr^)
         THEN
            rightmore := TRUE
         ELSIF (bit IN lptr^) AND (NOT (bit IN rptr^))
         THEN
            (* Not a subset, so we early return.  *)
            RETURN FALSE
         END
      END
   END ;
   RETURN rightmore
END ProperSubset ;


(*
   ProperSuperset - return TRUE if left is a proper superset of right.
                    If true the left set will have at least one element
                    more than set right.
*)

PROCEDURE ProperSuperset (VAR left, right: ARRAY OF BYTE;
                          highbit: CARDINAL) : BOOLEAN ;
VAR
   diffbits,
   diffleft,
   diffright: BYTESET ;
   leftmore : BOOLEAN ;
   i,
   bit,
   high,
   lastbit  : CARDINAL ;
   lptr,
   rptr     : PtrToByteset ;
BEGIN
   high := HIGH (left) ;
   lptr := ADR (left) ;
   rptr := ADR (right) ;
   i := 0 ;
   leftmore := FALSE ;
   WHILE i < high DO
      diffbits :=  lptr^ / rptr^ ;  (* / in M2 is xor.  *)
      diffleft := diffbits * lptr^ ;  (* * in M2 is and.  *)
      IF diffleft # BYTESET {}
      THEN
         leftmore := TRUE
      END ;
      diffright := diffbits * rptr^ ;
      IF diffright # BYTESET {}
      THEN
         (* Not a superset, so we early return.  *)
         RETURN FALSE
      END ;
      INC (rptr) ;
      INC (lptr) ;
      INC (i)
   END ;
   lastbit := highbit MOD TBITSIZE (BYTE) ;
   IF lastbit > 0
   THEN
      FOR bit := 0 TO lastbit DO
         IF (bit IN lptr^) AND (NOT (bit IN rptr^))
         THEN
            leftmore := TRUE
         ELSIF (NOT (bit IN lptr^)) AND (bit IN rptr^)
         THEN
            (* Not a superset, so we early return.  *)
            RETURN FALSE
         END
      END
   END ;
   RETURN leftmore
END ProperSuperset ;


(*
   LogicalDifference - build a logical difference expression tree.
                       dest := left and (not right).
*)

PROCEDURE LogicalDifference (VAR dest: ARRAY OF BYTE;
                             left, right: ARRAY OF BYTE;
                             highbit: CARDINAL) ;
BEGIN
   Not (right, right, highbit) ;
   And (dest, left, right, highbit)
END LogicalDifference ;


(*
   SymmetricDifference - build a logical difference expression tree.
                         dest := left xor right.
*)

PROCEDURE SymmetricDifference (VAR dest: ARRAY OF BYTE;
                               left, right: ARRAY OF BYTE;
                               highbit: CARDINAL) ;
VAR
   i,
   bit,
   high,
   lastbit: CARDINAL ;
   byteset: BYTESET ;
BEGIN
   high := HIGH (dest) ;
   i := 0 ;
   WHILE i < high DO
      dest[i] := BYTESET (left[i]) / BYTESET (right[i]) ;
      INC (i)
   END ;
   IF i = high
   THEN
      lastbit := highbit MOD TBITSIZE (BYTE) ;
      IF lastbit = 0
      THEN
         dest[i] := BYTESET (left[i]) / BYTESET (right[i])
      ELSE
         byteset := dest[i] ;
         FOR bit := 0 TO lastbit DO
            IF (bit IN BYTESET (left[i])) = (bit IN BYTESET (right[i]))
            THEN
               EXCL (byteset, bit)
            ELSE
               INCL (byteset, bit)
            END
         END ;
         dest[i] := byteset
      END
   ELSE
      HALT
   END
END SymmetricDifference ;


(*
   AssignBits - copy bits [0..highbit] from src to dest.
*)

PROCEDURE AssignBits (VAR dest: BYTESET; src: BYTESET; highbit: CARDINAL) ;
VAR
   bit,
   lastbit: CARDINAL ;
BEGIN
   (* Last byte.  *)
   lastbit := highbit MOD TBITSIZE (BYTE) ;
   IF lastbit = 0
   THEN
      (* Copy all bits.  *)
      dest := src
   ELSE
      (* Copy only required bits.  *)
      FOR bit := 0 TO lastbit DO
         IF bit IN src
         THEN
            INCL (dest, bit)
         ELSE
            EXCL (dest, bit)
         END
      END
   END
END AssignBits ;


(*
   Assign -
*)

PROCEDURE Assign (VAR dest: ARRAY OF BYTE; src: ARRAY OF BYTE; highbit: CARDINAL) ;
VAR
   i, high: CARDINAL ;
BEGIN
   high := HIGH (dest) ;
   i := 0 ;
   WHILE i < high DO
      dest[i] := src[i] ;
      INC (i)
   END ;
   AssignBits (dest[i], src[i], highbit)
END Assign ;


END M2WIDESET.
