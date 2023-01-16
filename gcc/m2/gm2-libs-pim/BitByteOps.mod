(* BitByteOps.mod provides a Logitech-3.0 compatible library.

Copyright (C) 2007-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE BitByteOps ;

FROM SYSTEM IMPORT BYTE, ADR, SHIFT, ROTATE, TSIZE, BITSET8, CARDINAL8 ;


(*
   GetBits - returns the bits firstBit..lastBit from source.
             Bit 0 of byte maps onto the firstBit of source.
*)

PROCEDURE GetBits (source: BYTE; firstBit, lastBit: CARDINAL) : BYTE ;
VAR
   si  : CARDINAL8 ;
   sb  : BITSET8 ;
   mask: BITSET8 ;
   i   : CARDINAL ;
BEGIN
   sb := VAL (BITSET8, source) ;
   mask := BITSET8 {} ;
   FOR i := firstBit TO lastBit DO
      INCL (mask, i)
   END ;
   sb := VAL (BITSET8, source) * mask ;
   i := 1 ;
   WHILE firstBit > 0 DO
      DEC (firstBit) ;
      i := i*2
   END ;
   si := VAL (CARDINAL8, sb) ;
   RETURN VAL (BYTE, si DIV VAL (CARDINAL8, i))
END GetBits ;


(*
   SetBits - sets bits in, byte, starting at, firstBit, and ending at,
             lastBit, with, pattern.  The bit zero of, pattern, will
             be placed into, byte, at position, firstBit.
*)

PROCEDURE SetBits (VAR byte: BYTE; firstBit, lastBit: CARDINAL;
                   pattern: BYTE) ;
VAR
   pb, pp: BITSET8 ;
   i, j  : CARDINAL ;
BEGIN
   pb := VAL (BITSET8, byte) ;
   pp := VAL (BITSET8, pattern) ;
   j := 0 ;
   FOR i := firstBit TO lastBit DO
      IF j IN pp
      THEN
         INCL (pb, i)
      ELSE
         EXCL (pb, i)
      END ;
      INC (j)
   END ;
   byte := VAL (BYTE, pb)
END SetBits ;


(*
   ByteAnd - returns a bitwise (left AND right)
*)

PROCEDURE ByteAnd (left, right: BYTE) : BYTE ;
BEGIN
   RETURN VAL (BYTE, VAL (BITSET8, left) * VAL (BITSET8, right))
END ByteAnd ;


(*
   ByteOr - returns a bitwise (left OR right)
*)

PROCEDURE ByteOr (left, right: BYTE) : BYTE ;
BEGIN
   RETURN VAL (BYTE, VAL (BITSET8, left) + VAL (BITSET8, right))
END ByteOr ;


(*
   ByteXor - returns a bitwise (left XOR right)
*)

PROCEDURE ByteXor (left, right: BYTE) : BYTE ;
BEGIN
   RETURN VAL (BYTE, VAL (BITSET8, left) DIV VAL (BITSET8, right))
END ByteXor ;


(*
   ByteNot - returns a byte with all bits inverted.
*)

PROCEDURE ByteNot (byte: BYTE) : BYTE ;
BEGIN
   RETURN VAL (BYTE, -VAL (BITSET8, byte))
END ByteNot ;


(*
   ByteShr - returns a, byte, which has been shifted, count
             bits to the right.
*)

PROCEDURE ByteShr (byte: BYTE; count: CARDINAL) : BYTE ;
BEGIN
   RETURN VAL (BYTE, SHIFT (VAL (BITSET8, byte), count))
END ByteShr ;


(*
   ByteShl - returns a, byte, which has been shifted, count
             bits to the left.
*)

PROCEDURE ByteShl (byte: BYTE; count: CARDINAL) : BYTE ;
BEGIN
   RETURN VAL (BYTE, SHIFT (VAL (BITSET8, byte), -VAL (INTEGER, count)))
END ByteShl ;


(*
   ByteSar - shift byte arthemetic right.  Preserves the top
             end bit as the value is shifted right.
*)

PROCEDURE ByteSar (byte: BYTE; count: CARDINAL) : BYTE ;
VAR
   b: BYTE ;
BEGIN
   IF MAX(BITSET8) IN VAL(BITSET8, byte)
   THEN
      b := VAL (BYTE, SHIFT (VAL (BITSET8, byte), count) + BITSET8 {MAX (BITSET8)}) ;
      RETURN b
   ELSE
      RETURN VAL (BYTE, SHIFT (VAL (BITSET8, byte), count))
   END
END ByteSar ;


(*
   ByteRor - returns a, byte, which has been rotated, count
             bits to the right.
*)

PROCEDURE ByteRor (byte: BYTE; count: CARDINAL) : BYTE ;
BEGIN
   RETURN VAL (BYTE, ROTATE (VAL (BITSET8, byte), count))
END ByteRor ;


(*
   ByteRol - returns a, byte, which has been rotated, count
             bits to the left.
*)

PROCEDURE ByteRol (byte: BYTE; count: CARDINAL) : BYTE ;
BEGIN
   RETURN VAL (BYTE, ROTATE (VAL (BITSET8, byte), -VAL (INTEGER, count)))
END ByteRol ;


(*
   HighHibble - returns the top nibble only from, byte,
                in the lowest nibble position.
*)

PROCEDURE HighNibble (byte: BYTE) : BYTE ;
BEGIN
   RETURN VAL (BYTE, VAL (CARDINAL8, byte) DIV 16)
END HighNibble ;


(*
   LowNibble - returns the low nibble only from, byte.
               The top nibble is replaced by zeros.
*)

PROCEDURE LowNibble (byte: BYTE) : BYTE ;
BEGIN
   RETURN VAL (BYTE, VAL (BITSET8, byte) * BITSET8 {0..3})
END LowNibble ;


(*
   Swap - swaps the low and high nibbles in the, byte.
*)

PROCEDURE Swap (byte: BYTE) : BYTE ;
BEGIN
   RETURN VAL(BYTE,
              VAL(BITSET8, VAL (CARDINAL8,
                                VAL (BITSET8, byte) *
                                BITSET8 {4..7}) DIV 16) +
              VAL(BITSET8, byte) * BITSET8 {0..3})
END Swap ;


END BitByteOps.
