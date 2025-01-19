(* BitBlockOps.mod provides a Logitech compatible module.

Copyright (C) 2007-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE BitBlockOps ;


FROM SYSTEM IMPORT BITSPERBYTE, SHIFT, BYTE, BITSET8, TSIZE ;
FROM Builtins IMPORT memmove, memset ;

TYPE
   ptrToByte = POINTER TO BITSET8 ;
   ptrToBitset = POINTER TO BITSET ;


(*
   BlockAnd - performs a bitwise AND on blocks
              [dest..dest+size-1] := [dest..dest+size-1] AND
                                     [src..src+size-1]
*)

PROCEDURE BlockAnd (dest, src: ADDRESS; size: CARDINAL) ;
VAR
   bitsetDest, bitsetSrc: ptrToBitset ;
   byteDest, byteSrc    : ptrToByte ;
BEGIN
   bitsetDest := dest ;
   bitsetSrc  := src ;
   WHILE size > TSIZE (BITSET) DO
      bitsetDest^ := bitsetDest^ * bitsetSrc^ ;
      INC (bitsetDest, TSIZE (BITSET)) ;
      INC (bitsetSrc, TSIZE (BITSET)) ;
      DEC (size, TSIZE (BITSET))
   END ;
   byteDest := VAL (ptrToByte, bitsetDest) ;
   byteSrc  := VAL (ptrToByte, bitsetSrc) ;
   WHILE size > 0 DO
      byteDest^ := byteDest^ * byteSrc^ ;
      INC(byteDest) ;
      INC(byteSrc) ;
      DEC(size)
   END
END BlockAnd ;


(*
   BlockOr - performs a bitwise OR on blocks
             [dest..dest+size-1] := [dest..dest+size-1] OR
                                    [src..src+size-1]
*)

PROCEDURE BlockOr (dest, src: ADDRESS; size: CARDINAL) ;
VAR
   bitsetDest, bitsetSrc: ptrToBitset ;
   byteDest, byteSrc    : ptrToByte ;
BEGIN
   bitsetDest := dest ;
   bitsetSrc  := src ;
   WHILE size > TSIZE (BITSET) DO
      bitsetDest^ := bitsetDest^ + bitsetSrc^ ;
      INC (bitsetDest, TSIZE (BITSET)) ;
      INC (bitsetSrc, TSIZE (BITSET)) ;
      DEC (size, TSIZE (BITSET))
   END ;
   byteDest := VAL (ptrToByte, bitsetDest) ;
   byteSrc  := VAL (ptrToByte, bitsetSrc) ;
   WHILE size > 0 DO
      byteDest^ := byteDest^ + byteSrc^ ;
      INC (byteDest) ;
      INC (byteSrc) ;
      DEC (size)
   END
END BlockOr ;


(*
   BlockXor - performs a bitwise XOR on blocks
              [dest..dest+size-1] := [dest..dest+size-1] XOR
                                     [src..src+size-1]
*)

PROCEDURE BlockXor (dest, src: ADDRESS; size: CARDINAL) ;
VAR
   bitsetDest, bitsetSrc: ptrToBitset ;
   byteDest, byteSrc    : ptrToByte ;
BEGIN
   bitsetDest := dest ;
   bitsetSrc  := src ;
   WHILE size>TSIZE(BITSET) DO
      bitsetDest^ := bitsetDest^ - bitsetSrc^ ;
      INC(bitsetDest, TSIZE(BITSET)) ;
      INC(bitsetSrc, TSIZE(BITSET)) ;
      DEC(size, TSIZE(BITSET))
   END ;
   byteDest := VAL(ptrToByte, bitsetDest) ;
   byteSrc  := VAL(ptrToByte, bitsetSrc) ;
   WHILE size>0 DO
      byteDest^ := byteDest^ - byteSrc^ ;
      INC(byteDest) ;
      INC(byteSrc) ;
      DEC(size)
   END
END BlockXor ;


(*
   BlockNot - performs a bitsize NOT on the block as defined
              by:  [dest..dest+size-1]
*)

PROCEDURE BlockNot (dest: ADDRESS; size: CARDINAL) ;
VAR
   bitsetDest: ptrToBitset ;
   byteDest  : ptrToByte ;
BEGIN
   bitsetDest := dest ;
   WHILE size>TSIZE(BITSET) DO
      bitsetDest^ := -bitsetDest^ ;
      INC(bitsetDest, TSIZE(BITSET)) ;
      DEC(size, TSIZE(BITSET))
   END ;
   byteDest := VAL(ptrToByte, bitsetDest) ;
   WHILE size>0 DO
      byteDest^ := - byteDest^ ;
      INC (byteDest) ;
      DEC (size)
   END
END BlockNot ;


(*
   BlockShr - performs a block shift right of, count, bits.
              Where the block is defined as:
              [dest..dest+size-1].
              The block is considered to be an ARRAY OF BYTEs
              which is shifted, bit at a time over each byte in
              turn.  The left most byte is considered the byte
              located at the lowest address.
              If you require an endianness SHIFT use
              the SYSTEM.SHIFT procedure and declare the
              block as a POINTER TO set type.
*)

PROCEDURE BlockShr (dest: ADDRESS; size, count: CARDINAL) ;
VAR
   p         : ptrToByte ;
   carry,
   hi, lo    : BITSET8 ;
   byteOffset,
   bitOffset : CARDINAL ;
BEGIN
   byteOffset := count DIV BITSPERBYTE ;
   IF byteOffset >= size
   THEN
      (* shifted all data out, nothing left *)
      p := dest ;
      dest := memset (p, 0, size)
   ELSE
      bitOffset := count MOD BITSPERBYTE ;
      IF byteOffset > 0
      THEN
         (* move whole bytes using memmove *)
         p := dest ;
         dest := memmove (dest, dest+VAL(ADDRESS, byteOffset), size-byteOffset) ;
         (* zero leading bytes *)
         dest := memset (p, 0, byteOffset)
      END ;
      IF bitOffset > 0
      THEN
         (* some real shifting is necessary *)
         p := dest + VAL (ADDRESS, byteOffset) ;
         hi := BITSET8 {} ;
         WHILE size>byteOffset DO
            lo := SHIFT (p^, -bitOffset) ;
            carry := SHIFT (p^, BITSPERBYTE - bitOffset) ;
            p^ := lo + hi ;
            INC (p) ;
            hi := carry ;
            DEC (size)
         END
      END
   END
END BlockShr ;


(*
   BlockShl - performs a block shift left of, count, bits.
              Where the block is defined as:
              [dest..dest+size-1].
              The block is considered to be an ARRAY OF BYTEs
              which is shifted, bit at a time over each byte in
              turn.  The left most byte is considered the byte
              located at the lowest address.
              If you require an endianness SHIFT use
              the SYSTEM.SHIFT procedure and declare the
              block as a POINTER TO set type.
*)

PROCEDURE BlockShl (dest: ADDRESS; size, count: CARDINAL) ;
VAR
   p         : ptrToByte ;
   carry,
   hi, lo    : BITSET8 ;
   byteOffset,
   bitOffset : CARDINAL ;
BEGIN
   byteOffset := count DIV BITSPERBYTE ;
   IF byteOffset>=size
   THEN
      (* shifted all data out, nothing left *)
      p := dest ;
      dest := memset(p, 0, size)
   ELSE
      bitOffset := count MOD BITSPERBYTE ;
      IF byteOffset>0
      THEN
         (* move whole bytes using memmove *)
         p := dest ;
         dest := memmove (dest, dest + VAL (ADDRESS, byteOffset), size - byteOffset) ;
         (* zero leading bytes *)
         dest := memset (p, 0, byteOffset)
      END ;
      IF bitOffset>0
      THEN
         (* some real shifting is necessary *)
         p := dest + VAL (ADDRESS, byteOffset) ;
         hi := BITSET8 {} ;
         WHILE size > byteOffset DO
            lo := SHIFT (p^, -bitOffset) ;
            carry := SHIFT (p^, (BITSPERBYTE - bitOffset)) ;
            p^ := lo + hi ;
            INC (p) ;
            hi := carry ;
            DEC (size)
         END
      END
   END
END BlockShl ;


(*
   BlockRor - performs a block rotate right of, count, bits.
              Where the block is defined as:
              [dest..dest+size-1].
              The block is considered to be an ARRAY OF BYTEs
              which is shifted, bit at a time over each byte in
              turn.  The left most byte is considered the byte
              located at the lowest address.
              If you require an endianness ROTATE use
              the SYSTEM.ROTATE procedure and declare the
              block as a POINTER TO set type.
*)

PROCEDURE BlockRor (dest: ADDRESS; size, count: CARDINAL) ;
BEGIN
   (* not yet implemented *)
   HALT
END BlockRor ;


(*
   BlockRol - performs a block rotate left of, count, bits.
              Where the block is defined as:
              [dest..dest+size-1].
              The block is considered to be an ARRAY OF BYTEs
              which is shifted, bit at a time over each byte in
              turn.  The left most byte is considered the byte
              located at the lowest address.
              If you require an endianness ROTATE use
              the SYSTEM.ROTATE procedure and declare the
              block as a POINTER TO set type.
*)

PROCEDURE BlockRol (dest: ADDRESS; size, count: CARDINAL) ;
BEGIN
   (* not yet implemented *)
   HALT
END BlockRol ;


END BitBlockOps.
