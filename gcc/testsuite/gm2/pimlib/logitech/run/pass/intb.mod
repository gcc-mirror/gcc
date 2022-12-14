(* Copyright (C) 2005 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE intb ;

FROM BitBlockOps IMPORT BlockAnd, BlockOr, BlockXor, BlockNot, BlockShr ;
FROM SYSTEM IMPORT ADDRESS, BYTE, WORD, SIZE, ADR, SHIFT ;
FROM libc IMPORT exit, printf ;
FROM StrLib IMPORT StrEqual ;

TYPE
   BYTESET = PACKEDSET OF [0..7] ;


PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF v
   THEN
      r := printf("successfully evaluated assertion (%s)\n", ADR(e))
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;


PROCEDURE clear (a: ADDRESS; n: CARDINAL) ;
VAR
   p: POINTER TO BYTE ;
BEGIN
   p := a ;
   WHILE n>0 DO
      p^ := BYTE(0) ;
      INC(p) ;
      DEC(n)
   END
END clear ;


PROCEDURE byteTest ;
VAR
   byte, lo: BYTE ;
BEGIN
   byte := VAL(BYTE, BYTESET {0, 3, 5, 7}) ;
   lo := VAL(BYTE, SHIFT(VAL(BYTESET, byte), -1)) ;
   Assert(lo=VAL(BYTE, BYTESET {2, 4, 6}), __FILE__, __LINE__, 'SHIFT on BYTE') ;
   lo := VAL(BYTE, SHIFT(VAL(BYTESET, lo), 1)) ;
   Assert(lo=VAL(BYTE, BYTESET {3, 5, 7}), __FILE__, __LINE__, 'SHIFT on BYTE') ;
END byteTest ;


PROCEDURE bitsetTest ;
VAR
   bitset, lo: BITSET ;
BEGIN
   bitset := BITSET{0, 3, 5, 7, 30, 31} ;
   lo := SHIFT(bitset, -1) ;
   Assert(lo=BITSET{2, 4, 6, 29, 30}, __FILE__, __LINE__, 'SHIFT on BITSET') ;
   lo := SHIFT(lo, 1) ;
   Assert(lo=BITSET{3, 5, 7, 30, 31}, __FILE__, __LINE__, 'SHIFT on BITSET') ;
END bitsetTest ;


VAR
   res: INTEGER ;
BEGIN
   res := 0 ;
   Assert (SIZE (BYTESET) = 1, __FILE__, __LINE__, 'set size should be a byte') ;
   byteTest ;
   bitsetTest ;
   exit(res)
END intb.
