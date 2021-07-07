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

MODULE bbits ;

FROM BitBlockOps IMPORT BlockAnd, BlockOr, BlockXor, BlockNot, BlockShr ;
FROM SYSTEM IMPORT ADDRESS, BYTE, WORD, SIZE, ADR, SHIFT ;
FROM libc IMPORT exit, printf ;
FROM StrLib IMPORT StrEqual ;


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
   byte: BYTE ;
BEGIN
   byte := VAL(BYTE, BITSET{0, 3, 5, 7}) ;
   BlockShr(ADR(byte), SIZE(byte), 1) ;
   Assert(byte=VAL(BYTE, BITSET{2, 4, 6}), __FILE__, __LINE__, 'BlockShr on BYTE') ;
   BlockXor(ADR(byte), ADR(byte), SIZE(byte)) ;
   Assert(byte=BYTE(0), __FILE__, __LINE__, 'BlockXor on BYTE')   
END byteTest ;


PROCEDURE bitsetTest ;
VAR
   bytes : POINTER TO ARRAY [0..SIZE(BITSET)-1] OF BYTE ;
   bitset: BITSET ;
BEGIN
   bytes := ADR(bitset) ;
   bytes^[0] := BYTE(0A3H) ;
   bytes^[1] := BYTE(053H) ;
   bytes^[2] := BYTE(031H) ;
   bytes^[3] := BYTE(01AH) ;

   BlockShr(ADR(bitset), SIZE(bitset), 1) ;

   Assert(bytes^[0]=BYTE(051H), __FILE__, __LINE__, 'BlockShr on byte[0]') ;
   Assert(bytes^[1]=BYTE(0A9H), __FILE__, __LINE__, 'BlockShr on byte[1]') ;
   Assert(bytes^[2]=BYTE(098H), __FILE__, __LINE__, 'BlockShr on byte[2]') ;
   Assert(bytes^[3]=BYTE(08DH), __FILE__, __LINE__, 'BlockShr on byte[3]') ;
   BlockXor(ADR(bitset), ADR(bitset), SIZE(bitset)) ;
   Assert(bitset=BITSET(0), __FILE__, __LINE__, 'BlockXor on BITSET')
END bitsetTest ;


VAR
   res: INTEGER ;
BEGIN
   res := 0 ;
   byteTest ;
   bitsetTest ;
   exit(res)
END bbits.
