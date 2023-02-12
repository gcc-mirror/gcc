(* Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE testsystem ;

FROM SYSTEM IMPORT BITSPERLOC, LOCSPERWORD,
                   LOC, BYTE, WORD, ADDRESS,
                   ADDADR, SUBADR, DIFADR, MAKEADR, ADR, ROTATE,
                   SHIFT, TSIZE ;
FROM libc IMPORT exit ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM FIO IMPORT Close, StdOut ;


PROCEDURE FindFirstElement (start: CARDINAL; s: LargeSet) : CARDINAL ;
BEGIN
   WHILE NOT (start IN s) DO
      INC(start)
   END ;
   RETURN( start )
END FindFirstElement ;


PROCEDURE FindLastElement (start: CARDINAL; s: LargeSet) : CARDINAL ;
BEGIN
   WHILE (start+1<1024) AND ((start+1) IN s) DO
      INC(start)
   END ;
   RETURN( start )
END FindLastElement ;


PROCEDURE debug (s: LargeSet) ;
VAR
   lo, hi: CARDINAL ;
BEGIN
   IF s=LargeSet{}
   THEN
      WriteString('{}')
   ELSE
      WriteString('{') ;
      lo := FindFirstElement(0, s) ;
      hi := FindLastElement(lo, s) ;
      WHILE hi<1024 DO
         IF hi=lo
         THEN
            WriteCard(lo, 0)
         ELSE
            WriteCard(lo, 0) ; WriteString('..') ; WriteCard(hi, 0)
         END ;
         lo := FindFirstElement(hi+1, s) ;
         hi := FindLastElement(lo, s) ;
         IF hi<1024
         THEN
            WriteString(', ')
         END
      END ;
      WriteString('}')
   END ;
   WriteLn
END debug ;


(*
 *   purpose of testsystem is to check that all ISO SYSTEM functions are
 *   implemented, compile and run.
 *)

TYPE
   LargeSet = SET OF [0..1023] ;
   SmallSet = SET OF [0..3] ;

VAR
   a1, a2: ADDRESS ;
   b1, b2: BYTE ;
   s1, s2: BITSET ;
   s3, s4: LargeSet ;
   c1, c2: CARDINAL ;
   w     : WORD ;
   l     : LOC ;
   array : ARRAY [0..TSIZE(ADDRESS)-1] OF LOC ;
   s5, s6: SmallSet ;
BEGIN
   a1 := ADR(array) ;
   a2 := ADDADR(a1, TSIZE(LOC)) ;
   IF SUBADR(a2, TSIZE(LOC))#a1
   THEN
      Close(StdOut) ;
      exit(1)
   END ;
   IF DIFADR(a2, a1) # INTEGER (TSIZE (LOC))
   THEN
      Close(StdOut) ;
      exit(2)
   END ;
   a1 := MAKEADR (ADDRESS (0)) ;
   IF a1#NIL
   THEN
      Close(StdOut) ;
      exit(3)
   END ;

(*
#if defined(__x86_64)
   a1 := MAKEADR(BYTE(0ABH), BYTE(0CDH)) ;
   a1 := MAKEADR(BYTE(0FEH), BYTE(0DCH), BYTE(0BAH), BYTE(098H),
                 BYTE(076H), BYTE(054H), BYTE(032H), BYTE(010H)) ;

   a1 := MAKEADR(CARDINAL(123456789), CARDINAL(987654321)) ;
#endif
*)
   s3 := LargeSet{0, 1, 31, 32, 33, 63, 64, 65, 127, 128, 129} ;
   debug(s3) ;
   s4 := SHIFT(s3, -1) ;
   debug(s4) ;
   IF s4#LargeSet{0, 30, 31, 32, 62, 63, 64, 126, 127, 128}
   THEN
      Close(StdOut) ;
      exit(4)
   END ;
   s4 := SHIFT(s4, 1) ;
   debug(s4) ;
   IF s4#LargeSet{1, 31, 32, 33, 63, 64, 65, 127, 128, 129}
   THEN
      Close(StdOut) ;
      exit(5)
   END ;
   s5 := SmallSet{0, 1} ;
   s6 := ROTATE(s5, 1) ;
   IF s6#SmallSet{1, 2}
   THEN
      Close(StdOut) ;
      exit(6)
   END ;
   s5 := SmallSet{0, 1} ;
   s6 := ROTATE(s5, -1) ;
   IF s6#SmallSet{0, 3}
   THEN
      Close(StdOut) ;
      exit(7)
   END ;
   s3 := LargeSet{0, 1, 31, 32, 33, 63, 64, 65, 127, 128, 129} ;
   debug(s3) ;
   s4 := ROTATE(s3, -1) ;
   debug(s4) ;
   IF s4#LargeSet{1023, 0, 30, 31, 32, 62, 63, 64, 126, 127, 128}
   THEN
      Close(StdOut) ;
      exit(8)
   END ;
   s3 := LargeSet{1, 31, 32, 33, 63, 64, 65, 127, 128, 129, 255, 1023} ;
   debug(s3) ;
   s4 := ROTATE(s3, 1) ;
   debug(s4) ;
   IF s4#LargeSet{0, 2, 32, 33, 34, 64, 65, 66, 128, 129, 130, 256}
   THEN
      Close(StdOut) ;
      exit(9)
   END
END testsystem.
(*
 * Local variables:
 *  compile-command: "gm2 -Wcpp -Wiso -c -g -I. testsystem.mod"
 * End:
 *)
