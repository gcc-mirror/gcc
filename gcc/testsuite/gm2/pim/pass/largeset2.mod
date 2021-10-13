(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE largeset2 ;

FROM SYSTEM IMPORT ADR, ADDRESS ;
FROM libc IMPORT exit ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;

TYPE
   LargeBitset = SET OF [0..127] ;


PROCEDURE TestIn (i: CARDINAL) ;
VAR
   j: CARDINAL ;
BEGIN
   IF NOT (i IN b)
   THEN
      exit(3)
   END ;
   FOR j := 0 TO MAX(LargeBitset) DO
      IF (i#j) AND (j IN b)
      THEN
         exit(4)
      END
   END
END TestIn ;


(*
   GetAddr - 
*)

PROCEDURE GetAddr (i: CARDINAL) : ADDRESS ;
BEGIN
   CASE i OF

   0..31  : RETURN( ADR(b) ) |
   32..63 : RETURN( ADR(b)+ADDRESS(4) ) |
   64..95 : RETURN( ADR(b)+ADDRESS(8) ) |
   96..127: RETURN( ADR(b)+ADDRESS(12) )

   ELSE
      HALT
   END
END GetAddr ;


VAR
   b: LargeBitset ;
   p: POINTER TO CARDINAL ;
   i, j: CARDINAL ;
BEGIN
   j := 1 ;
   b := LargeBitset{} ;
   FOR i := 0 TO MAX(LargeBitset) DO
      WriteString('index = ') ; WriteCard(i, 3) ; WriteLn ;
      INCL(b, i) ;
      p := GetAddr(i) ;
      IF p^#j
      THEN
         exit(1)
      END ;
      TestIn(i) ;
      EXCL(b, i) ;
      IF b#LargeBitset{}
      THEN
         exit(2)
      END ;
      IF i MOD 32 = 31
      THEN
         j := 1
      ELSE
         j := j*2
      END
   END
END largeset2.
