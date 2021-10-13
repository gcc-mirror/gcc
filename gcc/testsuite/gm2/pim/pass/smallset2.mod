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

MODULE smallset2 ;

FROM libc IMPORT exit ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;


PROCEDURE TestIn (i: CARDINAL) ;
VAR
   j: CARDINAL ;
BEGIN
   IF NOT (i IN b)
   THEN
      exit(3)
   END ;
   FOR j := 0 TO MAX(BITSET) DO
      IF (i#j) AND (j IN b)
      THEN
         exit(4)
      END
   END
END TestIn ;


VAR
   b: BITSET ;
   i, j: CARDINAL ;
BEGIN
   j := 1 ;
   b := {} ;
   FOR i := 0 TO MAX(BITSET) DO
      WriteString('index = ') ; WriteCard(i, 2) ; WriteLn ;
      INCL(b, i) ;
      IF VAL(CARDINAL, b)#j
      THEN
         exit(1)
      END ;
      TestIn(i) ;
      EXCL(b, i) ;
      IF b#{}
      THEN
         exit(2)
      END ;
      j := j*2
   END
END smallset2.
