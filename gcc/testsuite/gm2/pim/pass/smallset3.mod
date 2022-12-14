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

MODULE smallset3 ;

FROM libc IMPORT exit ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;


VAR
   b: BITSET ;
   j: CARDINAL ;
BEGIN
   j := 1 ;
   b := {} ;
   WriteString('index = ') ; WriteCard(1, 2) ; WriteLn ;
   INCL(b, 1) ;
   IF VAL(CARDINAL, b)#2
   THEN
      exit(1)
   END ;
   IF NOT (1 IN b)
   THEN
      exit(3)
   END ;
   IF 1 IN b
   THEN
      INC(j)
   ELSE
      exit(4)
   END ;

   EXCL(b, 1) ;
   IF b#{}
   THEN
      exit(2)
   END ;

   j := 20 ;
   b := {} ;
   WriteString('index = ') ; WriteCard(20, 2) ; WriteLn ;
   INCL(b, 20) ;
   IF VAL(CARDINAL, b)#1048576
   THEN
      exit(1)
   END ;
   IF NOT (20 IN b)
   THEN
      exit(3)
   END ;
   IF 20 IN b
   THEN
      INC(j)
   ELSE
      exit(4)
   END ;

   EXCL(b, 20) ;
   IF b#{}
   THEN
      exit(2)
   END

END smallset3.
