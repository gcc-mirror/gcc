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

MODULE largeset3 ;

FROM libc IMPORT exit ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteString, WriteLn ;

TYPE
   LargeBitset = SET OF [0..127] ;


VAR
   b: LargeBitset ;
   j: CARDINAL ;
BEGIN
   j := 1 ;
   b := LargeBitset{} ;
   WriteString('index = ') ; WriteCard(1, 2) ; WriteLn ;
   INCL(b, 1) ;
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
   IF b#LargeBitset{}
   THEN
      exit(2)
   END ;

   j := 20 ;
   b := LargeBitset{} ;
   WriteString('index = ') ; WriteCard(20, 2) ; WriteLn ;
   INCL(b, 20) ;
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
   IF b#LargeBitset{}
   THEN
      exit(2)
   END ;

   j := 40 ;
   b := LargeBitset{} ;
   WriteString('index = ') ; WriteCard(40, 2) ; WriteLn ;
   INCL(b, 40) ;
   IF NOT (40 IN b)
   THEN
      exit(3)
   END ;
   IF 40 IN b
   THEN
      INC(j)
   ELSE
      exit(4)
   END ;

   EXCL(b, 40) ;
   IF b#LargeBitset{}
   THEN
      exit(2)
   END ;

   j := 60 ;
   b := LargeBitset{} ;
   WriteString('index = ') ; WriteCard(60, 2) ; WriteLn ;
   INCL(b, 60) ;
   IF NOT (60 IN b)
   THEN
      exit(3)
   END ;
   IF 60 IN b
   THEN
      INC(j)
   ELSE
      exit(4)
   END ;

   EXCL(b, 60) ;
   IF b#LargeBitset{}
   THEN
      exit(2)
   END ;

   j := 100 ;
   b := LargeBitset{} ;
   WriteString('index = ') ; WriteCard(100, 3) ; WriteLn ;
   INCL(b, 100) ;
   IF NOT (100 IN b)
   THEN
      exit(3)
   END ;
   IF 100 IN b
   THEN
      INC(j)
   ELSE
      exit(4)
   END ;

   EXCL(b, 100) ;
   IF b#LargeBitset{}
   THEN
      exit(2)
   END

END largeset3.
