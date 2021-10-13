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
MODULE testcse26 ;

FROM NumberIO IMPORT StrToBin, BinToStr ;

CONST
   MaxBitsetSize = 32 ;
   nul           =  0C ;


PROCEDURE PopInt () : INTEGER ;
BEGIN
   RETURN( 1 )
END PopInt ;


PROCEDURE PushInt (i: INTEGER) ;
BEGIN
END PushInt ;

PROCEDURE PushCard (i: CARDINAL) ;
BEGIN
END PushCard ;

PROCEDURE PopCard () : CARDINAL ;
BEGIN
   RETURN( i )
END PopCard ;

PROCEDURE WriteError (a: ARRAY OF CHAR) ;
BEGIN
END WriteError ;


VAR
   i: CARDINAL ;

PROCEDURE Bit ;
VAR
   c       : CARDINAL ;
   Op1, Op2: CARDINAL ;
BEGIN
   (* in correct bitset range *)
   c := 0 ;
   FOR i := Op1 TO Op2 DO
      PushCard(i) ;
      Bit ;
      INC(c, PopCard())
   END ;
   INC(c) ;
   Op1 := c ;
   i := i + c ;
END Bit ;


BEGIN
   Bit
END testcse26.
