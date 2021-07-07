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

MODULE getconst ;

CONST
   LongReal = 1 ;
   Integer  = 2 ;
   Char     = 3 ;


PROCEDURE GetSymName (s: CARDINAL) : CARDINAL ;
BEGIN
   RETURN s
END GetSymName ;

PROCEDURE LengthKey (s: CARDINAL) : CARDINAL ;
BEGIN
   RETURN s
END LengthKey ;

PROCEDURE GetKey (s: CARDINAL; a: ARRAY OF CHAR) ;
BEGIN
END GetKey ;


(*
   GetConstLitType - returns the type of the constant, Sym.
                     All constants have type NulSym except CHAR constants
                     ie 00C 012C etc and floating point constants which have type LONGREAL.
*)

PROCEDURE GetConstLitType (Sym: CARDINAL) : CARDINAL ;
CONST
   Max = 4096 ;
VAR
   a   : ARRAY [0..Max] OF CHAR ;
   i,
   High: CARDINAL ;
BEGIN
   GetKey(GetSymName(Sym), a) ;
   High := LengthKey(GetSymName(Sym)) ;
   IF a[High-1]='C'
   THEN
      RETURN( Char )
   ELSE
      i := 0 ;
      WHILE i<High DO
         IF (a[i]='.') OR (a[i]='+')
         THEN
            RETURN( LongReal )
         ELSE
            INC(i)
         END
      END ;
      RETURN( Integer )
   END
END GetConstLitType ;


BEGIN
   IF GetConstLitType(2)=2
   THEN
   END
END getconst.
