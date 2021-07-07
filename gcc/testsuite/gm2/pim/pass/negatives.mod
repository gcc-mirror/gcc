(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE negatives ;   (*!m2pim*)

CONST
   MaxDigits = 50 ;
   nul = 0C ;


PROCEDURE CardToStr (x, n: CARDINAL ; VAR a: ARRAY OF CHAR) ;
VAR
   i, j,
   Higha : CARDINAL ;
   buf   : ARRAY [1..MaxDigits] OF CARDINAL ;
BEGIN
   i := 0 ;
   REPEAT
      INC(i) ;
      IF i>MaxDigits
      THEN
         HALT
      END ;
      buf[i] := x MOD 10 ;
      x := x DIV 10 ;
   UNTIL x=0 ;
   j := 0 ;
   Higha := HIGH(a) ;
   WHILE (n>i) AND (j<=Higha) DO
      a[j] := ' ' ;
      INC(j) ;
      DEC(n)
   END ;
   WHILE (i>0) AND (j<=Higha) DO
      a[j] := CHR( buf[i] + ORD('0') ) ;
      INC(j) ;
      DEC(i)
   END ;
   IF j<=Higha
   THEN
      a[j] := nul
   END
END CardToStr ;


END negatives.
