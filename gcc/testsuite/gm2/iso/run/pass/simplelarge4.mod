(* Copyright (C) 2014 Free Software Foundation, Inc. *)
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

MODULE simplelarge4 ;

FROM STextIO IMPORT WriteString, WriteLn, WriteChar, ReadToken, SkipLine ;
FROM SWholeIO IMPORT WriteCard, WriteInt ;
FROM WholeStr IMPORT StrToCard, ConvResults ;
FROM SYSTEM IMPORT CARDINAL8 ;
FROM libc IMPORT printf ;

CONST
   BoardX         =              16 ;
   BoardY         =              16 ;
   BoardSize      = BoardX * BoardY ;

TYPE
   Squares = [0..BoardSize-1] ;
   SoS     = SET OF Squares ;
   Colour  = (Blue, Red, Green, White) ;

VAR
   homeBase: SoS ;


PROCEDURE dumpSet (c: Colour) ;
VAR
   n: CARDINAL ;
BEGIN
   printf ("inside dumpSet (%d)\n", ORD(c)) ;
   printf ("      :  0 2 4 6 8 a c e \n") ;
   FOR n := MIN(Squares) TO MAX(Squares) DO
      IF n MOD 16 = 0
      THEN
         printf ("\nrow %2d:  ", n DIV 16)
      END ;
      IF n IN homeBase
      THEN
         printf ("1")
      ELSE
         printf ("0")
      END
   END ;
   printf ("\n")
END dumpSet ;


(*
   assert -
*)

PROCEDURE assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      WriteString('assert failed') ; WriteLn ;
      HALT
   END
END assert ;


BEGIN
   homeBase := SoS {0, 1, 2, 3,
                    16, 17, 18, 19,
                    32, 33, 34,
                    48, 49} ;

   dumpSet(Blue) ;

   homeBase := homeBase + SoS {4, 20, 35, 50, 65, 64} ;
   dumpSet(Blue) ;

   assert (0 IN homeBase)
END simplelarge4.
