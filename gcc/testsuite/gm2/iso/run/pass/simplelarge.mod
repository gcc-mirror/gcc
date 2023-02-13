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

MODULE simplelarge ;

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
   homeBase: ARRAY [MIN(Colour)..MAX(Colour)] OF SoS ;


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
      IF n IN homeBase[c]
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
   homeBase[Red] := SoS {} ;
   dumpSet(Red) ;
   homeBase[Blue] := SoS {0, 1, 2, 3,
                          16, 17, 18, 19,
                          32, 33, 34,
                          48, 49} ;

   dumpSet(Blue) ;

   assert (0 IN homeBase[Blue]) ;
   assert (1 IN homeBase[Blue]) ;
   assert (2 IN homeBase[Blue]) ;
   assert (3 IN homeBase[Blue]) ;

(*
   homeBase[Red] := SoS {255-0, 255-1, 255-2, 255-3,
                         255-16, 255-17, 255-18, 255-19,
                         255-32, 255-33, 255-34,
                         255-48, 255-49} ;
*)

   homeBase[Blue] := homeBase[Blue] + SoS {4, 20, 35, 50, 65, 64} ;
   dumpSet(Blue) ;
   dumpSet(Red) ;
   assert (homeBase[Red] = SoS {}) ;

(*
   homeBase[Red] := homeBase[Red] + SoS {255-4, 255-20, 255-35, 255-50, 255-65, 255-64} ;
*)

   assert (0 IN homeBase[Blue]) ;
   assert (1 IN homeBase[Blue]) ;
   assert (2 IN homeBase[Blue]) ;
   assert (3 IN homeBase[Blue]) ;
   assert (4 IN homeBase[Blue]) ;
   assert (NOT (5 IN homeBase[Blue])) ;
   assert (NOT (6 IN homeBase[Blue])) ;

   assert (homeBase[Red] = SoS {})
END simplelarge.
(*
 * Local variables:
 *  compile-command: "gm2 -g -fiso simplelarge.mod"
 * End:
 *)
