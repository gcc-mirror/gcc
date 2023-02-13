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
MODULE foo ;


TYPE
   proc = PROCEDURE (CHAR) ;

   rec = RECORD
            a, b: CARDINAL ;
         END ;

   colours = (red, blue, yellow) ;

   varrec= RECORD
              CASE type:colours OF

              red : x: CARDINAL |
              blue: y: CHAR

              ELSE
              END
           END ;

VAR
   myp : POINTER TO rec ;
   this: rec ;
   that: RECORD
            c, d: CARDINAL ;
            e   : rec ;
         END ;

   other: varrec ;

   inline: RECORD
             CASE type:colours OF

             red : x: CARDINAL |
             blue: y: CHAR

             ELSE
             END
          END ;
   array: ARRAY [1..10] OF CHAR ;
   large: ARRAY [1..100], [1..200] OF rec ;
   it   : proc ;
   another,
   card   : CARDINAL ;

(*
TYPE

   sub     = [1..10] ;
   bar = CARDINAL ;
VAR
   z      : bar ;
*)
   i, j, k: INTEGER ;
(*
   c      : colours ;
   b      : BOOLEAN ;
*)

(* *)
PROCEDURE dummy (q: CARDINAL) ;
VAR
   t: CARDINAL ;
   a: CHAR ;
BEGIN
   t := 123 ;
   (* another := *) dummy(t) ;
(*   RETURN( t ) *)
END dummy ;


BEGIN
   card := 12 ;
   (* myproc('a', card) ; *)
   (*  *)
   (* another := *) dummy(card);
   i := 100
   (* i := j+k *)
END foo.
