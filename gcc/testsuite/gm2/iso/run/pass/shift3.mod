(* Copyright (C) 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE shift3 ;

FROM libc IMPORT exit ;
FROM SYSTEM IMPORT SHIFT ;

PROCEDURE Check (a, b: large) ;
BEGIN
   IF a#b
   THEN
      exit(1)
   END
END Check ;


PROCEDURE DoIt (s: large; v: INTEGER; r: large) ;
BEGIN
   s := SHIFT(s, v) ;
   IF s#r
   THEN
      exit(2)
   END
END DoIt ;


TYPE
   large = SET OF [0..1023] ;
VAR
   b: large ;
   i: INTEGER ;
BEGIN
   b := large{1, 2, 3} ;
   b := SHIFT(b, 1) ;
   Check(b, large{2, 3, 4}) ;
   b := large{1, 2, 3} ;
   b := SHIFT(b, -1) ;
   Check(b, large{0, 1, 2}) ;
   i := 1 ;
   b := large{1, 2, 3} ;
   DoIt(b, i, large{2, 3, 4}) ;
   i := -1 ;
   b := large{3, 4, 5} ;
   DoIt(b, i, large{2, 3, 4})
END shift3.
(*
 * Local variables:
 *  compile-command: "gm2 -Wiso -c -g -I. shift3.mod"
 * End:
 *)
