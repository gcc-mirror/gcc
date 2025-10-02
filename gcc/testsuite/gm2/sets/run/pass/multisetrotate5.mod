(* Copyright (C) 2025 Free Software Foundation, Inc. *)
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

MODULE multisetrotate5 ;

FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT ROTATE, WORD, BITSPERLOC ;

TYPE
   multi = SET OF [0..SIZE (WORD) * 2 * BITSPERLOC-1] ;


(*
   dump -
*)

PROCEDURE dump (s: multi) ;
VAR
   bits, i: CARDINAL ;
BEGIN
   bits := SIZE (multi) * BITSPERLOC -1;
   FOR i := 0 TO bits DO
      printf (" %2d", i)
   END ;
   printf ("\n") ;
   FOR i := 0 TO bits DO
      IF i IN s
      THEN
         printf ("  X")
      ELSE
         printf ("   ")
      END
   END ;
   printf ("\n")
END dump ;


VAR
   set : multi ;
   bits: INTEGER ;
BEGIN
   dump (multi {1}) ;
   dump (multi {2}) ;
   set := multi {2} ;
   dump (set) ;
   set := multi {1} ;
   dump (set) ;
   IF ROTATE (set, 1) = multi {2}
   THEN
      exit (0)
   END ;
   set := multi {2} ;
   set := ROTATE (set, 1) ;
   dump (set) ;
   exit (1)
END multisetrotate5.
