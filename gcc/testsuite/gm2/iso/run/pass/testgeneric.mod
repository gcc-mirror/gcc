(* Copyright (C) 2015 Free Software Foundation, Inc. *)
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
Boston, MA 02110-1301, USA. *)

MODULE testgeneric ;

FROM SYSTEM IMPORT WORD32, ADR ;
FROM libc IMPORT printf, exit ;


VAR
   test: CARDINAL ;
   code: INTEGER ;

PROCEDURE assert (b: BOOLEAN; a: ARRAY OF CHAR) ;
BEGIN
   INC (test) ;
   IF NOT b
   THEN
      printf ("failed test %d which was %a\n", ADR(a)) ;
      code := 1
   END
END assert ;


VAR
   w: WORD32 ;
   c: CARDINAL ;
   i: INTEGER ;
BEGIN
   code := 0 ;
   test := 0 ;
   c := 1 ;
   IF SIZE(w)=SIZE(c)
   THEN
      w := c ;
      i := w ;
      assert (CARDINAL(i) = c, "copying data through WORD32")
   END ;

   w := 1 ;
   i := w ;
   assert (i=1, "assigning const into a WORD32") ;
   
   exit (code)
END testgeneric.
