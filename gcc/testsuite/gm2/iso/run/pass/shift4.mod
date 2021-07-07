(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

MODULE shift4 ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT SHIFT, BITSPERLOC ;

TYPE
   large = SET OF [0..1023] ;
VAR
   r   : INTEGER ;
   i   : INTEGER ;
   b, c: large ;
BEGIN
   r := 0 ;
   b := large{1, 2, 3, 1022} ;
   b := SHIFT(b, 1) ;
   IF b#large{2, 3, 4, 1023}
   THEN
      exit(1)
   END ;
   b := large{1, 2, 3, 1023} ;
   b := SHIFT(b, -1) ;
   IF b#large{0, 1, 2, 1022}
   THEN
      exit(2)
   END ;
   b := large{1+SIZE(BITSET)*BITSPERLOC} ;
   b := SHIFT(b, -1) ;
   IF b#large{SIZE(BITSET)*BITSPERLOC}
   THEN
      exit(3)
   END ;
   b := SHIFT(b, -1) ;
   IF b#large{SIZE(BITSET)*BITSPERLOC-1}
   THEN
      exit(4)
   END ;
   FOR i := 0 TO MAX(large) DO
      b := large{0} ;
      b := SHIFT(b, i) ;
      c := large{i} ;
      IF b#c
      THEN
         printf("failed shift left in loop on iteration %d\n", i) ;
         r := 5
      END
   END ;
   FOR i := 0 TO MAX(large) DO
      b := large{i} ;
      b := SHIFT(b, -i) ;
      c := large{0} ;
      IF b#c
      THEN
         printf("failed shift right in loop on iteration %d\n", i) ;
         r := 6
      END
   END ;
   printf("all done\n") ;
   exit(r)
END shift4.
