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
FROM SYSTEM IMPORT SHIFT, BITSPERLOC, TBITSIZE, BYTE ;


CONST
   EarlyFail = TRUE ;
   Verbose = FALSE ;


(*
   assert -
*)

PROCEDURE assert (condition: BOOLEAN) ;
BEGIN
   IF NOT condition
   THEN
      printf ("assert failed\n");
      exit (1)
   END
END assert ;


(*
   SanityCheck -
*)

PROCEDURE SanityCheck ;
BEGIN
   assert (MIN (large) = 0) ;
   assert (MAX (large) = 1023) ;
   assert (TBITSIZE (BYTE) = 8) ;
END SanityCheck ;


TYPE
   large = SET OF [0..1023] ;
VAR
   r   : INTEGER ;
   i   : INTEGER ;
   b, c: large ;
BEGIN
   SanityCheck ;
   r := 0 ;
   b := large{1, 2, 3, 1022} ;
   IF b # large{1, 2, 3, 1022}
   THEN
      printf ("failed to assign a large set with a constant set\n");
      exit (1)
   END ;
   b := SHIFT(b, 1) ;  (* Shift left by 1 bit.  *)
   IF b#large{2, 3, 4, 1023}
   THEN
      printf ("failed (exit 1) as b#large{2, 3, 4, 1023}\n");
      exit(1)
   END ;
   b := large{1, 2, 3, 1023} ;
   b := SHIFT(b, -1) ;    (* Shift right by 1 bit.  *)
   IF b#large{0, 1, 2, 1022}
   THEN
      printf ("failed (exit 2) as b#large{1, 2, 3, 1022}\n");
      exit(2)
   END ;
   b := large{1+SIZE(BITSET)*BITSPERLOC} ;
   b := SHIFT(b, -1) ;
   IF b#large{SIZE(BITSET)*BITSPERLOC}
   THEN
      printf ("failed (exit 3)\n");
      exit(3)
   END ;
   b := SHIFT(b, -1) ;
   IF b#large{SIZE(BITSET)*BITSPERLOC-1}
   THEN
      printf ("failed (exit 4)\n");
      exit(4)
   END ;

   printf ("test left shift on byte boundaries\n");
   FOR i := 0 TO MAX(large) BY 8 DO
      b := large{0} ;
      b := SHIFT(b, i) ;
      c := large{i} ;
      IF b # c
      THEN
         printf("failed shift left in loop on iteration %d, failed to shift bit 0 left by %i bits\n", i, i) ;
         IF EarlyFail
         THEN
            exit (5)
         END ;
         r := 5
      END
   END ;
   IF r = 0
   THEN
      printf ("test left shift on byte boundaries passed\n")
   ELSE
      printf ("test left shift on byte boundaries failed\n")
   END ;

   printf ("test right shift on byte boundaries\n");
   FOR i := 0 TO MAX(large) BY 8 DO
      b := large{i} ;
      b := SHIFT(b, -i) ;
      c := large{0} ;
      IF b = c
      THEN
         IF Verbose
         THEN
            printf ("success shifted large set right by %d bits\n", i)
         END
      ELSE
         printf("failed shift right in loop on iteration %d\n", i) ;
         IF EarlyFail
         THEN
            exit (6)
         END ;
         r := 6
      END
   END ;
   IF r = 0
   THEN
      printf ("test right shift on byte boundaries passed\n")
   ELSE
      printf ("test right shift on byte boundaries failed\n")
   END ;

   printf ("test shift on each bit\n");
   FOR i := 0 TO MAX(large) DO
      b := large{0} ;
      b := SHIFT(b, i) ;
      c := large{i} ;
      IF b = c
      THEN
         IF Verbose
         THEN
            printf ("success shifted large set left by %d bits\n", i)
         END
      ELSE
         printf("failed shift left in loop on iteration %d, failed to shift bit 0 left by %i bits\n", i, i) ;
         IF EarlyFail
         THEN
            exit (5)
         END ;
         r := 5
      END
   END ;
   FOR i := 0 TO MAX(large) DO
      b := large{i} ;
      b := SHIFT(b, -i) ;
      c := large{0} ;
      IF b = c
      THEN
         IF Verbose
         THEN
            printf ("success shifted large set right by %d bits\n", i)
         END
      ELSE
         printf("failed shift right in loop on iteration %d\n", i) ;
         IF EarlyFail
         THEN
            exit (6)
         END ;
         r := 6
      END
   END ;
   printf("all done\n") ;
   exit(r)
END shift4.
