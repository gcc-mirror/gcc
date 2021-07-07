(* Copyright (C) 2010 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with gm2; see the file COPYING.  If not, write to the Free
Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301, USA. *)

MODULE coroutine ;

FROM SYSTEM IMPORT ADDRESS ;
FROM COROUTINES IMPORT COROUTINE, NEWCOROUTINE, PROTECTION, TRANSFER ;
FROM Storage IMPORT ALLOCATE ;
FROM libc IMPORT printf ;

CONST
   Workspace = 32 * 1024 * 1024 ;


PROCEDURE first ;
VAR
   x: CARDINAL ;
BEGIN
   x := 0 ;
   LOOP
      printf ("c1 is alive and well\n") ;
      IF x=1000
      THEN
         printf ("finished!\n") ;
         TRANSFER (c1, main)
      ELSE
         TRANSFER (c1, c2)
      END ;
      INC (x)
   END
END first ;


PROCEDURE second ;
BEGIN
   LOOP
      printf ("c2 is alive and well\n") ;
      TRANSFER (c2, c1)
   END
END second ;


VAR
   main,
   c1, c2: COROUTINE ;
   w1, w2: ADDRESS ;
BEGIN
   ALLOCATE (w1, Workspace) ;
   NEWCOROUTINE (first, w1, Workspace, c1) ;
   ALLOCATE (w2, Workspace) ;
   NEWCOROUTINE (second, w2, Workspace, c2) ;
   printf ("first context switch to c1\n") ;
   TRANSFER (main, c1) ;
   printf ("back to main and all done\n\n")
END coroutine.
