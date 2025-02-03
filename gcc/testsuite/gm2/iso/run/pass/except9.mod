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

MODULE except9 ;

FROM libc IMPORT printf ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT ADR, WORD ;


PROCEDURE fly (debug: BOOLEAN) ;
BEGIN
   printf("fly main body\n") ;
   IF 4 DIV ip^ = 4
   THEN
      printf("yes it worked\n")
   ELSE
      printf("no it failed\n")
   END
END fly ;

(*
 *   a GNU M2 version of the Modula-2 example given in the ISO standard.
 *   This is a hand translation of the equivalent except.c file in this
 *   directory which is written to prove that the underlying runtime system
 *   will work with the GCC exception handling trees.
 *)

PROCEDURE tryFlying (debug: BOOLEAN) ;
BEGIN
   printf("tryFlying main body\n");  
   fly (debug) ;
EXCEPT
   printf("inside tryFlying exception routine\n") ;
   IF (ip#NIL) AND (ip^=0)
   THEN
      ip^ := 1 ;
      RETRY
   END
END tryFlying ;


PROCEDURE keepFlying (debug: BOOLEAN) ;
BEGIN
   printf("keepFlying main body\n") ;
   tryFlying (debug) ;
EXCEPT
   printf("inside keepFlying exception routine\n") ;
   IF ip=NIL
   THEN
      NEW(ip) ;
      ip^ := 0 ;
      RETRY
   END
END keepFlying ;


VAR
   ip: POINTER TO INTEGER ;
BEGIN
   ip := NIL ;
   keepFlying (FALSE) ;
   printf("all done\n")
END except9.
