(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

MODULE except5 ;

FROM libc IMPORT printf ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM SYSTEM IMPORT ADR, WORD, THROW ;


PROCEDURE fly ;
VAR
   r: INTEGER ;
BEGIN
   IF ip=NIL
   THEN
      THROW(1)
   END ;
   IF 4 DIV ip^ = 4
   THEN
      r := printf("yes it worked\n")
   END ;
END fly ;

(*
 *   a heavily reduced test case, to aid debugging the compiler..
 *)

PROCEDURE tryFlying ;
VAR
   r: INTEGER ;
BEGIN
   fly
   EXCEPT
   IF ip=NIL
   THEN
      r := printf("inside Modula-2 exception handler\n");
      NEW(ip) ;
      ip^ := 1 ;
      RETRY
   END
END tryFlying ;

VAR
   r : INTEGER ;
   ip: POINTER TO INTEGER ;
BEGIN
   ip := NIL ;
   tryFlying ;
   r := printf("all done\n")
END except5.
