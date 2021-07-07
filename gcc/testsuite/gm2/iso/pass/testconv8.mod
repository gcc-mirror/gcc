(* Copyright (C) 2017 Free Software Foundation, Inc.  *)
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
Boston, MA 02110-1301, USA.  *)

MODULE testconv8 ;   (*!m2iso*)

FROM libc IMPORT printf ;

TYPE
   day = [1..31] ;

PROCEDURE test ;
VAR
   d1: day ;
   c : CARDINAL ;
   i : INTEGER ;
BEGIN
   IF -3+2 = i
   THEN
      printf ("something 1\n")
   END ;
   IF -3+2 = c
   THEN
      printf ("something 2\n")
   END ;
   IF -3+4 = d1
   THEN
      printf ("something 3\n")
   END
END test ;

BEGIN
   test
END testconv8.
