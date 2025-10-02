(* Copyright (C) 2024 Free Software Foundation, Inc. *)
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

MODULE setincl ;

FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT ROTATE ;


PROCEDURE incl (VAR set: BITSET; bit: CARDINAL) ;
BEGIN
   INCL (set, bit)
END incl ;


PROCEDURE excl (VAR set: BITSET; bit: CARDINAL) ;
BEGIN
   EXCL (set, bit)
END excl ;


VAR
   set: BITSET ;
BEGIN
   set := BITSET {} ;
   incl (set, 1) ;
   IF set # BITSET {1}
   THEN
      exit (1)
   END ;
   excl (set, 1) ;
   IF set # BITSET {}
   THEN
      exit (2)
   END ;
   exit (0)
END setincl.
