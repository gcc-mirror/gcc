(* Copyright (C) 2010 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

MODULE int8field ;

IMPORT SYSTEM ;
FROM libc IMPORT exit ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteLn ;

PROCEDURE assert (a, b: CARDINAL) ;
BEGIN
   WriteCard(a, 1) ; WriteLn ;
   IF a#b
   THEN
      exit(1)
   END
END assert ;

TYPE
   Version = RECORD
                major : SYSTEM.CARDINAL8;
                minor : SYSTEM.CARDINAL8;
                path  : SYSTEM.CARDINAL8;
             END ;

VAR
   v: Version ;
BEGIN
   v.major := 1 ;
   v.minor := 2 ;
   v.path  := 3 ;
   assert(v.major, 1) ;
   assert(v.minor, 2) ;
   assert(v.path, 3)
END int8field.
