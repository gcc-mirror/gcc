(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE testlength2 ;

FROM libc IMPORT exit ;
(* FROM Strings IMPORT Length ; *)

PROCEDURE Length (a: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   RETURN 12
END Length ;

VAR
   a: ARRAY [0..20] OF CHAR ;
   l: CARDINAL ;
BEGIN
   a := "hello world" ;
   l := LENGTH(a) ;
   IF l#12
   THEN
      exit(1)
   END
END testlength2.
