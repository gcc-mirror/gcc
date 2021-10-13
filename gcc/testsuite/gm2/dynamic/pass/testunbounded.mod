(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE testunbounded ;

FROM StrIO IMPORT WriteString ;


TYPE
   DESCRIPTOR = POINTER TO RECORD
                              a, b, c, d, e: CARDINAL ;
                              RunName: ARRAY [0..15] OF CHAR ;
                           END ;


PROCEDURE DisplayProcess (p: DESCRIPTOR) ;
VAR
   a: ARRAY [0..4] OF CHAR ;
   db: ARRAY [0..50] OF CHAR ;
BEGIN
   WITH p^ DO
      WriteString(RunName)
   END
END DisplayProcess ;


VAR
   p: DESCRIPTOR ;
BEGIN
   DisplayProcess(p)
END testunbounded.
