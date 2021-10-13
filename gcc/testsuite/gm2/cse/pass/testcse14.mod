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
MODULE testcse14 ;


PROCEDURE Power10 (r: REAL; power: CARDINAL) : REAL;
VAR 
   i: CARDINAL;
BEGIN
   i := 0 ;
   WHILE i<power DO
      r := r * 10.0 ;
      INC(i)
   END ;
   RETURN( r )
END Power10 ;


BEGIN
   IF Power10(1.0, 1)=2.0
   THEN
   END
END testcse14.
