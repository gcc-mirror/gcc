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
MODULE testarray ;


PROCEDURE bufreadchar (Id: CARDINAL) : INTEGER ;
VAR 
   object: CHAR;  
BEGIN
   WITH files[ Id ] DO
      object := buf[bufindex];
      INC(bufindex);
      RETURN( ORD(object) ) ;
   END
END bufreadchar ;


VAR
   files: ARRAY [0..10] OF RECORD
                              count,
                              bufindex: CARDINAL ;
                              buf     : ARRAY [0..5] OF CHAR ;
                           END ;
   i: INTEGER ;
BEGIN
   i := bufreadchar(1)
END testarray.
