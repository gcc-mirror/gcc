(* Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE procmod5 ;

PROCEDURE a ;
   PROCEDURE b ;
       PROCEDURE c ;
           MODULE d ; IMPORT e ; BEGIN e END d ;
       END c;
   END b ;
   PROCEDURE e ; VAR e1: CARDINAL ; BEGIN e1 := 11 END e ;
END a ;

PROCEDURE e ; VAR e1: CARDINAL ; BEGIN e1 := 99 END e ;

BEGIN
   a
END procmod5.
