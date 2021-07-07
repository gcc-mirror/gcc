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

MODULE procmod3 ;


PROCEDURE bar ;
VAR
   variable: CARDINAL ;

   PROCEDURE error ;
   BEGIN
   END error ;

   MODULE foo ;
   IMPORT error, variable ;

(*
   PROCEDURE try ;
   BEGIN
      variable := 99 ;
      error
   END try ;
*)

   VAR
      x: INTEGER ;
   BEGIN
      x := 101 ;
      variable := 99 ;
      error
   END foo ;


BEGIN
END bar ;

BEGIN
   bar
END procmod3.
