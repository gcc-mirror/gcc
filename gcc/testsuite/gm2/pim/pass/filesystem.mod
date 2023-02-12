(* Copyright (C) 2005 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free Software
Foundation; either version 2, or (at your option) any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
FOR A PARTICULAR PURPOSE.  See the GNU General Public License for more
details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE filesystem ;

FROM DynamicStrings IMPORT String ;

TYPE
   File = CARDINAL ;

(*
   delete - deletes file, name. It also kills the string, name.
*)

PROCEDURE delete (VAR name: String; VAR f: File) ;
BEGIN
END delete ;

VAR
   n: String ;
   f: File ;
BEGIN
   delete(n, f)
END filesystem.
