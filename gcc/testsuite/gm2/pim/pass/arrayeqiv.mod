(* Copyright (C) 2006 Free Software Foundation, Inc. *)
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

MODULE arrayeqiv ;

(*
    Title      : arrayeqiv
    Author     : Gaius Mulley
    System     : GNU Modula-2
    Date       : Mon Dec  4 09:52:23 2006
    Revision   : $Version$ 
    Description: tests array equivalence
*)

TYPE
   arrayType1 = ARRAY [0..2] OF CARDINAL;
   arrayType2 = arrayType1 ;
VAR
   ar1, ar2: arrayType2 ;
BEGIN
   ar1[1]:= ar2[2]
END arrayeqiv.
