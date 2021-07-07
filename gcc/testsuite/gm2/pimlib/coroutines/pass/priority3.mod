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

IMPLEMENTATION MODULE priority3[7] ;

(* Procedures p3 and p4 do not need to call TurnInterrupts whereas
   p1 and p2 will need to call TurnInterrupts
*)

PROCEDURE p1 ;
BEGIN
   p2
END p1 ;

MODULE inner[0] ;

EXPORT p2 ;

PROCEDURE p2 ;
BEGIN
END p2 ;

PROCEDURE p3 ;
BEGIN
END p3 ;

BEGIN
END inner ;

PROCEDURE p4 ;
BEGIN
END p4 ;

BEGIN
   p1
END priority3.
