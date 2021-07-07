(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

MODULE badparam2 ;

PROCEDURE foo (i: INTEGER; c: myset1) : BOOLEAN ;
BEGIN
   RETURN( FALSE )
END foo ;

TYPE
   proc = PROCEDURE (INTEGER, myset2) : BOOLEAN ;
   myset1 = SET OF [low..high] ;
   myset2 = SET OF [low+1..high-1] ;

CONST
   high = low + 10 ;
   low  = 2 ;

PROCEDURE func (pa: proc) ;
BEGIN
END func ;


VAR
   p: proc ;
BEGIN
   func(foo) ;
   (* func(p) ;  (* ok *) *)
END badparam2.
