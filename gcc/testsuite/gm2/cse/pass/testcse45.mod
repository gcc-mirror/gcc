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
MODULE testcse45 ;


TYPE

   Where = RECORD
              Declared,
              FirstUsed: CARDINAL ;
           END ;

   SymUndefined = RECORD
                     Name      : CARDINAL ;   (* Index into name array, name *)
                                              (* of record.                  *)
                     At        : Where ;      (* Where was sym declared/used *)
                  END ;


VAR
   myvar: SymUndefined ;


PROCEDURE InitWhereDeclared (VAR at: Where) ;
BEGIN
   WITH at DO
      Declared :=  1 ;
      FirstUsed := 2
   END
END InitWhereDeclared ;


(*
   MakeUnbounded - makes an unbounded array Symbol.
                   No name is required.
*)

PROCEDURE MakeUnbounded ;
BEGIN
   WITH myvar DO
      InitWhereDeclared(At) (* Declared here               *)
   END
END MakeUnbounded ;


BEGIN
   MakeUnbounded
END testcse45.
