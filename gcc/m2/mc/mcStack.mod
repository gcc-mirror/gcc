(* Copyright (C) 2015-2024 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

IMPLEMENTATION MODULE mcStack ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

FROM Indexing IMPORT Index, InitIndex, LowIndice, HighIndice, GetIndice, PutIndice,
                     DeleteIndice, KillIndex ;


TYPE
   stack = POINTER TO RECORD
                         list : Index ;
			 count: CARDINAL ;
                      END ;

(*
   init - create and return a stack.
*)

PROCEDURE init () : stack ;
VAR
   s: stack ;
BEGIN
   NEW (s) ;
   WITH s^ DO
      list := InitIndex (1) ;
      count := 0
   END ;
   RETURN s
END init ;


(*
   kill - deletes stack, s.
*)

PROCEDURE kill (VAR s: stack) ;
BEGIN
   s^.list := KillIndex (s^.list) ;
   DISPOSE (s) ;
   s := NIL
END kill ;


(*
   push - an address, a, onto the stack, s.
          It returns, a.
*)

PROCEDURE push (s: stack; a: ADDRESS) : ADDRESS ;
BEGIN
   WITH s^ DO
      IF count=0
      THEN
         PutIndice (list, LowIndice (list), a)
      ELSE
         PutIndice (list, HighIndice (list)+1, a)
      END ;
      INC (count)
   END ;
   RETURN a
END push ;


(*
   pop - and return the top element from stack, s.
*)

PROCEDURE pop (s: stack) : ADDRESS ;
VAR
   a: ADDRESS ;
BEGIN
   WITH s^ DO
      IF count = 0
      THEN
         HALT
      ELSE
         DEC (count) ;
         a := GetIndice (list, HighIndice (list)) ;
	 DeleteIndice (list, HighIndice (list)) ;
         RETURN a
      END
   END
END pop ;


(*
   replace - performs a pop; push (a); return a.
*)

PROCEDURE replace (s: stack; a: ADDRESS) : ADDRESS ;
VAR
   b: ADDRESS ;
BEGIN
   b := pop (s) ;
   RETURN push (s, a)
END replace ;


(*
   depth - returns the depth of the stack.
*)

PROCEDURE depth (s: stack) : CARDINAL ;
BEGIN
   RETURN s^.count
END depth ;


(*
   access - returns the, i, th stack element.
            The top of stack is defined by:

            access (s, depth (s)).
*)

PROCEDURE access (s: stack; i: CARDINAL) : ADDRESS ;
BEGIN
   IF (i>s^.count) OR (i=0)
   THEN
      HALT
   ELSE
      RETURN GetIndice (s^.list, i)
   END
END access ;


END mcStack.
