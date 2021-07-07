(* M2StackAddress.mod provides a generic stack for ADDRESS sized objects.

Copyright (C) 2001-2021 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE M2StackAddress ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Error IMPORT InternalError ;
FROM M2Debug IMPORT Assert ;

CONST
   MaxBucket = 10 ;

TYPE
   StackBucket     = POINTER TO RECORD
                                   bucket: ARRAY [0..MaxBucket-1] OF ADDRESS ;
                                   items : CARDINAL ;
                                   prev  : StackBucket ;
                                END ;

   StackOfAddress    = POINTER TO RECORD
                                     tail: StackBucket ;
                                  END ;


(*
   InitStackAddress - creates and returns a new stack.
*)

PROCEDURE InitStackAddress () : StackOfAddress ;
VAR
   s: StackOfAddress ;
BEGIN
   NEW (s) ;
   WITH s^ DO
      tail := NIL
   END ;
   RETURN s
END InitStackAddress ;


(*
   KillBucket - destroys a StackBucket and returns, NIL.
*)

PROCEDURE KillBucket (b: StackBucket) : StackBucket ;
BEGIN
   IF b # NIL
   THEN
      b := KillBucket (b^.prev) ;
      DISPOSE (b)
   END ;
   RETURN NIL
END KillBucket ;


(*
   KillStackAddress - destroys a stack, returning NIL.
*)

PROCEDURE KillStackAddress (s: StackOfAddress) : StackOfAddress ;
BEGIN
   IF s#NIL
   THEN
      s^.tail := KillBucket (s^.tail) ;
      DISPOSE (s)
   END ;
   RETURN NIL
END KillStackAddress ;


(*
   InitBucket - returns an empty StackBucket.
*)

PROCEDURE InitBucket (l: StackBucket) : StackBucket ;
VAR
   b: StackBucket ;
BEGIN
   NEW(b) ;
   WITH b^ DO
      items := 0 ;
      prev  := l
   END ;
   RETURN( b )
END InitBucket ;


(*
   PushAddress - pushes a word, w, onto, s.
*)

PROCEDURE PushAddress (s: StackOfAddress; w: ADDRESS) ;
BEGIN
   IF s=NIL
   THEN
      InternalError ('stack has not been initialized')
   ELSE
      WITH s^ DO
         IF (tail=NIL) OR (tail^.items=MaxBucket)
         THEN
            tail := InitBucket(tail)
         END ;
         WITH tail^ DO
            IF items<MaxBucket
            THEN
               bucket[items] := w ;
               INC(items)
            END
         END
      END
   END
END PushAddress ;


(*
   PopAddress - pops an element from stack, s.
*)

PROCEDURE PopAddress (s: StackOfAddress) : ADDRESS ;
VAR
   b: StackBucket ;
BEGIN
   IF s=NIL
   THEN
      InternalError ('stack has not been initialized')
   ELSE
      IF s^.tail=NIL
      THEN
         InternalError ('stack underflow')
      ELSE
         IF s^.tail^.items=0
         THEN
            b := s^.tail ;
            IF b=NIL
            THEN
               InternalError ('stack underflow')
            ELSE
               s^.tail := b^.prev
            END ;
            DISPOSE(b)
         END ;
         WITH s^.tail^ DO
            DEC(items) ;
            RETURN( bucket[items] )
         END
      END
   END
END PopAddress ;


(*
   IsEmptyAddress - returns TRUE if stack, s, is empty.
*)

PROCEDURE IsEmptyAddress (s: StackOfAddress) : BOOLEAN ;
BEGIN
   RETURN( (s=NIL) OR (s^.tail=NIL) )
END IsEmptyAddress ;


(*
   PeepAddress - returns the element at, n, items below in the stack.
                 Top of stack can be seen via Peep(s, 1)
*)

PROCEDURE PeepAddress (s: StackOfAddress; n: CARDINAL) : ADDRESS ;
VAR
   b: StackBucket ;
BEGIN
   IF s^.tail=NIL
   THEN
      InternalError ('stack underflow')
   ELSE
      IF s^.tail^.items=0
      THEN
         b := s^.tail ;
         IF b=NIL
         THEN
            InternalError ('stack underflow')
         ELSE
            s^.tail := b^.prev
         END ;
         DISPOSE(b)
      END ;
      b := s^.tail ;
      WHILE n>=1 DO
         IF b=NIL
         THEN
            InternalError ('stack underflow')
         ELSIF b^.items>=n
         THEN
            RETURN( b^.bucket[b^.items-n] )
         ELSE
            Assert(b^.items<n) ;
            DEC(n, b^.items) ;
            b := b^.prev
         END
      END ;
      InternalError ('stack underflow')
   END
END PeepAddress ;


(*
   ReduceAddress - reduce the stack by n elements.
*)

PROCEDURE ReduceAddress (s: StackOfAddress; n: CARDINAL) ;
VAR
   b: StackBucket ;
BEGIN
   IF s^.tail=NIL
   THEN
      InternalError ('stack underflow')
   ELSE
      IF s^.tail^.items=0
      THEN
         b := s^.tail ;
         IF b=NIL
         THEN
            InternalError ('stack underflow')
         ELSE
            s^.tail := b^.prev
         END ;
         DISPOSE(b)
      END ;
      LOOP
         IF s^.tail=NIL
         THEN
            InternalError ('stack underflow')
         ELSIF s^.tail^.items>=n
         THEN
            DEC( s^.tail^.items, n) ;
            RETURN  (* all done exit *)
         ELSE
            b := s^.tail ;
            DEC(n, b^.items) ;
            s^.tail := s^.tail^.prev ;
            DISPOSE(b)
         END
      END
   END
END ReduceAddress ;


(*
   NoOfItemsInStackAddress - returns the number of items held in the stack, s.
*)

PROCEDURE NoOfItemsInStackAddress (s: StackOfAddress) : CARDINAL ;
VAR
   b: StackBucket ;
   n: CARDINAL ;
BEGIN
   IF IsEmptyAddress(s)
   THEN
      RETURN( 0 )
   ELSE
      n := 0 ;
      b := s^.tail ;
      WHILE b#NIL DO
         INC (n, b^.items) ;
         b := b^.prev
      END ;
      RETURN( n )
   END
END NoOfItemsInStackAddress ;


END M2StackAddress.
