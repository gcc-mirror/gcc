(* M2StackWord.mod provides a generic stack for WORD sized objects.

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

IMPLEMENTATION MODULE M2StackWord ;

FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM M2Error IMPORT InternalError ;
FROM M2Debug IMPORT Assert ;

CONST
   MaxBucket = 10 ;

TYPE
   StackBucket     = POINTER TO Bucket ;
   Bucket          = RECORD
                         bucket: ARRAY [0..MaxBucket-1] OF WORD ;
                         items : CARDINAL ;
                         last  : StackBucket ;
                     END ;

   StackOfWord     = POINTER TO StackDescriptor ;
   StackDescriptor = RECORD
                        tail: StackBucket ;
                     END ;


(*
   InitStackWord - creates and returns a new stack.
*)

PROCEDURE InitStackWord () : StackOfWord ;
VAR
   s: StackOfWord ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      tail := NIL
   END ;
   RETURN( s )
END InitStackWord ;


(*
   KillBucket - destroys a StackBucket and returns, NIL.
*)

PROCEDURE KillBucket (b: StackBucket) : StackBucket ;
BEGIN
   IF b#NIL
   THEN
      b := KillBucket(b^.last) ;
      DISPOSE(b)
   END ;
   RETURN( NIL )
END KillBucket ;


(*
   KillStackWord - destroys a stack, returning NIL.
*)

PROCEDURE KillStackWord (s: StackOfWord) : StackOfWord ;
BEGIN
   IF s#NIL
   THEN
      s^.tail := KillBucket(s^.tail) ;
      DISPOSE(s)
   END ;
   RETURN( NIL )
END KillStackWord ;


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
      last  := l
   END ;
   RETURN( b )
END InitBucket ;


(*
   PushWord - pushes a word, w, onto, s.
*)

PROCEDURE PushWord (s: StackOfWord; w: WORD) ;
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
END PushWord ;


(*
   PopWord - pops an element from stack, s.
*)

PROCEDURE PopWord (s: StackOfWord) : WORD ;
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
               s^.tail := b^.last
            END ;
            DISPOSE(b)
         END ;
         WITH s^.tail^ DO
            DEC(items) ;
            RETURN( bucket[items] )
         END
      END
   END
END PopWord ;


(*
   IsEmptyWord - returns TRUE if stack, s, is empty.
*)

PROCEDURE IsEmptyWord (s: StackOfWord) : BOOLEAN ;
BEGIN
   RETURN( (s=NIL) OR (s^.tail=NIL) )
END IsEmptyWord ;


(*
   PeepWord - returns the element at, n, items below in the stack.
              Top of stack can be seen via Peep(s, 1)
*)

PROCEDURE PeepWord (s: StackOfWord; n: CARDINAL) : WORD ;
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
            s^.tail := b^.last
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
            b := b^.last
         END
      END ;
      InternalError ('stack underflow')
   END
END PeepWord ;


(*
   ReduceWord - reduce the stack by n elements.
*)

PROCEDURE ReduceWord (s: StackOfWord; n: CARDINAL) ;
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
            s^.tail := b^.last
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
            s^.tail := s^.tail^.last ;
            DISPOSE(b)
         END
      END
   END
END ReduceWord ;


(*
   NoOfItemsInStackWord - returns the number of items held in the stack, s.
*)

PROCEDURE NoOfItemsInStackWord (s: StackOfWord) : CARDINAL ;
VAR
   b: StackBucket ;
   n: CARDINAL ;
BEGIN
   IF IsEmptyWord(s)
   THEN
      RETURN( 0 )
   ELSE
      n := 0 ;
      b := s^.tail ;
      WHILE b#NIL DO
         INC(n, b^.items) ;
         b := b^.last
      END ;
      RETURN( n )
   END
END NoOfItemsInStackWord ;


END M2StackWord.
