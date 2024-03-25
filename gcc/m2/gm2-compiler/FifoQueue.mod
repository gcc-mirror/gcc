(* FifoQueue.mod provides a simple fifo queue.

Copyright (C) 2001-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE FifoQueue ;

FROM Lists IMPORT List, InitList, PutItemIntoList, GetItemFromList ;

TYPE
   Fifo = RECORD
             Queue: List ;
             Out  : CARDINAL ;
          END ;

VAR
   const,
   subrange,
   enumeration,
   constructor: Fifo ;


(*
   PutInto - places a CARDINAL number, c, into a fifo queue.
*)

PROCEDURE PutInto (VAR f: Fifo; c: CARDINAL) ;
BEGIN
   WITH f DO
      PutItemIntoList(Queue, c)
   END
END PutInto ;


(*
   GetFrom - retrieves a CARDINAL number, c, from a fifo queue.
*)

PROCEDURE GetFrom (VAR f: Fifo; VAR c: CARDINAL) ;
BEGIN
   WITH f DO
      INC(Out) ;
      c := GetItemFromList(Queue, Out)
   END
END GetFrom ;


(*
   PutEnumerationIntoFifoQueue - places an enumeration symbol, c,
                                 into a fifo queue.
*)

PROCEDURE PutEnumerationIntoFifoQueue (c: CARDINAL) ;
BEGIN
   PutInto(enumeration, c)
END PutEnumerationIntoFifoQueue ;


(*
   GetEnumerationFromFifoQueue - retrieves an enumeration symbol,
                                 c, from a fifo queue.
*)

PROCEDURE GetEnumerationFromFifoQueue (VAR c: CARDINAL) ;
BEGIN
   GetFrom(enumeration, c)
END GetEnumerationFromFifoQueue ;


(*
   PutSubrangeIntoFifoQueue - places a subrange symbol into a fifo
                              queue.
*)

PROCEDURE PutSubrangeIntoFifoQueue (c: CARDINAL) ;
BEGIN
   PutInto(subrange, c)
END PutSubrangeIntoFifoQueue ;


(*
   GetSubrangeFromFifoQueue - retrieves a subrange symbol from a
                              fifo queue.
*)

PROCEDURE GetSubrangeFromFifoQueue (VAR c: CARDINAL) ;
BEGIN
   GetFrom(subrange, c)
END GetSubrangeFromFifoQueue ;


(*
   PutConstructorIntoFifoQueue - places a constructor symbol
                                 into a fifo queue.
*)

PROCEDURE PutConstructorIntoFifoQueue (c: CARDINAL) ;
BEGIN
   PutInto(constructor, c)
END PutConstructorIntoFifoQueue ;


(*
   GetConstructorFromFifoQueue - retrieves a constructor symbol
                                 from a fifo queue.
*)

PROCEDURE GetConstructorFromFifoQueue (VAR c: CARDINAL) ;
BEGIN
   GetFrom(constructor, c)
END GetConstructorFromFifoQueue ;


(*
   PutConstIntoFifoQueue - places a constant symbol
                           into a fifo queue.
*)

PROCEDURE PutConstIntoFifoQueue (c: CARDINAL) ;
BEGIN
   PutInto(const, c)
END PutConstIntoFifoQueue ;


(*
   GetConstFromFifoQueue - retrieves a const symbol
                           from a fifo queue.
*)

PROCEDURE GetConstFromFifoQueue (VAR c: CARDINAL) ;
BEGIN
   GetFrom(const, c)
END GetConstFromFifoQueue ;


(*
   Init - initialize the fifo queue.
*)

PROCEDURE Init (VAR f: Fifo) ;
BEGIN
   WITH f DO
      InitList(Queue) ;
      Out := 0
   END
END Init ;


BEGIN
   Init(const) ;
   Init(enumeration) ;
   Init(subrange) ;
   Init(constructor)
END FifoQueue.
