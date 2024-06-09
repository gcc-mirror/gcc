(* Lists.mod provides an unordered list manipulation package.

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

IMPLEMENTATION MODULE Lists ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

CONST
   MaxNoOfElements = 5 ;

TYPE
   List = POINTER TO list ;
   list = RECORD
             NoOfElements: CARDINAL ;
             Elements    : ARRAY [1..MaxNoOfElements] OF WORD ;
             Next        : List ;
          END ;

(*
   InitList - creates a new list, l.
*)

PROCEDURE InitList (VAR l: List) ;
BEGIN
   NEW (l) ;
   WITH l^ DO
      NoOfElements := 0 ;
      Next := NIL
   END
END InitList ;


(*
   KillList - deletes the complete list, l.
*)

PROCEDURE KillList (VAR l: List) ;
BEGIN
   IF l#NIL
   THEN
      IF l^.Next#NIL
      THEN
         KillList(l^.Next)
      END ;
      DISPOSE(l)
   END
END KillList ;


(*
   PutItemIntoList - places a WORD, c, into list, l.
*)

PROCEDURE PutItemIntoList (l: List; c: WORD) ;
BEGIN
   WITH l^ DO
      IF NoOfElements<MaxNoOfElements
      THEN
         INC(NoOfElements) ;
         Elements[NoOfElements] := c
      ELSIF Next#NIL
      THEN
         PutItemIntoList(Next, c)
      ELSE
         InitList(Next) ;
         PutItemIntoList(Next, c)
      END
   END
END PutItemIntoList ;


(*
   GetItemFromList - retrieves the nth WORD from list, l.
                     (recursive solution).
*)
(*
PROCEDURE GetItemFromList (l: List; n: CARDINAL) : WORD ;
BEGIN
   IF n>NoOfItemsInList(l)
   THEN
      RETURN( 0 )
   ELSE
      WITH l^ DO
         IF n<=NoOfElements
         THEN
            RETURN( Elements[n] )
         ELSE
            RETURN( GetItemFromList( Next, n-NoOfElements ) )
         END
      END
   END
END GetItemFromList ;
*)

(* iterative solution *)
PROCEDURE GetItemFromList (l: List; n: CARDINAL) : WORD ;
BEGIN
   WHILE l#NIL DO
      WITH l^ DO
         IF n<=NoOfElements
         THEN
            RETURN( Elements[n] )
         ELSE
            DEC(n, NoOfElements)
         END
      END ;
      l := l^.Next
   END ;
   RETURN( 0 )
END GetItemFromList ;


(*
   GetIndexOfList - returns the index for WORD, c, in list, l.
                    If more than one WORD, c, exists the index
                    for the first is returned.
*)

PROCEDURE GetIndexOfList (l: List; c: WORD) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   IF l=NIL
   THEN
      RETURN( 0 )
   ELSE
      WITH l^ DO
         i := 1 ;
         WHILE i<=NoOfElements DO
            IF Elements[i]=c
            THEN
               RETURN( i )
            ELSE
               INC(i)
            END
         END ;
         RETURN( NoOfElements+GetIndexOfList(Next, c) )
      END
   END
END GetIndexOfList ;


(*
   NoOfItemsInList - returns the number of items in list, l.
*)
(*
PROCEDURE NoOfItemsInList (l: List) : CARDINAL ;
BEGIN
   IF l=NIL
   THEN
      RETURN( 0 )
   ELSE
      WITH l^ DO
         RETURN( NoOfElements+NoOfItemsInList(Next) )
      END
   END
END NoOfItemsInList ;
*)


(*
   NoOfItemsInList - returns the number of items in list, l.
                     (iterative algorithm of the above).
*)

PROCEDURE NoOfItemsInList (l: List) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF l=NIL
   THEN
      RETURN( 0 )
   ELSE
      t := 0 ;
      REPEAT
         WITH l^ DO
            INC(t, NoOfElements)
         END ;
         l := l^.Next
      UNTIL l=NIL;
      RETURN( t )
   END
END NoOfItemsInList ;


(*
   IncludeItemIntoList - adds a WORD, c, into a list providing
                         the value does not already exist.
*)

PROCEDURE IncludeItemIntoList (l: List; c: WORD) ;
BEGIN
   IF NOT IsItemInList(l, c)
   THEN
      PutItemIntoList(l, c)
   END
END IncludeItemIntoList ;


(*
   RemoveItem - remove an element at index, i, from the list data type.
*)

PROCEDURE RemoveItem (p, l: List; i: CARDINAL) ;
BEGIN
   WITH l^ DO
      DEC(NoOfElements) ;
      WHILE i<=NoOfElements DO
         Elements[i] := Elements[i+1] ;
         INC(i)
      END ;
      IF (NoOfElements=0) AND (p#NIL)
      THEN
         p^.Next := l^.Next ;
         DISPOSE(l)
      END
   END
END RemoveItem ;


(*
   RemoveItemFromList - removes a WORD, c, from a list.
                        It assumes that this value only appears once.
*)

PROCEDURE RemoveItemFromList (l: List; c: WORD) ;
VAR
   p    : List ;
   i    : CARDINAL ;
   Found: BOOLEAN ;
BEGIN
   IF l#NIL
   THEN
      Found := FALSE ;
      p := NIL ;
      REPEAT
         WITH l^ DO
            i := 1 ;
            WHILE (i<=NoOfElements) AND (Elements[i]#c) DO
               INC(i)
            END ;
         END ;
         IF (i<=l^.NoOfElements) AND (l^.Elements[i]=c)
         THEN
            Found := TRUE
         ELSE
            p := l ;
            l := l^.Next
         END
      UNTIL (l=NIL) OR Found ;
      IF Found
      THEN
         RemoveItem(p, l, i)
      END
   END
END RemoveItemFromList ;


(*
   IsItemInList - returns true if a WORD, c, was found in list, l.
*)

PROCEDURE IsItemInList (l: List; c: WORD) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   REPEAT
      WITH l^ DO
         i := 1 ;
         WHILE (i<=NoOfElements) DO
            IF Elements[i]=c
            THEN
               RETURN( TRUE )
            ELSE
               INC(i)
            END
         END
      END ;
      l := l^.Next
   UNTIL l=NIL ;
   RETURN( FALSE )
END IsItemInList ;


(*
   ForeachItemInListDo - calls procedure, P, foreach item in list, l.
*)

PROCEDURE ForeachItemInListDo (l: List; P: PerformOperation) ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := NoOfItemsInList(l) ;
   i := 1 ;
   WHILE i<=n DO
      P(GetItemFromList(l, i)) ;
      INC(i)
   END
END ForeachItemInListDo ;


(*
   DuplicateList - returns a duplicate list derived from, l.
*)

PROCEDURE DuplicateList (l: List) : List ;
VAR
   m   : List ;
   n, i: CARDINAL ;
BEGIN
   InitList(m) ;
   n := NoOfItemsInList(l) ;
   i := 1 ;
   WHILE i<=n DO
      PutItemIntoList(m, GetItemFromList(l, i)) ;
      INC(i)
   END ;
   RETURN( m )
END DuplicateList ;


END Lists.
