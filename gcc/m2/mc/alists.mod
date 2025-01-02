(* alists.mod address lists module.

Copyright (C) 2015-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius@glam.ac.uk>.

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

IMPLEMENTATION MODULE alists ;


FROM Storage IMPORT ALLOCATE, DEALLOCATE ;

CONST
   MaxnoOfelements = 5 ;

TYPE
   alist = POINTER TO RECORD
              noOfelements: CARDINAL ;
              elements    : ARRAY [1..MaxnoOfelements] OF ADDRESS ;
              next        : alist ;
           END ;


(*
   initList - creates a new alist, l.
*)

PROCEDURE initList () : alist ;
VAR
   l: alist ;
BEGIN
   NEW (l) ;
   WITH l^ DO
      noOfelements := 0 ;
      next := NIL
   END ;
   RETURN l
END initList ;


(*
   killList - deletes the complete alist, l.
*)

PROCEDURE killList (VAR l: alist) ;
BEGIN
   IF l#NIL
   THEN
      IF l^.next#NIL
      THEN
         killList (l^.next)
      END ;
      DISPOSE (l)
   END
END killList ;


(*
   putItemIntoList - places an ADDRESS, c, into alist, l.
*)

PROCEDURE putItemIntoList (l: alist; c: ADDRESS) ;
BEGIN
   WITH l^ DO
      IF noOfelements<MaxnoOfelements
      THEN
         INC (noOfelements) ;
         elements[noOfelements] := c
      ELSIF next#NIL
      THEN
         putItemIntoList (next, c)
      ELSE
         next := initList () ;
         putItemIntoList (next, c)
      END
   END
END putItemIntoList ;


(*
   getItemFromList - retrieves the nth WORD from alist, l.
*)

PROCEDURE getItemFromList (l: alist; n: CARDINAL) : ADDRESS ;
BEGIN
   WHILE l#NIL DO
      WITH l^ DO
         IF n<=noOfelements
         THEN
            RETURN elements[n]
         ELSE
            DEC (n, noOfelements)
         END
      END ;
      l := l^.next
   END ;
   RETURN 0
END getItemFromList ;


(*
   getIndexOfList - returns the index for WORD, c, in alist, l.
                    If more than one WORD, c, exists the index
                    for the first is returned.
*)

PROCEDURE getIndexOfList (l: alist; c: ADDRESS) : CARDINAL ;
VAR
   i: CARDINAL ;
BEGIN
   IF l=NIL
   THEN
      RETURN 0
   ELSE
      WITH l^ DO
         i := 1 ;
         WHILE i<=noOfelements DO
            IF elements[i]=c
            THEN
               RETURN i
            ELSE
               INC(i)
            END
         END ;
         RETURN noOfelements + getIndexOfList (next, c)
      END
   END
END getIndexOfList ;


(*
   noOfItemsInList - returns the number of items in alist, l.
*)

PROCEDURE noOfItemsInList (l: alist) : CARDINAL ;
VAR
   t: CARDINAL ;
BEGIN
   IF l=NIL
   THEN
      RETURN 0
   ELSE
      t := 0 ;
      REPEAT
         WITH l^ DO
            INC (t, noOfelements)
         END ;
         l := l^.next
      UNTIL l=NIL;
      RETURN t
   END
END noOfItemsInList ;


(*
   includeItemIntoList - adds an ADDRESS, c, into a alist providing
                         the value does not already exist.
*)

PROCEDURE includeItemIntoList (l: alist; c: ADDRESS) ;
BEGIN
   IF NOT isItemInList (l, c)
   THEN
      putItemIntoList (l, c)
   END
END includeItemIntoList ;


(*
   removeItem - remove an element at index, i, from the alist data type.
*)

PROCEDURE removeItem (p, l: alist; i: CARDINAL) ;
BEGIN
   WITH l^ DO
      DEC (noOfelements) ;
      WHILE i<=noOfelements DO
         elements[i] := elements[i+1] ;
         INC (i)
      END ;
      IF (noOfelements=0) AND (p#NIL)
      THEN
         p^.next := l^.next ;
         DISPOSE (l)
      END
   END
END removeItem ;


(*
   removeItemFromList - removes a ADDRESS, c, from a alist.
                        It assumes that this value only appears once.
*)

PROCEDURE removeItemFromList (l: alist; c: ADDRESS) ;
VAR
   p    : alist ;
   i    : CARDINAL ;
   found: BOOLEAN ;
BEGIN
   IF l#NIL
   THEN
      found := FALSE ;
      p := NIL ;
      REPEAT
         WITH l^ DO
            i := 1 ;
            WHILE (i<=noOfelements) AND (elements[i]#c) DO
               INC (i)
            END ;
         END ;
         IF (i<=l^.noOfelements) AND (l^.elements[i]=c)
         THEN
            found := TRUE
         ELSE
            p := l ;
            l := l^.next
         END
      UNTIL (l=NIL) OR found ;
      IF found
      THEN
         removeItem (p, l, i)
      END
   END
END removeItemFromList ;


(*
   isItemInList - returns true if a ADDRESS, c, was found in alist, l.
*)

PROCEDURE isItemInList (l: alist; c: ADDRESS) : BOOLEAN ;
VAR
   i: CARDINAL ;
BEGIN
   REPEAT
      WITH l^ DO
         i := 1 ;
         WHILE i<=noOfelements DO
            IF elements[i]=c
            THEN
               RETURN TRUE
            ELSE
               INC (i)
            END
         END
      END ;
      l := l^.next
   UNTIL l=NIL ;
   RETURN FALSE
END isItemInList ;


(*
   foreachItemInListDo - calls procedure, P, foreach item in alist, l.
*)

PROCEDURE foreachItemInListDo (l: alist; p: performOperation) ;
VAR
   i, n: CARDINAL ;
BEGIN
   n := noOfItemsInList(l) ;
   i := 1 ;
   WHILE i<=n DO
      p (getItemFromList (l, i)) ;
      INC(i)
   END
END foreachItemInListDo ;


(*
   duplicateList - returns a duplicate alist derived from, l.
*)

PROCEDURE duplicateList (l: alist) : alist ;
VAR
   m   : alist ;
   n, i: CARDINAL ;
BEGIN
   m := initList () ;
   n := noOfItemsInList (l) ;
   i := 1 ;
   WHILE i<=n DO
      putItemIntoList (m, getItemFromList (l, i)) ;
      INC (i)
   END ;
   RETURN m
END duplicateList ;


(*
   equalList - returns TRUE if left contains the same information as right.
*)

PROCEDURE equalList (left, right: alist) : BOOLEAN ;
VAR
   leftn, rightn, i: CARDINAL ;
BEGIN
   leftn := noOfItemsInList (left) ;
   rightn := noOfItemsInList (right) ;
   IF leftn = rightn
   THEN
      i := 1 ;
      WHILE i <= leftn DO
         IF isItemInList (right, getItemFromList (left, i))
         THEN
            INC (i)
         ELSE
            RETURN FALSE
         END
      END
   ELSE
      RETURN FALSE
   END ;
   RETURN TRUE
END equalList ;


END alists.
