(* nameKey.mod provides a dynamic binary tree name to key.

Copyright (C) 2015-2024 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE nameKey ;


FROM SYSTEM IMPORT ADR ;
FROM Storage IMPORT ALLOCATE, DEALLOCATE ;
FROM Indexing IMPORT Index, InitIndex, GetIndice, PutIndice, InBounds ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM StdIO IMPORT Write ;
FROM NumberIO IMPORT WriteCard ;
FROM StrLib IMPORT StrLen ;
FROM libc IMPORT strlen ;
FROM ASCII IMPORT nul ;


TYPE
   ptrToChar  = POINTER TO CHAR ;

   nameNode   = POINTER TO RECORD
                   data : ptrToChar ;
                   key  : Name ;
                   left,
                   right: nameNode ;
                END ;

   comparison = (less, equal, greater) ;

VAR
   binaryTree: nameNode ;
   keyIndex  : Index ;
   lastIndice: CARDINAL ;


(*
   getKey - returns the name, a, of the key, Key.
*)

PROCEDURE getKey (key: Name; VAR a: ARRAY OF CHAR) ;
VAR
   p       : ptrToChar ;
   i, higha: CARDINAL ;
BEGIN
   p := keyToCharStar (key) ;
   i := 0 ;
   higha := HIGH (a) ;
   WHILE (p#NIL) AND (i<=higha) AND (p^#nul) DO
      a[i] := p^ ;
      INC (p) ;
      INC (i)
   END ;
   IF i<=higha
   THEN
      a[i] := nul
   END
END getKey ;


(*
   isKey - returns TRUE if string, a, is currently a key.
           We dont use the Compare function, we inline it and avoid
           converting, a, into a String, for speed.
*)

PROCEDURE isKey (a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   child : nameNode ;
   p     : ptrToChar ;
   i,
   higha : CARDINAL ;
BEGIN
   (* firstly set up the initial values of child, using sentinal node *)
   child := binaryTree^.left ;
   IF child#NIL
   THEN
      REPEAT
         i := 0 ;
         higha := HIGH (a) ;
         p := keyToCharStar (child^.key) ;
         WHILE (i<=higha) AND (a[i]#nul) DO
            IF a[i]<p^
            THEN
               child := child^.left ;
               i := higha
            ELSIF a[i]>p^
            THEN
               child := child^.right ;
               i := higha
            ELSE
               IF (a[i]=nul) OR (i=higha)
               THEN
                  IF p^=nul
                  THEN
                     RETURN TRUE
                  ELSE
                     child := child^.left
                  END
               END ;
               INC (p)
            END ;
            INC (i)
         END ;
      UNTIL child=NIL
   END ;
   RETURN FALSE
END isKey ;


(*
   doMakeKey - finds the name, n, in the tree or else create a name.
               If a name is found then the string, n, is deallocated.
*)

PROCEDURE doMakeKey (n: ptrToChar; higha: CARDINAL) : Name ;
VAR
   result: comparison ;
   father,
   child : nameNode ;
   k     : Name ;
BEGIN
   result := findNodeAndParentInTree (n, child, father) ;
   IF child=NIL
   THEN
      IF result=less
      THEN
         NEW (child) ;
         father^.left := child
      ELSIF result=greater
      THEN
         NEW (child) ;
         father^.right := child
      END ;
      WITH child^ DO
         right := NIL ;
         left := NIL ;
         INC (lastIndice) ;
         key := lastIndice ;
         data := n ;
         PutIndice (keyIndex, key, n)
      END ;
      k := lastIndice
   ELSE
      DEALLOCATE (n, higha+1) ;
      k := child^.key
   END ;
   RETURN k
END doMakeKey ;


(*
   makeKey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
*)

PROCEDURE makeKey (a: ARRAY OF CHAR) : Name ;
VAR
   n, p  : ptrToChar ;
   i,
   higha : CARDINAL ;
BEGIN
   higha := StrLen(a) ;
   ALLOCATE (p, higha+1) ;
   IF p=NIL
   THEN
      HALT      (* out of memory error *)
   ELSE
      n := p ;
      i := 0 ;
      WHILE i<higha DO
         p^ := a[i] ;
         INC(i) ;
         INC(p)
      END ;
      p^ := nul ;

      RETURN doMakeKey (n, higha)
   END
END makeKey ;


(*
   makekey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
             These keys last for the duration of compilation.
*)

PROCEDURE makekey (a: ADDRESS) : Name ;
VAR
   n,
   p, pa : ptrToChar ;
   i,
   higha : CARDINAL ;
BEGIN
   IF a=NIL
   THEN
      RETURN NulName
   ELSE
      higha := strlen (a) ;
      ALLOCATE (p, higha+1) ;
      IF p=NIL
      THEN
         HALT      (* out of memory error *)
      ELSE
         n  := p ;
         pa := a ;
         i  := 0 ;
         WHILE i<higha DO
            p^ := pa^ ;
            INC (i) ;
            INC (p) ;
            INC (pa)
         END ;
         p^ := nul ;

         RETURN doMakeKey (n, higha)
      END
   END
END makekey ;


(*
   lengthKey - returns the StrLen of Key.
*)

PROCEDURE lengthKey (key: Name) : CARDINAL ;
VAR
   i: CARDINAL ;
   p: ptrToChar ;
BEGIN
   p := keyToCharStar (key) ;
   i := 0 ;
   WHILE p^#nul DO
      INC (i) ;
      INC (p)
   END ;
   RETURN i
END lengthKey ;


(*
   compare - return the result of Names[i] with Names[j]
*)

PROCEDURE compare (pi: ptrToChar; j: Name) : comparison ;
VAR
   pj: ptrToChar ;
   c1, c2: CHAR ;
BEGIN
   pj := keyToCharStar(j) ;
   c1 := pi^ ;
   c2 := pj^ ;
   WHILE (c1#nul) OR (c2#nul) DO
      IF c1<c2
      THEN
         RETURN less
      ELSIF c1>c2
      THEN
         RETURN greater
      ELSE
         INC (pi) ;
         INC (pj) ;
         c1 := pi^ ;
         c2 := pj^
      END
   END ;
   RETURN equal
END compare ;


(*
   findNodeAndParentInTree - search BinaryTree for a name.
                             If this name is found in the BinaryTree then
                             child is set to this name and father is set to the node above.
                             A comparison is returned to assist adding entries into this tree.
*)

PROCEDURE findNodeAndParentInTree (n: ptrToChar; VAR child, father: nameNode) : comparison ;
VAR
   result: comparison ;
BEGIN
   (* firstly set up the initial values of child and father, using sentinal node *)
   father := binaryTree ;
   child := binaryTree^.left ;
   IF child=NIL
   THEN
      RETURN less
   ELSE
      REPEAT
         result := compare (n, child^.key) ;
         IF result=less
         THEN
            father := child ;
            child := child^.left
         ELSIF result=greater
         THEN
            father := child ;
            child := child^.right
         END
      UNTIL (child=NIL) OR (result=equal) ;
      RETURN result
   END
END findNodeAndParentInTree ;


(*
   isSameExcludingCase - returns TRUE if key1 and key2 are
                         the same. It is case insensitive.
                         This function deliberately inlines CAP for speed.
*)

PROCEDURE isSameExcludingCase (key1, key2: Name) : BOOLEAN ;
VAR
   pi, pj: ptrToChar ;
   c1, c2: CHAR ;
BEGIN
   IF key1=key2
   THEN
      RETURN TRUE
   ELSE
      pi := keyToCharStar(key1) ;
      pj := keyToCharStar(key2) ;
      c1 := pi^ ;
      c2 := pj^ ;
      WHILE (c1#nul) AND (c2#nul) DO
         IF (c1=c2) OR
            (((c1>='A') AND (c1<='Z')) AND (c2=CHR(ORD(c1)-ORD('A')+ORD('a')))) OR
            (((c2>='A') AND (c2<='Z')) AND (c1=CHR(ORD(c2)-ORD('A')+ORD('a'))))
         THEN
            INC (pi) ;
            INC (pj) ;
            c1 := pi^ ;
            c2 := pj^
         ELSE
            (* difference found *)
            RETURN FALSE
         END
      END ;
      RETURN c1=c2
   END
END isSameExcludingCase ;


(*
   keyToCharStar - returns the C char * string equivalent for, key.
*)

PROCEDURE keyToCharStar (key: Name) : ADDRESS ;
BEGIN
   IF (key=NulName) OR (NOT InBounds (keyIndex, key))
   THEN
      RETURN NIL
   ELSE
      RETURN GetIndice (keyIndex, key)
   END
END keyToCharStar ;


PROCEDURE writeKey (key: Name) ;
VAR
   s: ptrToChar ;
BEGIN
   s := keyToCharStar (key) ;
   WHILE (s#NIL) AND (s^#nul) DO
      Write (s^) ;
      INC (s)
   END
END writeKey ;


BEGIN
   lastIndice := 0 ;
   keyIndex := InitIndex(1) ;
   NEW (binaryTree) ;
   binaryTree^.left := NIL
END nameKey.
