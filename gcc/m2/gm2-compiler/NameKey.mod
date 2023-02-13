(* NameKey.mod provides a dynamic binary tree name to key.

Copyright (C) 2001-2023 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE NameKey ;


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
   PtrToChar  = POINTER TO CHAR ;

   NameNode   = POINTER TO Node ;
   Node       = RECORD
                   Data : PtrToChar ;
                   Key  : Name ;
                   Left,
                   Right: NameNode ;
                END ;

   Comparison = (less, equal, greater) ;

VAR
   BinaryTree: NameNode ;
   KeyIndex  : Index ;
   LastIndice: CARDINAL ;


(*
   GetKey - returns the name, a, of the key, Key.
*)

PROCEDURE GetKey (key: Name; VAR a: ARRAY OF CHAR) ;
VAR
   p       : PtrToChar ;
   i, higha: CARDINAL ;
BEGIN
   p := KeyToCharStar(key) ;
   i := 0 ;
   higha := HIGH(a) ;
   WHILE (p#NIL) AND (i<=higha) AND (p^#nul) DO
      a[i] := p^ ;
      INC(p) ;
      INC(i)
   END ;
   IF i<=higha
   THEN
      a[i] := nul
   END
END GetKey ;


(*
   IsKey - returns TRUE if string, a, is currently a key.
           We dont use the Compare function, we inline it and avoid
           converting, a, into a String, for speed.
*)

PROCEDURE IsKey (a: ARRAY OF CHAR) : BOOLEAN ;
VAR
   child : NameNode ;
   p     : PtrToChar ;
   i,
   higha : CARDINAL ;
BEGIN
   (* firstly set up the initial values of child, using sentinal node *)
   child := BinaryTree^.Left ;
   IF child#NIL
   THEN
      REPEAT
         i := 0 ;
         higha := HIGH(a) ;
         p := KeyToCharStar(child^.Key) ;
         WHILE (i<=higha) AND (a[i]#nul) DO
            IF a[i]<p^
            THEN
               child := child^.Left ;
               i := higha
            ELSIF a[i]>p^
            THEN
               child := child^.Right ;
               i := higha
            ELSE
               IF (a[i]=nul) OR (i=higha)
               THEN
                  IF p^=nul
                  THEN
                     RETURN( TRUE )
                  ELSE
                     child := child^.Left
                  END
               END ;
               INC(p)
            END ;
            INC(i)
         END ;
      UNTIL child=NIL
   END ;
   RETURN( FALSE ) ;
END IsKey ;


(*
   DoMakeKey - finds the name, n, in the tree or else create a name.
               If a name is found then the string, n, is deallocated.
*)

PROCEDURE DoMakeKey (n: PtrToChar; higha: CARDINAL) : Name ;
VAR
   result: Comparison ;
   father,
   child : NameNode ;
   k     : Name ;
BEGIN
   result := FindNodeAndParentInTree(n, child, father) ;
   IF child=NIL
   THEN
      IF result=less
      THEN
         NEW(child) ;
         father^.Left := child
      ELSIF result=greater
      THEN
         NEW(child) ;
         father^.Right := child
      END ;
      WITH child^ DO
         Right := NIL ;
         Left := NIL ;
         INC(LastIndice) ;
         Key := LastIndice ;
         Data := n ;
         PutIndice(KeyIndex, Key, n)
      END ;
      k := LastIndice
   ELSE
      DEALLOCATE(n, higha+1) ;
      k := child^.Key
   END ;
   RETURN( k )
END DoMakeKey ;


(*
   MakeKey - returns the Key of the symbol, a. If a is not in the
             name table then it is added, otherwise the Key of a is returned
             directly. Note that the name table has no scope - it merely
             presents a more convienient way of expressing strings. By a Key.
*)

PROCEDURE MakeKey (a: ARRAY OF CHAR) : Name ;
VAR
   n, p  : PtrToChar ;
   i,
   higha : CARDINAL ;
BEGIN
   higha := StrLen(a) ;
   ALLOCATE(p, higha+1) ;
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

      RETURN( DoMakeKey(n, higha) )
   END
END MakeKey ;


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
   p, pa : PtrToChar ;
   i,
   higha : CARDINAL ;
BEGIN
   IF a=NIL
   THEN
      RETURN( NulName )
   ELSE
      higha := strlen(a) ;
      ALLOCATE(p, higha+1) ;
      IF p=NIL
      THEN
         HALT      (* out of memory error *)
      ELSE
         n  := p ;
         pa := a ;
         i  := 0 ;
         WHILE i<higha DO
            p^ := pa^ ;
            INC(i) ;
            INC(p) ;
            INC(pa)
         END ;
         p^ := nul ;

         RETURN( DoMakeKey(n, higha) )
      END
   END
END makekey ;


(*
   LengthKey - returns the StrLen of Key.
*)

PROCEDURE LengthKey (Key: Name) : CARDINAL ;
VAR
   i: CARDINAL ;
   p: PtrToChar ;
BEGIN
   p := KeyToCharStar(Key) ;
   i := 0 ;
   WHILE p^#nul DO
      INC(i) ;
      INC(p)
   END ;
   RETURN( i )
END LengthKey ;


(*
   Compare - return the result of Names[i] with Names[j]
*)

PROCEDURE Compare (pi: PtrToChar; j: Name) : Comparison ;
VAR
   pj: PtrToChar ;
   c1, c2: CHAR ;
BEGIN
   pj := KeyToCharStar(j) ;
   c1 := pi^ ;
   c2 := pj^ ;
   WHILE (c1#nul) OR (c2#nul) DO
      IF c1<c2
      THEN
         RETURN( less )
      ELSIF c1>c2
      THEN
         RETURN( greater )
      ELSE
         INC(pi) ;
         INC(pj) ;
         c1 := pi^ ;
         c2 := pj^
      END
   END ;
   RETURN( equal )
END Compare ;


(*
   FindNodeAndParentInTree - search BinaryTree for a name.
                             If this name is found in the BinaryTree then
                             child is set to this name and father is set to the node above.
                             A comparison is returned to assist adding entries into this tree.
*)

PROCEDURE FindNodeAndParentInTree (n: PtrToChar; VAR child, father: NameNode) : Comparison ;
VAR
   result: Comparison ;
BEGIN
   (* firstly set up the initial values of child and father, using sentinal node *)
   father := BinaryTree ;
   child := BinaryTree^.Left ;
   IF child=NIL
   THEN
      RETURN( less )
   ELSE
      REPEAT
         result := Compare(n, child^.Key) ;
         IF result=less
         THEN
            father := child ;
            child := child^.Left
         ELSIF result=greater
         THEN
            father := child ;
            child := child^.Right
         END
      UNTIL (child=NIL) OR (result=equal) ;
      RETURN( result )
   END
END FindNodeAndParentInTree ;


(*
   IsSameExcludingCase - returns TRUE if key1 and key2 are
                         the same. It is case insensitive.
                         This function deliberately inlines CAP for speed.
*)

PROCEDURE IsSameExcludingCase (key1, key2: Name) : BOOLEAN ;
VAR
   pi, pj: PtrToChar ;
   c1, c2: CHAR ;
BEGIN
   IF key1=key2
   THEN
      RETURN( TRUE )
   ELSE
      pi := KeyToCharStar(key1) ;
      pj := KeyToCharStar(key2) ;
      c1 := pi^ ;
      c2 := pj^ ;
      WHILE (c1#nul) AND (c2#nul) DO
         IF (c1=c2) OR
            (((c1>='A') AND (c1<='Z')) AND (c2=CHR(ORD(c1)-ORD('A')+ORD('a')))) OR
            (((c2>='A') AND (c2<='Z')) AND (c1=CHR(ORD(c2)-ORD('A')+ORD('a'))))
         THEN
            INC(pi) ;
            INC(pj) ;
            c1 := pi^ ;
            c2 := pj^
         ELSE
            (* difference found *)
            RETURN( FALSE )
         END
      END ;
      RETURN( c1=c2 )
   END
END IsSameExcludingCase ;


(*
   KeyToCharStar - returns the C char * string equivalent for, key.
*)

PROCEDURE KeyToCharStar (key: Name) : ADDRESS ;
BEGIN
   IF (key=NulName) OR (NOT InBounds(KeyIndex, key))
   THEN
      RETURN( NIL )
   ELSE
      RETURN( GetIndice(KeyIndex, key) )
   END
END KeyToCharStar ;


PROCEDURE WriteKey (key: Name) ;
VAR
   s: PtrToChar ;
BEGIN
   s := KeyToCharStar(key) ;
   WHILE (s#NIL) AND (s^#nul) DO
      Write(s^) ;
      INC(s)
   END
END WriteKey ;


(*
   CharKey - returns the key[i] character.
*)

PROCEDURE CharKey (key: Name; i: CARDINAL) : CHAR ;
VAR
   p: PtrToChar ;
BEGIN
   IF i >= LengthKey (key)
   THEN
      HALT
   END ;
   p := KeyToCharStar (key) ;
   INC (p, i) ;
   RETURN p^
END CharKey ;


BEGIN
   LastIndice := 0 ;
   KeyIndex := InitIndex(1) ;
   NEW(BinaryTree) ;
   BinaryTree^.Left := NIL
END NameKey.
