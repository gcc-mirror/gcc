(* Sets.mod provides a dynamic set module.

Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE Sets ;

FROM SYSTEM IMPORT ADDRESS, BYTE ;
FROM SymbolTable IMPORT FinalSymbol ;
FROM M2Error IMPORT InternalError ;
FROM Storage IMPORT ALLOCATE, REALLOCATE, DEALLOCATE ;
FROM libc IMPORT memset, memcpy ;
FROM M2Printf IMPORT printf0, printf1, printf2 ;
FROM Assertion IMPORT Assert ;


CONST
   BitsetSize  = SIZE (BITSET) ;
   MaxBitset   = MAX (BITSET) ;
   BitsPerByte = (MaxBitset + 1) DIV BitsetSize ;
   Debugging   = FALSE ;

TYPE
   PtrToByte   = POINTER TO BYTE ;
   PtrToBitset = POINTER TO BITSET ;
   Set = POINTER TO RECORD
                       init,
                       start,
                       end     : CARDINAL ;
                       pb      : PtrToBitset ;
                       bytes   : CARDINAL ;
                       elements: CARDINAL ;
                    END ;


(*
   growSet -
*)

PROCEDURE growSet (i: CARDINAL; bytes: CARDINAL) ;
BEGIN
   printf2("i = %d,  bytes = %d\n", i, bytes)
END growSet ;


(*
   checkRange - checks to make sure, i, is within range and
                it will extend the set bitmap if required.
*)

PROCEDURE checkRange (s: Set; i: CARDINAL) ;
VAR
   bits,
   o, j: CARDINAL ;
   b   : PtrToBitset ;
   v   : PtrToByte ;
BEGIN
   WITH s^ DO
      IF i<init
      THEN
         InternalError ('set element is too low and out of bounds')
      ELSIF i>FinalSymbol()
      THEN
         InternalError ('set element is too high and out of bounds')
      ELSE
         j := bytes * BitsPerByte ;
         IF i>=j
         THEN
            o := bytes ;
            IF Debugging
            THEN
               printf2("previous bitset size %d bytes, need %d bits\n",
                       o, i)
            END ;
            IF bytes=0
            THEN
               bytes := BitsetSize
            END ;
            WHILE i >= bytes*BitsPerByte DO
               IF Debugging
               THEN
                  growSet(i, bytes)
               END ;
               bytes := bytes * 2
            END ;
            ALLOCATE(b, bytes) ;
            IF Debugging
            THEN
               bits := bytes*8 ;
               printf2("new allocated bitset size %d bytes, holds %d bits\n", bytes, bits) ;
               IF i>bits
               THEN
                  InternalError ('buffer is too small')
               END
            END ;
            (* a := memset(b, 0, bytes) ; *)
            v := PtrToByte(b) ;
            INC(v, o) ;
            Assert (memset (v, 0, bytes-o) = v) ;
            Assert (memcpy (b, pb, o) = b) ;
            IF Debugging
            THEN
               printf1("deallocating old bitset size %d bytes\n", o)
            END ;
            IF o>0
            THEN
               DEALLOCATE(pb, o)
            END ;
            pb := b
         END
      END
   END
END checkRange ;


(*
   findPos - returns a pointer to the BITSET which will contain, i.
*)

PROCEDURE findPos (pb: PtrToBitset; i: CARDINAL) : PtrToBitset ;
VAR
   v: PtrToByte ;
BEGIN
   IF (((i DIV (MaxBitset+1)) * (MaxBitset+1)) DIV BitsPerByte) MOD BitsetSize#0
   THEN
      InternalError ('must be a multiple of bitset size')
   END ;
   v := PtrToByte(pb) ;
   INC(v, ((i DIV (MaxBitset+1)) * (MaxBitset+1)) DIV BitsPerByte) ;
   pb := PtrToBitset(v) ;
   RETURN( pb )
END findPos ;


(*
   InitSet - initializes and returns a set.  The set will
             never contain an element less than, low.
*)

PROCEDURE InitSet (low: CARDINAL) : Set ;
VAR
   s: Set ;
BEGIN
   NEW(s) ;
   WITH s^ DO
      init     := low ;
      start    := 0 ;
      end      := 0 ;
      pb       := NIL ;
      bytes    := 0 ;
      elements := 0
   END ;
   RETURN( s )
END InitSet ;


(*
   KillSet - deallocates Set, s.
*)

PROCEDURE KillSet (s: Set) : Set ;
BEGIN
   WITH s^ DO
      IF bytes>0
      THEN
         DEALLOCATE(pb, bytes)
      END
   END ;
   DISPOSE(s) ;
   RETURN( NIL )
END KillSet ;


(*
   DuplicateSet - returns a new duplicated set.
*)

PROCEDURE DuplicateSet (s: Set) : Set ;
VAR
   t: Set ;
BEGIN
   NEW(t) ;
   t^ := s^ ;
   WITH t^ DO
      ALLOCATE(pb, bytes) ;
      Assert (memcpy (pb, s^.pb, bytes) = pb)
   END ;
   RETURN( t )
END DuplicateSet ;


(*
   ForeachElementInSetDo - for each element e in, s, call, p(e).
*)

PROCEDURE ForeachElementInSetDo (s: Set; p: PerformOperation) ;
VAR
   i, j, c: CARDINAL ;
   b      : PtrToBitset ;
   v      : PtrToByte ;
BEGIN
   WITH s^ DO
      i := start ;
      c := elements ;
      b := findPos(pb, i) ;
      j := i MOD (MaxBitset+1) ;
      WHILE (i<=end) AND (c>0) DO
         IF j IN b^
         THEN
            DEC(c) ;
            p(i)
         END ;
         IF j=MaxBitset
         THEN
            v := PtrToByte(b) ;
            INC(v, BitsetSize) ;   (* avoid implications of C address arithmetic in mc PtrToByte *)
            b := PtrToBitset(v) ;
            j := 0
         ELSE
            INC(j)
         END ;
         INC(i)
      END
   END
END ForeachElementInSetDo ;


(*
   IsElementInSet - returns TRUE if element, i, is in set, s.
*)

PROCEDURE IsElementInSet (s: Set; i: CARDINAL) : BOOLEAN ;
VAR
   b: PtrToBitset ;
BEGIN
   checkRange(s, i) ;
   WITH s^ DO
      b := findPos(pb, i) ;
      RETURN( (i MOD (MaxBitset+1)) IN b^ )
   END
END IsElementInSet ;


(*
   NoOfElementsInSet - returns the number of elements in a set, s.
*)

PROCEDURE NoOfElementsInSet (s: Set) : CARDINAL ;
BEGIN
   RETURN( s^.elements )
END NoOfElementsInSet ;


(*
   ExcludeElementFromSet - excludes element, i, from set, s.
*)

PROCEDURE ExcludeElementFromSet (s: Set; i: CARDINAL) ;
VAR
   b: PtrToBitset ;
BEGIN
   checkRange(s, i) ;
   WITH s^ DO
      b := findPos(pb, i) ;
      IF (i MOD (MaxBitset+1)) IN b^
      THEN
         DEC(elements) ;
         EXCL(b^, i MOD (MaxBitset+1))
      END
   END
END ExcludeElementFromSet ;


(*
   IncludeElementIntoSet - includes element, i, into set, s.
*)

PROCEDURE IncludeElementIntoSet (s: Set; i: CARDINAL) ;
VAR
   b: PtrToBitset ;
BEGIN
   checkRange(s, i) ;
   WITH s^ DO
      b := findPos(pb, i) ;
      IF NOT ((i MOD (MaxBitset+1)) IN b^)
      THEN
         INC(elements) ;
         INCL(b^, i MOD (MaxBitset+1)) ;
         IF (start=0) OR (start>i)
         THEN
            start := i
         END ;
         IF (end=0) OR (end<i)
         THEN
            end := i
         END
      END
   END
END IncludeElementIntoSet ;


(*
   EqualSet - return TRUE if left = right.
*)

PROCEDURE EqualSet (left, right: Set) : BOOLEAN ;
VAR
   v   : PtrToByte ;
   lptr,
   rptr: PtrToBitset ;
   last,
   el  : CARDINAL ;
BEGIN
   IF (left^.init = right^.init) AND
      (left^.start = right^.start) AND
      (left^.end = right^.end) AND
      (left^.elements = right^.elements)
   THEN
      (* Now check contents.  *)
      el := left^.start ;
      last := left^.end ;
      WHILE el <= last DO
         lptr := findPos (left^.pb, el) ;
         rptr := findPos (right^.pb, el) ;
         IF el + BitsetSize < last
         THEN
            (* We can check complete bitset,  *)
            IF lptr^ # rptr^
            THEN
               RETURN FALSE
            END ;
            INC (el, BitsetSize) ;
            v := PtrToByte (lptr) ;
            INC (v, BitsetSize) ;   (* Avoid implications of C address arithmetic in mc PtrToByte *)
            lptr := PtrToBitset (v) ;
            v := PtrToByte (rptr) ;
            INC (v, BitsetSize) ;   (* Avoid implications of C address arithmetic in mc PtrToByte *)
            rptr := PtrToBitset (v)
         ELSE
            (* We must check remaining bits only.  *)
            WHILE (el <= last) AND (el >= left^.init) DO
               IF IsElementInSet (left, el) # IsElementInSet (right, el)
               THEN
                  RETURN FALSE
               END ;
               INC (el)
            END ;
            RETURN TRUE
         END
      END ;
      RETURN TRUE
   END ;
   RETURN FALSE
END EqualSet ;


END Sets.
