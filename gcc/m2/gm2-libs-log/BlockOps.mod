(* BlockOps.mod provides a Logitech compatible module for block moves.

Copyright (C) 2005-2023 Free Software Foundation, Inc.
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE BlockOps ;

FROM Builtins IMPORT memcpy, memmove, memset ;
FROM SYSTEM IMPORT TSIZE, BYTE, WORD ;

TYPE
   ptrToByte = POINTER TO BYTE ;
   ptrToWord = POINTER TO WORD ;


(*
   MoveBlockForward - moves, n, bytes from, src, to, dest.
                      Starts copying from src and keep copying
                      until, n, bytes have been copied.
*)

PROCEDURE BlockMoveForward (dest, src: ADDRESS; n: CARDINAL) ;
BEGIN
   IF ((src<=dest) AND (src+n>=dest)) OR
      ((src>=dest) AND (src+n<=dest))
   THEN
      dest := memmove(dest, src, n)
   ELSE
      (* no overlap, use memcpy *)
      dest := memcpy(dest, src, n)
   END
END BlockMoveForward ;


(*
   MoveBlockBackward - moves, n, bytes from, src, to, dest.
                       Starts copying from src+n and keeps copying
                       until, n, bytes have been copied.
                       The last datum to be copied will be the byte
                       at address, src.
*)

PROCEDURE BlockMoveBackward (dest, src: ADDRESS; n: CARDINAL) ;
VAR
   pbd, pbs: ptrToByte ;
BEGIN
   IF ((src<=dest) AND (src+n>=dest)) OR
      ((src>=dest) AND (src+n<=dest))
   THEN
      dest := memmove(dest, src, n)
   ELSE
      (* copy byte by byte backwards *)
      pbs := src+VAL(ADDRESS, n-TSIZE(BYTE)) ;
      pbd := dest+VAL(ADDRESS, n-TSIZE(BYTE)) ;
      WHILE n>0 DO
         pbd^ := pbs^ ;
         DEC(n) ;
         DEC(pbd) ;
         DEC(pbs)
      END
   END
END BlockMoveBackward ;


(*
   BlockClear - fills, block..block+n-1, with zero's.
*)

PROCEDURE BlockClear (block: ADDRESS; n: CARDINAL) ;
BEGIN
   block := memset(block, 0, n)
END BlockClear ;


(*
   BlockSet - fills, n, bytes starting at, block, with a pattern
              defined at address pattern..pattern+patternSize-1.
*)

PROCEDURE BlockSet (block: ADDRESS; n: CARDINAL;
                    pattern: ADDRESS; patternSize: CARDINAL) ;
VAR
   b: ADDRESS ;
BEGIN
   b := block ;
   WHILE n>0 DO
      block := memcpy(b, pattern, patternSize) ;
      INC(b, patternSize) ;
      DEC(n, patternSize)
   END ;
   IF n>0
   THEN
      block := memcpy(b, pattern, n)
   END
END BlockSet ;


(*
   BlockEqual - returns TRUE if the blocks defined, a..a+n-1, and,
                b..b+n-1 contain the same bytes.
*)

PROCEDURE BlockEqual (a, b: ADDRESS; n: CARDINAL) : BOOLEAN ;
VAR
   pwa, pwb: ptrToWord ;
   pba, pbb: ptrToByte ;
BEGIN
   pwa := a ;
   pwb := b ;
   WHILE n>=TSIZE(WORD) DO
      IF pwa^#pwb^
      THEN
         RETURN FALSE
      END ;
      INC(pwa, TSIZE(WORD)) ;
      INC(pwb, TSIZE(WORD)) ;
      DEC(n, TSIZE(WORD))
   END ;
   (* and check any remaining bytes *)
   pba := VAL(ptrToByte, pwa) ;
   pbb := VAL(ptrToByte, pwb) ;
   WHILE n>0 DO
      IF pba^#pbb^
      THEN
         RETURN FALSE
      END ;
      INC(pba) ;
      INC(pbb) ;
      DEC(n)
   END ;
   RETURN TRUE
END BlockEqual ;


(*
   BlockPosition - searches for a pattern as defined by
                   pattern..patternSize-1 in the block,
                   block..block+blockSize-1.  It returns
                   the offset from block indicating the
                   first occurence of, pattern.
                   MAX(CARDINAL) is returned if no match
                   is detected.
*)

PROCEDURE BlockPosition (block: ADDRESS; blockSize: CARDINAL;
                         pattern: ADDRESS; patternSize: CARDINAL) : CARDINAL ;
VAR
   n, o    : CARDINAL ;
   pba, pbb: ptrToByte ;
BEGIN
   o := 0 ;
   pba := block ;
   pbb := pattern ;
   WHILE blockSize>0 DO
      pbb := pattern ;
      n := patternSize ;
      WHILE n>0 DO
         IF pbb^#pba^
         THEN
            RETURN o
         END ;
         INC(pba) ;
         INC(pbb) ;
         INC(o) ;
         DEC(n)
      END
   END ;
   RETURN MAX(CARDINAL)
END BlockPosition ;


END BlockOps.
