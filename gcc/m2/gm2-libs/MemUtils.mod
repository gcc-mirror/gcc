(* MemUtils.mod provides some basic memory utilities.

Copyright (C) 2001-2025 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE MemUtils ;


FROM SYSTEM IMPORT WORD, BYTE, TSIZE ;


(*
   MemCopy - copys a region of memory to the required destination.
*)

PROCEDURE MemCopy (from: ADDRESS; length: CARDINAL; to: ADDRESS) ;
VAR
   pwb, pwa: POINTER TO WORD ;
   pbb, pba: POINTER TO BYTE ;
BEGIN
   WHILE length>=TSIZE(WORD) DO
      pwa := from ;
      pwb := to ;
      pwb^ := pwa^ ;
      INC(from  , TSIZE(WORD)) ;
      INC(to    , TSIZE(WORD)) ;
      DEC(length, TSIZE(WORD))
   END ;
   WHILE length>0 DO
      pba := from ;
      pbb := to ;
      pbb^ := pba^ ;
      INC(from  , TSIZE(BYTE)) ;
      INC(to    , TSIZE(BYTE)) ;
      DEC(length, TSIZE(BYTE))
   END
END MemCopy ;


(*
   MemZero - sets a region of memory: a..a+length to zero.
*)

PROCEDURE MemZero (a: ADDRESS; length: CARDINAL) ;
VAR
   pwa: POINTER TO WORD ;
   pba: POINTER TO BYTE ;
BEGIN
   pwa := a ;
   WHILE length>=TSIZE(WORD) DO
      pwa^ := WORD(0) ;
      INC(pwa, TSIZE(WORD)) ;
      DEC(length, TSIZE(WORD))
   END ;
   pba := ADDRESS(pwa) ;
   WHILE length>=TSIZE(BYTE) DO
      pba^ := BYTE(0) ;
      INC(pba, TSIZE(BYTE)) ;
      DEC(length, TSIZE(BYTE))
   END
END MemZero ;


END MemUtils.
