(* Copyright (C) 2015 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

MODULE packedrecord ;

FROM libc IMPORT printf, exit ;
FROM SYSTEM IMPORT SIZE, BYTE ;


TYPE
   Colour     = (white, black) ;
   HeapOpcode = (move, take, begin, end, add, del) ;
   PieceNo    = [0..15] ;

   Instruction = RECORD
                    <* bytealignment(0) *>
                    o: HeapOpcode ;  (* 3 bits *)
                    c: Colour ;      (* 1 bit  *)
                    n: PieceNo ;     (* 4 bits *)
                 END ;

(*
   assert - 
*)

PROCEDURE assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      printf ("assert failed\n");
      exit (1)
   END
END assert ;


VAR
   i: Instruction ;
BEGIN
   printf ("size of 'i' is %d byte(s)\n", SIZE (i)) ;
   assert (SIZE (i) = SIZE (BYTE))
END packedrecord.
