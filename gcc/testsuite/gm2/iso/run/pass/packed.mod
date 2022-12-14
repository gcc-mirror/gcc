(* Copyright (C) 2009 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)

MODULE packed ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT CAST, SHIFT ;


PROCEDURE Assert (b: BOOLEAN; file: ARRAY OF CHAR; line: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf("%s:%d: assert failed\n", file, line)
   END
END Assert ;


(*
   tests based on ISO standard, note 4 on Page 361 
*)

PROCEDURE test ;
VAR
   v: CARDINAL ;
BEGIN
   Assert(CAST(CARDINAL, BITSET{0}) = VAL(CARDINAL, 1), __FILE__, __LINE__) ;
   v := MAX(CARDINAL)-1 ;
   WHILE v>0 DO
      Assert(CAST(CARDINAL, SHIFT(CAST(BITSET, v), -1)) = v DIV 2, __FILE__, __LINE__) ;
      v := v DIV 2
   END ;
   v := MAX(CARDINAL) DIV 2 ;
   WHILE v>0 DO
      Assert(CAST(CARDINAL, SHIFT(CAST(BITSET, v), 1)) = v*2, __FILE__, __LINE__) ;
      v := v DIV 2
   END
END test ;


BEGIN
   test
END packed.
