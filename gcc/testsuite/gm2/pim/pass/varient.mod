(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE varient;

FROM libc IMPORT exit, printf ;

TYPE
  DataType = (card, other);
  RcdType = RECORD
              CASE Data : DataType OF
                card  :  j : CARDINAL;
                         k : CARDINAL |
                other : st : CHAR ;
              END
            END ;
      
VAR
  R1 : RcdType;
  r  : INTEGER ;
BEGIN
   WITH R1 DO
      Data := card;
      j := 123;
      k := 456;
      r := printf('j = %d and k = %d\n', j, k)
   END ;
   IF (R1.j#123) OR (R1.k#456)
   THEN
      exit(1)
   END
END varient.
