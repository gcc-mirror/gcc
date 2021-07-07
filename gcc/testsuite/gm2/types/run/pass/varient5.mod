(* Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE varient5 ;

FROM SYSTEM IMPORT BYTE, SIZE, ADR ;
FROM libc IMPORT exit ;

TYPE
   union = RECORD
              CASE :BOOLEAN OF

              TRUE: b1, b2, b3, b4: BYTE |
              FALSE: c: CARDINAL

              END
           END ;

PROCEDURE Assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      exit(1)
   END
END Assert ;


VAR
   p: POINTER TO BYTE ;
   t: CARDINAL ;
   x: union ;
BEGIN
   IF SIZE(CARDINAL)=4
   THEN
      t := 0FEEDBEEFH ;
      p := ADR(t) ;
      IF p^=BYTE(0EFH)
      THEN
         x.c := 0FEEDBEEFH
      ELSE
         x.c := 0EFBEEDFEH
      END ;
      Assert(x.b4=BYTE(0FEH)) ;
      Assert(x.b3=BYTE(0EDH)) ;
      Assert(x.b2=BYTE(0BEH)) ;
      Assert(x.b1=BYTE(0EFH))
   END
END varient5.
