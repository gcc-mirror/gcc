(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

MODULE constructor3 ;

FROM libc IMPORT exit ;


VAR
   f: position ;

TYPE
   position = RECORD
                 x1, y1, x2, y2: CARDINAL ;
              END ;

CONST
   first = position{1,2,3,4} ;

BEGIN
   f := first ;
   IF (f.x1=1) AND (f.y1=2) AND (f.x2=3) AND (f.y2=4)
   THEN
      (* all ok *)
   ELSE
      exit(1)
   END
END constructor3.
