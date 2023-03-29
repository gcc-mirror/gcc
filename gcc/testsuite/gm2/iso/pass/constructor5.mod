(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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

MODULE constructor5 ;

VAR
   second: rec ;

CONST
   first = rec{green, position{2,3,4,5}} ;

TYPE
   rec = RECORD
            col: colours;
            pos: position ;
         END ;

   colours = (red, green, blue) ;

   position = RECORD
                 x1, y1, x2, y2: CARDINAL ;
              END ;

BEGIN
   second := first
END constructor5.
