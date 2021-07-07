(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE testcse33 ;


CONST
   MaxBoxes =  500 ;
   MaxX     =  120 ;  (* 38 ; *)
   MaxY     =   80 ;  (* 24 ; *)

TYPE
   Box    = RECORD
               x1, y1,
               x2, y2   : CARDINAL ;
               RoomOfBox: CARDINAL ;
            END ;

VAR
   Boxes    : ARRAY [0..MaxBoxes] OF Box ;
   NoOfBoxes: CARDINAL ;


PROCEDURE Init ;
BEGIN
   NoOfBoxes := 0 ;
   (* Initialize box 0 the edge of the map *)
   WITH Boxes[0] DO
      x1 := 1 ;
      x2 := MaxX ;
      y1 := 1 ;
      y2 := MaxY
   END
END Init ;

BEGIN
   Init
END testcse33.
