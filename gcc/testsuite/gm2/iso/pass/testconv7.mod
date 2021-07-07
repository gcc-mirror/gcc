(* Copyright (C) 2015 Free Software Foundation, Inc.  *)
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
Boston, MA 02110-1301, USA.  *)

MODULE testconv7 ;   (*!m2iso*)

TYPE
  Month    = [1..12] ;
  DateTime = RECORD
                month: Month ;
             END ;

PROCEDURE IsValidDateTime (userData: DateTime) : BOOLEAN ;
(* Tests if the value of userData is a valid *)
BEGIN
   WITH userData DO
      CASE month OF

      1:  RETURN FALSE |

      ELSE
      END
   END ;
   RETURN TRUE
END IsValidDateTime ;


VAR
   u: DateTime ;
BEGIN
   IF IsValidDateTime (u)
   THEN
   END
END testconv7.
