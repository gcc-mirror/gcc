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

MODULE real3 ;

FROM RealConv IMPORT ValueReal ;
FROM M2RTS IMPORT Halt ;


PROCEDURE Assert (b: BOOLEAN; l: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      Halt(__FILE__, l, __FUNCTION__, "assert failed")
   END
END Assert ;


VAR
   r: REAL ;
BEGIN
   Assert(ValueReal('3.14')=3.14, __LINE__) ;
   Assert(ValueReal('3.14E0')=3.14E0, __LINE__) ;
   Assert(ValueReal('-3.14E4')=-3.14E4, __LINE__)
END real3.
