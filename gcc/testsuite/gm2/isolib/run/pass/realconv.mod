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

MODULE realconv ;

FROM RealConv IMPORT LengthFloatReal, LengthEngReal, LengthFixedReal ;
FROM M2RTS IMPORT Halt ;


PROCEDURE Assert (b: BOOLEAN; l: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      Halt(__FILE__, l, __FUNCTION__, "assert failed")
   END
END Assert ;


BEGIN
   (* LengthFixedReal *)

   Assert(LengthFixedReal(12.3456789, 3)=6, __LINE__) ;  (* 12.345 *)
   Assert(LengthFixedReal(123.456789, 3)=7, __LINE__) ;  (* 123.456 *)
   Assert(LengthFixedReal(1234.56789, 3)=8, __LINE__) ;  (* 1234.567 *)
   Assert(LengthFixedReal(1234.56789, -3)=4, __LINE__) ; (* 1000 *)

   (* LengthEngReal *)

   Assert(LengthEngReal(12.3456789, 3)=4, __LINE__) ;   (* 12.3 *)
   Assert(LengthEngReal(123.456789, 3)=3, __LINE__) ;   (* 123 *)
   Assert(LengthEngReal(1234.56789, 3)=7, __LINE__) ;   (* 1.23E+3 *)
   Assert(LengthEngReal(12345.6789, 3)=7, __LINE__) ;   (* 12.3E+3 *)

   (* LengthFloatReal *)

   Assert(LengthFloatReal(1234.56789, 3)=7, __LINE__) ;   (* 1.23E+3 *)

END realconv.
