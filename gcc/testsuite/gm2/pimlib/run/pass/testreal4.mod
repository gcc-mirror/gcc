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

MODULE testreal4 ;

FROM InOut IMPORT WriteLn, WriteString ;
FROM RealConversions IMPORT SetNoOfExponentDigits,
                            LongRealToString ;
FROM StrLib IMPORT StrEqual ;


PROCEDURE Assert (b: BOOLEAN) ;
BEGIN
   IF NOT b
   THEN
      WriteString('assertion failed') ; WriteLn ;
      r := 1
   END
END Assert ;


VAR
   s: ARRAY [0..40] OF CHAR ;
   ok: BOOLEAN ;
   r : INTEGER ;
BEGIN
   r := 0 ;
   SetNoOfExponentDigits(3) ;
   LongRealToString(0.0123456789, -8, 15, s, ok) ;
   Assert(StrEqual(s, '1.23456789E-002') AND ok) ;
   IF ok
   THEN
      WriteString(s) ; WriteLn
   END ;
   IF NOT StrEqual(s, '1.23456789E-002')
   THEN
      WriteString('expecting 1.23456789E-002 and received ') ; WriteString(s) ; WriteLn
   END ;
   LongRealToString(1.23456789, -8, 15, s, ok) ;
   Assert(StrEqual(s, '1.23456789E+000') AND ok) ;
   IF ok
   THEN
      WriteString(s) ; WriteLn
   END ;
   IF NOT StrEqual(s, '1.23456789E+000')
   THEN
      WriteString('expecting 1.23456789E+000 and received ') ; WriteString(s) ; WriteLn
   END ;
   HALT(r)
END testreal4.
