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

MODULE stringreal2 ;

FROM DynamicStrings IMPORT String, EqualArray, KillString, InitString ;
FROM ConvStringReal IMPORT RealToFixedString ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM FIO IMPORT StdOut ;
FROM SFIO IMPORT WriteS ;

CONST
   maxString = 80 ;

VAR
   i: CARDINAL ;
   s: String ;
   f: REAL ;
BEGIN
   FOR i := 3 TO 10 DO
      f := 3.141592653589793 ;
      s := RealToFixedString(f, i) ;
      s := WriteS(StdOut, s) ;
      WriteLn ;
      s := KillString(s)
   END
END stringreal2.
