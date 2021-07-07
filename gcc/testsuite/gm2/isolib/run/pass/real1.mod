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

MODULE real1 ;


FROM StringConvert IMPORT LongrealToString, ToSigFig ;
FROM SFIO IMPORT WriteS ;
FROM FIO IMPORT StdOut ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM DynamicStrings IMPORT String, InitString ;


VAR
   i: CARDINAL ;
   s: String ;
BEGIN
   FOR i := 2 TO 30 DO
      WriteString('correct to ') ; WriteCard(i, 2) ;
      WriteString(' ') ;
      s := WriteS(StdOut, ToSigFig(InitString('1.23456789012345678901234567890'), i)) ; WriteLn
   END ;
   s := WriteS(StdOut, LongrealToString(1.23456789012345678901234567890, 0, 0)) ;
   WriteLn ;
   FOR i := 2 TO 30 DO
      WriteString('pi correct to ') ; WriteCard(i, 2) ;
      WriteString(' ') ;
      s := WriteS(StdOut, ToSigFig(InitString('3.14159265358979323846264338327950288419716939937510'), i)) ; WriteLn
   END ;
   WriteString('now printing pi  3.1415926535897932384626433832795028841971693993751') ; WriteLn ;
   WriteString('longreal printed ') ;
   s := WriteS(StdOut, LongrealToString(3.1415926535897932384626433832795028841971693993751, 0, 0)) ; WriteLn
END real1.
