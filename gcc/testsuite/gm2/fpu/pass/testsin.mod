(* testsin.mod basic floating point test.

Copyright (C) 2001-2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

MODULE testsin ;

FROM MathLib0 IMPORT pi, sin, cos ;
FROM FpuIO IMPORT WriteReal, WriteLongReal ;
FROM StrIO IMPORT WriteString, WriteLn ;


VAR
   c,
   b,
   a: LONGREAL ;
BEGIN
   c := pi ;
   a := sin(pi/3.0) ;
   b := cos(pi/3.0) ;
   WriteLongReal(a, 70, 68) ; WriteLn ;
   WriteLongReal(b, 70, 68) ; WriteLn ;
END testsin.
