(* testfp.mod basic floating point test.

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

MODULE testfp ;

FROM Builtins IMPORT atan2, sin ;
FROM FpuIO IMPORT WriteLongReal ;
FROM StrIO IMPORT WriteString, WriteLn ;

VAR
   x: LONGREAL ;
BEGIN
   x := atan2 (0.4, 1.0) ;
   WriteLongReal (x, 20, 10) ; WriteLn ;
   x := sin (0.4) ;
   WriteLongReal (x, 20, 10) ; WriteLn
END testfp.
