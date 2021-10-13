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

MODULE TestLong4 ;

FROM StrIO IMPORT WriteLn, WriteString;
FROM FpuIO IMPORT WriteLongInt;
FROM M2RTS IMPORT ExitOnHalt ;

VAR
  LongIntegerVariable : LONGINT;

BEGIN
   LongIntegerVariable := -12345678901234;
   WriteString('we should see the value -12345678901234 appear here: ') ;
   WriteLongInt(LongIntegerVariable,0);
   WriteLn ;
   IF LongIntegerVariable=-12345678901234
   THEN
      WriteString(' correct result') ; WriteLn
   ELSE
      WriteString(' incorrect result') ; WriteLn ;
      ExitOnHalt(1) ;
      HALT
   END 
END TestLong4.
