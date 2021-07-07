(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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
MODULE TestLong;

FROM StrIO IMPORT WriteString, WriteLn;
FROM FpuIO IMPORT StrToLongInt, WriteLongInt, LongIntToStr;
FROM StrLib IMPORT StrEqual ;
FROM M2RTS IMPORT ExitOnHalt ;

TYPE
   String = ARRAY [0..255] OF CHAR;

VAR
   CorrectResult,
   LongIntegerVariable : LONGINT;
   SameResult,
   St                  : String;

BEGIN
   LongIntegerVariable := 12345678901234;
   WriteLongInt(LongIntegerVariable, 0);
   WriteLn;
   St := '12345678901234';
   WriteString(St);
   WriteLn;
   LongIntToStr(LongIntegerVariable, 0, SameResult) ;
   StrToLongInt(St, CorrectResult) ;
   WriteLongInt(CorrectResult, 0);
   WriteLn ;
   IF NOT StrEqual(St, SameResult)
   THEN
      WriteString('test failed: correct value is: ') ; WriteString(St) ;
      WriteString(' assignment produced ') ; WriteString(SameResult) ;
      WriteLn ;
      ExitOnHalt(1) ;
      HALT
   END
END TestLong.
