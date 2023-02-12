(* Copyright (C) 2001, 2002, 2003, 2004, 2005, 2006, 2007, 2008
   Free Software Foundation, Inc. *)
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

MODULE testmin ;


FROM SYSTEM IMPORT TSIZE ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard, WriteInt ;

TYPE
   barfoo      = CARDINAL ;
   foobar      = barfoo ;
   ColourCodes = (black, brown, red, orange) ;

VAR
   i: INTEGER ;
   b: BOOLEAN ;
   c: CARDINAL ;
   s: BITSET ;
   t: ColourCodes ;
BEGIN
   WriteString('INTEGER  ') ; WriteCard(TSIZE(INTEGER), 8) ; WriteLn ;
   WriteString('INTEGER  ') ; WriteInt(MAX(INTEGER), 12) ; WriteInt(MIN(INTEGER), 12) ; WriteLn ;
   WriteString('CARDINAL ') ; WriteCard(MAX(CARDINAL), 12) ; WriteCard(MIN(CARDINAL), 12) ; WriteLn ;
   WriteString('BOOLEAN  ') ; WriteCard(VAL(CARDINAL, MAX(BOOLEAN)), 12) ; WriteCard(VAL(CARDINAL, MIN(BOOLEAN)), 12) ; WriteLn ;
   WriteString('BITSET   ') ; WriteCard(MAX(BITSET), 12) ; WriteCard(MIN(BITSET), 12) ; WriteLn ;
   FOR t := MIN(ColourCodes) TO MAX(ColourCodes) DO

   END
END testmin.
