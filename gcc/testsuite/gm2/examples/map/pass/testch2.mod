(* Copyright (C) 2005, 2006, 2007, 2008, 2009, 2010
                 Free Software Foundation, Inc. *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA. *)
MODULE testch2 ;

FROM NumberIO IMPORT WriteCard ;
FROM Chance IMPORT InitRandom, KillRandom, GetAndDeleteRandom, AddRandom ;
FROM StrIO IMPORT WriteLn ;

VAR
   Index: CARDINAL ;
   i,  j: CARDINAL ;
BEGIN
   FOR i := 1 TO 15 DO
      Index := InitRandom() ;
      AddRandom(Index, 5) ;
      FOR j := 1 TO 6 DO
         WriteCard(GetAndDeleteRandom(Index), 1)
      END ;
      WriteLn ;
      KillRandom(Index)
   END
END testch2.
