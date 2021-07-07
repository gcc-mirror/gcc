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
MODULE testcoords ;


FROM StrIO IMPORT WriteString, WriteLn ;
FROM NumberIO IMPORT WriteCard ;
FROM StoreCoords IMPORT InitCoords, KillCoords,
                        AddCoord, GetAndDeleteRandomCoord ;

VAR
   Index: CARDINAL ;
   i, j : CARDINAL ;
   x, y : CARDINAL ;
BEGIN
   FOR i := 1 TO 10 DO
      Index := InitCoords() ;
      WriteString('Index:') ; WriteCard(Index, 4) ; WriteString('Coords') ;
      FOR j := 1 TO 5 DO
         AddCoord(Index, j, j)
      END ;
      FOR j := 1 TO 6 DO
         GetAndDeleteRandomCoord(Index, x, y) ;
         WriteCard(x, 4) ; WriteCard(y, 2)
      END ;
      WriteLn ;
      KillCoords(Index)
   END
END testcoords.
