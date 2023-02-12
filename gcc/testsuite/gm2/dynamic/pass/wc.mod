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
MODULE wc ;


FROM StdIO IMPORT Read ;
FROM NumberIO IMPORT WriteCard ;
FROM StrIO IMPORT WriteLn ;
FROM ASCII IMPORT cr ;

CONST
   Max = 1000 ;

VAR
   a    : ARRAY [0..Max] OF CHAR ;
   words,
   word : CARDINAL ;
   chars: CARDINAL ;
   ch   : CHAR ;
BEGIN
   word := 0 ;
   words := 0 ;
   chars := 0 ;
   Read(ch) ;
   WHILE ch#cr DO
      INC(chars) ;
      a[chars] := ch ;
      IF ch#' '
      THEN
         IF word=0
         THEN
            word := 1 ;
            INC(words)
         END
      ELSE
         word := 0
      END ;
      Read(ch)
   END ;
   WriteLn ;
   WriteCard(words, 4) ;
   WriteLn ;
   WriteCard(chars, 4) ;
   WriteLn
END wc.
