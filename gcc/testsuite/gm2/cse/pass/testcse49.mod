(* Copyright (C) 2005 Free Software Foundation, Inc. *)
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

MODULE testcse49 ;

FROM StrIO IMPORT WriteLn ;
FROM SYSTEM IMPORT ADR, ADDRESS ;

TYPE
   string = ARRAY [0..10] OF CHAR ;
   STRING = RECORD
               c: POINTER TO string ;
               h: CARDINAL ;
            END ;

PROCEDURE StrLen (a: STRING) : CARDINAL ;
BEGIN
   a.c^[4] := 'a' ;
   RETURN 5
END StrLen ;


PROCEDURE foo ;
VAR
   t: STRING ;
   b: string ;
BEGIN
   b := 'hello' ;
   t.c := ADR(b) ;
   t.h := 5 ;
   IF StrLen(t)=5
   THEN
      WriteLn
   END
END foo ;

BEGIN
   foo
END testcse49.
