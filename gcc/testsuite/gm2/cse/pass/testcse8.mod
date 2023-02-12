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
MODULE testcse8 ;

PROCEDURE StrLen (a: ARRAY OF CHAR) : CARDINAL ;
BEGIN
   RETURN( 1 )
END StrLen ;

PROCEDURE Space (ch: CHAR) : BOOLEAN ;
BEGIN
   RETURN( TRUE )
END Space ;

PROCEDURE GetNextArg (CmdLine: ARRAY OF CHAR; VAR CmdIndex: CARDINAL;
                      VAR Arg: ARRAY OF CHAR) : BOOLEAN ;
VAR
   ArgIndex: CARDINAL ;  (* Index into Arg *)
   HighA,
   HighC: CARDINAL ;
BEGIN
   HighA := HIGH(Arg) ;
   HighC := StrLen(CmdLine) ;
   ArgIndex := 0 ;
   (* Skip spaces *)
   WHILE (CmdIndex<HighC) AND Space(CmdLine[CmdIndex]) DO
      INC(CmdIndex)
   END ;
   RETURN( TRUE )
END GetNextArg ;


VAR
   a   : ARRAY [0..20] OF CHAR ;
   i, j: CARDINAL ;
BEGIN
   IF GetNextArg(a, i, a)
   THEN
   END
END testcse8.
