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

MODULE testcse5 ;

FROM StrIO IMPORT WriteString, WriteLn ;
FROM StrLib IMPORT StrLen, StrCopy ;

CONST
   nul = 0C ;


PROCEDURE StrEqual (a, b: ARRAY OF CHAR) : BOOLEAN ;
VAR
   i,
   Higha,
   Highb: CARDINAL ;
   Equal: BOOLEAN ;
BEGIN
   Higha := StrLen( a ) ;
   Highb := StrLen( b ) ;
   IF Higha=Highb
   THEN
      Equal := TRUE ;
      i := 0 ;
      WHILE Equal AND (i<Higha) DO
         Equal := (a[i]=b[i]) ;
         INC( i )
      END
   ELSE
      Equal := FALSE
   END ;
   RETURN( Equal )
END StrEqual ;

(*
PROCEDURE StrLen (a: ARRAY OF CHAR) : CARDINAL ;
VAR
   High,
   Len : CARDINAL ;
BEGIN
   Len := 0 ;
   High := HIGH( a ) ;
   WHILE (Len<=High) AND (a[Len]#nul) DO
      INC( Len ) ;
   END ;
   RETURN( Len )
END StrLen ;


PROCEDURE StrCopy (a: ARRAY OF CHAR ; VAR b: ARRAY OF CHAR) ;
VAR
   Higha,
   Highb,
   n    : CARDINAL ;
BEGIN
   n := 0 ;
   Higha := StrLen( a ) ;
   Highb := HIGH( b ) ;
   WHILE (n<Higha) AND (n<=Highb) DO
      b[n] := a[n] ;
      INC( n )
   END ;
   IF n<=Highb
   THEN
      b[n] := nul
   END
END StrCopy ;
*)

VAR
   a: ARRAY [0..10] OF CHAR ;
BEGIN
   StrCopy('hello', a) ;
   IF StrEqual(a, 'hello')
   THEN
      WriteString('works')
   END ;
   WriteLn
END testcse5.
