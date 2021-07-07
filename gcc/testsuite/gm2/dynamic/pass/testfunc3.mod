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
MODULE testfunc3 ;



PROCEDURE WriteLn ;
BEGIN
END WriteLn ;

PROCEDURE StartOfIndexInBuffer () : CARDINAL ;
BEGIN
   RETURN( 0 )
END StartOfIndexInBuffer ;

PROCEDURE FindNextLineInBuffer (a, b: CARDINAL) ;
BEGIN
END FindNextLineInBuffer ;

PROCEDURE DisplayUpToToken (a, b: CARDINAL) : CARDINAL ;
BEGIN
   RETURN( a )
END DisplayUpToToken ;

PROCEDURE WriteNChars (ch: CHAR; a: CARDINAL) ;
BEGIN
END WriteNChars ;

PROCEDURE DisplayRestOfBuffer (a, b: CARDINAL) ;
BEGIN
END DisplayRestOfBuffer ;


VAR
   BufferOffset,
   TokenLength,
   Indent      : CARDINAL ;


(*
   WriteBuffer - writes the buffer with the CurrentSymbol underlined.
*)

PROCEDURE WriteBuffer ;
VAR
   OffsetIntoSource,
   OffsetIntoBuffer,
   Indent          : CARDINAL ;
BEGIN
   OffsetIntoBuffer := StartOfIndexInBuffer() ;
   OffsetIntoSource := BufferOffset ;
   FindNextLineInBuffer(OffsetIntoBuffer, OffsetIntoSource) ;

   (* Found end of first line, now display BufferSource *)

   Indent := DisplayUpToToken(OffsetIntoBuffer, OffsetIntoSource) ;
   WriteNChars(' ', Indent) ;
   WriteNChars('^', TokenLength) ;
   WriteLn ;
   DisplayRestOfBuffer(OffsetIntoBuffer, OffsetIntoSource) ;
   WriteLn
END WriteBuffer ;


BEGIN
   WriteBuffer
END testfunc3.
