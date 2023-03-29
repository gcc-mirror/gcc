(* Copyright (C) 2009 Free Software Foundation, Inc. *)
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

MODULE proccast ;

FROM SYSTEM IMPORT ADDRESS, ADR, CAST ;
FROM libc IMPORT exit ;

PROCEDURE foo ;
BEGIN
END foo ;


PROCEDURE GetProcAdr (a: ADDRESS; name: ARRAY OF CHAR) : PROC ;
VAR
   p: PROC ;
BEGIN
   p := foo ;
   RETURN( p )
END GetProcAdr ;


TYPE
   INITPROC = PROCEDURE (INTEGER) ;
VAR
   initproc: INITPROC ;
BEGIN
   initproc := CAST(INITPROC, GetProcAdr(ADR(foo), 'testing')) ;
   IF CAST(ADDRESS,initproc)=NIL
   THEN
      exit(1)
   END
END proccast.
