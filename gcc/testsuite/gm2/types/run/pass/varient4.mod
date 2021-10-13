(* Copyright (C) 2003, 2004, 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE varient4 ;

FROM d IMPORT this, test ;
FROM libc IMPORT memset ;
FROM SYSTEM IMPORT ADDRESS, TSIZE, ADR ;

PROCEDURE zero ;
BEGIN
   IF memset(ADR(hmm), 0, TSIZE(this))=NIL
   THEN
   END
END zero ;

VAR
   hmm: this ;
BEGIN
   zero ; hmm.tag   := 99   ; test(hmm, 1, 99) ;
   zero ; hmm.foo   := 77   ; test(hmm, 2, 77) ;
   zero ; hmm.bar   := TRUE ; test(hmm, 3, 1) ;
   zero ; hmm.bt    := 66 ; test(hmm, 4, 66) ;
   zero ; hmm.bf    := 55 ; test(hmm, 5, 55) ;
   zero ; hmm.an    := 44 ; test(hmm, 6, 44) ;
   zero ; hmm.final := 33 ; test(hmm, 7, 33)
END varient4.
