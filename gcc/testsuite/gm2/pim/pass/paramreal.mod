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

MODULE paramreal ;

FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteLn ;

PROCEDURE test (r: REAL; lr: LONGREAL; li: LONGINT) ;
VAR
   i: INTEGER ;
BEGIN
   lr := 5.91 ;
   li := 123456789 ;
   r  := 3.1415927 ;
   li := 1 ;
   WHILE li*10>li DO
      li := li*10 ;
      Write('.')
   END ;
   WriteLn ;
   i := 1 ;
   WHILE i*10>i DO
      i := i*10 ;
      Write('.')
   END ;
   WriteLn
END test ;

VAR
   r: REAL ;
   lr: LONGREAL ;
   li: LONGINT ;
BEGIN
   lr := 5.123456789 ;
   li := 987654321 ;
   r  := 0.123456789 ;
   test(r, lr, li)
END paramreal.
