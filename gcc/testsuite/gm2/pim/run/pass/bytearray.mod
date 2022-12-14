(* Copyright (C) 2011 Free Software Foundation, Inc. *)
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
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA. *)

MODULE bytearray ;

FROM SYSTEM IMPORT BYTE ;
FROM libc IMPORT printf, exit ;

PROCEDURE bytes (b: ARRAY OF BYTE; s: CARDINAL) ;
BEGIN
   IF HIGH(b)#s-1
   THEN
      printf ("passing ARRAY OF BYTE failed, expected %d and received %d bytes\n", s, HIGH(b)) ;
      exit (1)
   END
END bytes ;

VAR
   i: INTEGER ;
   c: CARDINAL ;
   ch: CHAR ;
BEGIN
   bytes (i, SIZE(i)) ;
   bytes (c, SIZE(c)) ;
   bytes (ch, SIZE(ch)) ;
   bytes ("hello world", 12)
END bytearray.
