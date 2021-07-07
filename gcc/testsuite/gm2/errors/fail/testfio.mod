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

MODULE testfio ;

FROM Args IMPORT GetArg ;
FROM StdIO IMPORT Write ;
FROM StrIO IMPORT WriteString, WriteLn ;
FROM FIO IMPORT Exists, OpenToRead, Close, File, IsNoError, EOF, ReadChar ;

VAR
   i: INTEGER ;
   f: File ;
   c: CARDINAL ;
   a: ARRAY [0..20] OF CHAR ;
BEGIN
   WriteString('testfio starting') ; WriteLn ;
   c := 1 ;
   WHILE GetArg(a, c) DO
      WriteString('File: ') ; WriteString(a) ;
      IF Exists(a)
      THEN
         WriteString(' exists') ; WriteLn ;
         f := OpenToRead(a) ;
         WHILE NOT EOF(i) DO
            Write(ReadChar(f))
         END ;
         Close(f)
      ELSE
         WriteString(' not found: ') ; WriteString(a) ; WriteLn
      END ;
      INC(c)
   END
END testfio.
