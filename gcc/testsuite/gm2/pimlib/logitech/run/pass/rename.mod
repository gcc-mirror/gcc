(* rename.mod a tiny test program to test the ability to rename a file.

Copyright (C) 2019 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaius.mulley@southwales.ac.uk>.

This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GNU Modula-2 is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Modula-2; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  *)

MODULE rename ;

FROM FileSystem IMPORT File, Response, Lookup, Rename, Close, SetWrite ;
IMPORT FIO ;

VAR
   f, s: File ;
BEGIN
   Lookup (f, "first.txt", TRUE) ;
   IF f.res = done
   THEN
      SetWrite (f) ;
      FIO.WriteString (f.fio, "hello world") ;
      FIO.WriteLine (f.fio) ;
      Close (f) ;
      Lookup (f, "first.txt", FALSE) ;
      Rename (f, 'second.txt') ;
      Lookup (s, "second.txt", FALSE) ;
      IF s.res = done
      THEN
         HALT (0)
      END
   ELSE
      HALT
   END
END rename.
