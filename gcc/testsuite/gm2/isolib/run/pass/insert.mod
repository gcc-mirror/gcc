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

MODULE insert ;

IMPORT Strings ;
IMPORT STextIO ;
FROM libc IMPORT exit, printf ;

PROCEDURE Assert (b: BOOLEAN; l: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf("\nassert failed at line %d\n", l);
      exit(1)
   END
END Assert ;


VAR
   a: ARRAY [0..15] OF CHAR;
BEGIN
   STextIO.WriteString("this is a test"); STextIO.WriteLn ;
   STextIO.WriteString("will become"); STextIO.WriteLn ;
   STextIO.WriteString("this not is a te"); STextIO.WriteLn ;
   STextIO.WriteString(' '); STextIO.WriteLn ;
   Strings.Assign("this is a test", a);
   Strings.Insert("not ", 5, a);
   STextIO.WriteString(a); STextIO.WriteLn ;
   Assert(Strings.Equal(a, "this not is a te"), __LINE__) ;
   Strings.Assign("this not is a te", a);
   Strings.Insert("not ", 5, a);
   Assert(Strings.Equal(a, "this not not is "), __LINE__) ;
   Strings.Assign("this not is a te", a) ;
   Strings.Insert("1234", 14, a) ;
   STextIO.WriteLn; STextIO.WriteString(a); STextIO.WriteLn ;
   Assert(Strings.Equal(a, "this not is a 12"), __LINE__) ;
   Strings.Assign("this not is a te", a);
   Strings.Insert("1234", 0, a) ;
   STextIO.WriteLn; STextIO.WriteString("1234this not is "); STextIO.WriteLn ;
   STextIO.WriteLn; STextIO.WriteString(a); STextIO.WriteLn ;
   Assert(Strings.Equal(a, "1234this not is "), __LINE__) ;
   Strings.Assign("0123456789012345", a);
   Strings.Insert("abcdefghijklmnopqrstuvwxyz", 0, a) ;
   STextIO.WriteLn; STextIO.WriteString("abcdefghijklmnop"); STextIO.WriteLn ;
   STextIO.WriteLn; STextIO.WriteString(a); STextIO.WriteLn ;
   Assert(Strings.Equal(a, "abcdefghijklmnop"), __LINE__) ;
   
END insert.
