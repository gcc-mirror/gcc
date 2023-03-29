(* Copyright (C) 2005, 2006, 2007, 2008 Free Software Foundation, Inc. *)
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

MODULE testadd ;

FROM addition IMPORT add ;

FROM libc IMPORT exit, printf ;
FROM SYSTEM IMPORT ADR ;

VAR
   res: INTEGER ;

PROCEDURE Assert (v: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL; e: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
BEGIN
   IF v
   THEN
      r := printf("successfully evaluated assertion (%s)\n", ADR(e))
   ELSE
      r := printf("%s:%d assertion failed when evaluating %s\n", ADR(f), l, ADR(e)) ;
      res := 1
   END
END Assert ;


BEGIN
   res := 0 ;
   Assert(add(1, 2)=3, __FILE__, __LINE__, "3") ;
   Assert(add(2, 1)=3, __FILE__, __LINE__, "3") ;
   Assert(add(0, 1)=1, __FILE__, __LINE__, "1") ;
   Assert(add(0, 0)=0, __FILE__, __LINE__, "0") ;
   Assert(add(1024, 1024)=2048, __FILE__, __LINE__, "2048") ;
   Assert(add(10000, 20000)=30000, __FILE__, __LINE__, "30000") ;
   exit(res)
END testadd.
