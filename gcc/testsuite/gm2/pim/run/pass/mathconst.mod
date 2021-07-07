(* Copyright (C) 2008 Free Software Foundation, Inc. *)
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

MODULE mathconst ;

FROM libc IMPORT printf, exit ;

PROCEDURE func1 (r: REAL) ;
BEGIN
   printf("value passed into function was %g\n", r) ;
   IF r#130.0
   THEN
      exit(1)
   END
END func1 ;

PROCEDURE func2 (r: REAL) ;
BEGIN
   printf("value passed into function was %g\n", r) ;
   IF r#7.0
   THEN
      exit(1)
   END
END func2 ;

PROCEDURE func3 (r: REAL) ;
BEGIN
   printf("value passed into function was %g\n", r) ;
   IF r#123.0
   THEN
      exit(1)
   END
END func3 ;

VAR
   x: REAL ;
BEGIN
   x := 123.0 ;
   func1(7.0+x) ;
   func2(7.0) ;
   func3(x)
END mathconst.
