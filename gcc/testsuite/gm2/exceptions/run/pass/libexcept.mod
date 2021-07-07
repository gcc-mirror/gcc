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

MODULE libexcept ;


FROM libc IMPORT exit, write, exit, printf ;
FROM ASCII IMPORT nul, nl ;
FROM SYSTEM IMPORT ADR, THROW ;
FROM M2RTS IMPORT Length ;
FROM NumberIO IMPORT CardToStr ;
FROM RTExceptions IMPORT IsInExceptionState ;

   
PROCEDURE Assert (c: BOOLEAN; line: CARDINAL; column: CARDINAL;
                  message: ARRAY OF CHAR) ;
VAR
   r: INTEGER ;
   a: ARRAY [0..10] OF CHAR ;
BEGIN
   IF NOT c
   THEN
      r := write(2, ADR(__FILE__), Length(__FILE__)) ;
      r := write(2, ADR(": "), Length(":")) ;
      CardToStr(line, 0, a) ;
      r := write(2, ADR(a), Length(a)) ;
      r := write(2, ADR(": "), Length(":")) ;
      CardToStr(column, 0, a) ;
      r := write(2, ADR(a), Length(a)) ;
      r := write(2, ADR(": "), Length(":")) ;
      r := write(2, ADR(message), Length(message)) ;
      a[0] := nl ;
      a[1] := nul ;
      r := write(2, ADR(a), Length(a)) ;
      e := 1
   END
END Assert ;

VAR
   e, r: INTEGER ;
BEGIN
   Assert(NOT IsInExceptionState(), __LINE__, __COLUMN__, "should not be in the exception state") ;
   THROW(1) ;
   exit(1)
EXCEPT
   Assert(IsInExceptionState(), __LINE__, __COLUMN__, "should be in the exception state") ;
   r := printf("correctly in exception handler, about to exit with code %d\n", e) ;
   exit(e)
END libexcept.
