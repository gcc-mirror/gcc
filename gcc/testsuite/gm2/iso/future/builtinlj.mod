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

MODULE builtinlj ;

FROM Builtins IMPORT longjmp, setjmp ;
FROM SYSTEM IMPORT ADR, ADDRESS, WORD ;
FROM libc IMPORT printf ;

PROCEDURE func ;
BEGIN
   r := printf("call longjmp\n") ;
   longjmp(ADR(env), 1)
END func ;

VAR
   env: ARRAY [0..5] OF WORD ;
   r  : INTEGER ;
BEGIN
   IF setjmp(ADR(env))=0
   THEN
      func
   ELSE
      r := printf("worked bye\n")
   END
END builtinlj.
