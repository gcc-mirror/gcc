(* trivial test module which calls a C module.

Copyright (C) 2009-2020 Free Software Foundation, Inc.
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

MODULE m ;

FROM SYSTEM IMPORT ADR ;
FROM c IMPORT funcString ;

PROCEDURE farrayofchar (a: ARRAY OF CHAR) ;
BEGIN
   IF funcString('hello')#5
   THEN
      HALT(1)
   END ;
   IF funcString(a)#5
   THEN
      HALT(2)
   END
END farrayofchar ;


BEGIN
   farrayofchar('hello')
END m.
