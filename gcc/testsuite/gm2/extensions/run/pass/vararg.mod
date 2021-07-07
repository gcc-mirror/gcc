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

MODULE vararg ;

FROM cvararg IMPORT func ;

BEGIN
   IF func(1, INTEGER(1))=1
   THEN
   END ;
   IF func(2, INTEGER(1), INTEGER(2))=1
   THEN
   END ;
   IF func(3, INTEGER(1), INTEGER(2), INTEGER(3))=1
   THEN
   END ;
   IF func(4, INTEGER(1), INTEGER(2), INTEGER(3), INTEGER(4))=1
   THEN
   END ;
   IF func(5, INTEGER(1), INTEGER(2), INTEGER(3), INTEGER(4), INTEGER(5))=1
   THEN
   END
END vararg.
