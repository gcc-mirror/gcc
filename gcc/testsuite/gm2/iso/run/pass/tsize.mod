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

MODULE tsize ;

FROM libc IMPORT exit ;
FROM SYSTEM IMPORT TSIZE ;

TYPE
   t1 =  RECORD
            CASE tag: CARDINAL OF
          ||| 0 : v1 : CHAR;
          | 1 : v2 : INTEGER;
            ELSE
            END
         END ;

   t2 =  RECORD
            CASE atag: CARDINAL OF
          ||| 0 : v1 : CARDINAL;
          | 1 : v2 : CHAR;
            ELSE
            END
         END ;


BEGIN
   (* incorrect test code commented out
   IF TSIZE(t1, 0)#SIZE(CHAR)
   THEN
      exit(1)
   END ;
   IF TSIZE(t1, 1)#SIZE(INTEGER)
   THEN
      exit(1)
   END ;
   IF TSIZE(t2, 0)#SIZE(CARDINAL)
   THEN
      exit(1)
   END ;
   IF TSIZE(t2, 1)#SIZE(CHAR)
   THEN
      exit(1)
   END
   *)
END tsize.
