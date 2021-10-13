(* Copyright (C) 2005, 2006 Free Software Foundation, Inc. *)
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

MODULE caseiso2 ;

(* remember SIZE is a standard and pervasive function in ISO *)
FROM libc IMPORT exit ;

VAR
   r1 : RECORD
           CASE :CARDINAL OF   (* case without tag field has this 
                                syntax by ISO *)
             0 : v1 : CARDINAL;
           | 1 : v2 : INTEGER;
           ELSE
           END
        END;

VAR
   r2 : RECORD
           CASE CARDINAL OF
           | 0 : v1 : CARDINAL;       (* pipe is allowed before first 
                                     record field by ISO *)
           | 1 : v2 : INTEGER;
           ELSE
           END
        END;

VAR
   r3: RECORD
          v1: CARDINAL;
       END ;
BEGIN
   r1.v2 := -1 ;
   r2.v2 := -1 ;
   IF SIZE(r1)#SIZE(r2)
   THEN
      exit(1)
   END ;
   IF SIZE(r1)#SIZE(r3)
   THEN
      exit(2)
   END
END caseiso2.
