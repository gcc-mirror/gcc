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

MODULE mym2a ;

FROM cpp IMPORT mytry ;
FROM libc IMPORT printf, exit ;


VAR
   r: INTEGER ;
BEGIN
   r := printf("start of main Modula-2 program\n") ;
   mytry ;
   r := printf("ending (should not get here)\n") ;
   exit(1);
EXCEPT
   r := printf("Modula-2 caught exception correctly\n") ;
   exit(0);
END mym2a.
