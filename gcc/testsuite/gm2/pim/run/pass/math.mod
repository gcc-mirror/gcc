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

MODULE math ;

IMPORT MathLib0, SMathLib0 ;
FROM libc IMPORT printf, exit ;


PROCEDURE Assert (b: BOOLEAN; f: ARRAY OF CHAR; l: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      printf("%s:%d: assert failed\n", f, l) ;
      exit(1)
   END
END Assert ;


VAR
   r: REAL ;
   s: SHORTREAL ;
BEGIN
   r := 2.3 ;
   printf("value of entier (10.0 + r) = %d (should be 12)\n", MathLib0.entier (10.0 + r)) ;
   Assert(MathLib0.entier (10.0 + r) = 12, __FILE__, __LINE__) ;
   s := 5.9 ;
   printf("value of SMathLib0.entier (10.0 + s) = %d (should be 15)\n", SMathLib0.entier (10.0 + s)) ;
   Assert(SMathLib0.entier (10.0 + s) = 15, __FILE__, __LINE__) ;
END math.
