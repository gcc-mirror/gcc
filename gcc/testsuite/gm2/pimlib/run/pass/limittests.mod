(* Copyright (C) 2017 Free Software Foundation, Inc.  *)
(* This file is part of GNU Modula-2.

GNU Modula-2 is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GNU Modula-2 is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License along
with gm2; see the file COPYING.  If not, write to the Free Software
Foundation, 51 Franklin Street, Fifth Floor,
Boston, MA 02110-1301, USA.  *)

MODULE limittests ;

FROM Builtins IMPORT isfinite ;
FROM FpuIO IMPORT WriteLongReal, LongRealToStr ;
FROM StrLib IMPORT StrEqual ;
FROM libc IMPORT exit, printf ;
FROM StrIO IMPORT WriteLn ;
FROM FIO IMPORT FlushBuffer, StdOut ;


PROCEDURE Assert (b: BOOLEAN; l: CARDINAL) ;
BEGIN
   IF NOT b
   THEN
      FlushBuffer (StdOut) ;
      printf ("%s:%d:regression test failed during execution\n",
              __FILE__, l) ;
      r := 1
   END
END Assert ;


VAR
   a: REAL;
   r: INTEGER ;
BEGIN
   r := 0 ;
   a := 1.0 / 0.0 ;
   IF NOT isfinite (a)
   THEN
      printf ("detected infinite number\n")
   ELSE
      printf ("failed to detect infinite number\n") ;
      r := 1
   END ;
   a := 1.0 / 1.0 ;
   IF isfinite (a)
   THEN
      printf ("detected finite number\n")
   ELSE
      printf ("failed to detect finite number\n") ;
      r := 2
   END ;
   IF r = 0
   THEN
      printf ("all tests passed\n")
   END ;
   exit (r)

END limittests.
