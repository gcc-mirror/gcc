(* SMathLib0.mod provide access to the SHORTREAL instrinics.

Copyright (C) 2009-2025 Free Software Foundation, Inc.
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

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  *)

IMPLEMENTATION MODULE SMathLib0 ;

IMPORT cbuiltin, libm ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrtf)) sqrt (x: SHORTREAL): SHORTREAL;
BEGIN
   RETURN cbuiltin.sqrtf (x)
END sqrt ;

PROCEDURE exp (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.expf (x)
END exp ;


(*
                log (b)
   log (b)  =      c
      a         ------
                log (a)
                   c
*)

PROCEDURE ln (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.logf (x) / libm.logf (exp1)
END ln ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinf)) sin (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sinf (x)
END sin ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosf)) cos (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cosf (x)
END cos ;

PROCEDURE tan (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.tanf (x)
END tan ;

PROCEDURE arctan (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.atanf (x)
END arctan ;

PROCEDURE entier (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN TRUNC (libm.floorf (x))
END entier ;


END SMathLib0.
