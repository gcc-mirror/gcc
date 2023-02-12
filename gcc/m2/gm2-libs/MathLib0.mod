(* MathLib0.mod provides access to math functions.

Copyright (C) 2003-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE MathLib0 ;

IMPORT cbuiltin, libm ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrt)) sqrt (x: REAL): REAL;
BEGIN
   RETURN cbuiltin.sqrt (x)
END sqrt ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrtl)) sqrtl (x: LONGREAL): LONGREAL;
BEGIN
   RETURN cbuiltin.sqrtl (x)
END sqrtl ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrts)) sqrts (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sqrtf (x)
END sqrts ;

PROCEDURE exp (x: REAL) : REAL ;
BEGIN
   RETURN libm.exp (x)
END exp ;

PROCEDURE exps (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.expf (x)
END exps ;

PROCEDURE expl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN libm.expl (x)
END expl ;


(*
                log (b)
   log (b)  =      c
      a         ------
                log (a)
                   c
*)

PROCEDURE ln (x: REAL) : REAL ;
BEGIN
   RETURN libm.log (x) / libm.log (exp1)
END ln ;

PROCEDURE lns (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.logf (x) / libm.logf (exp1)
END lns ;

PROCEDURE lnl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN libm.logl (x) / libm.logl (exp1)
END lnl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sin)) sin (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.sin (x)
END sin ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinl)) sinl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.sinl (x)
END sinl ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_sinf)) sins (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.sinf (x)
END sins ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cos)) cos (x: REAL) : REAL ;
BEGIN
   RETURN cbuiltin.cos (x)
END cos ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosf)) coss (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN cbuiltin.cosf (x)
END coss ;

PROCEDURE __ATTRIBUTE__  __BUILTIN__ ((__builtin_cosl)) cosl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN cbuiltin.cosl (x)
END cosl ;

PROCEDURE tan (x: REAL) : REAL ;
BEGIN
   RETURN libm.tan (x)
END tan ;

PROCEDURE tanl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN libm.tanl (x)
END tanl ;

PROCEDURE tans (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.tanf (x)
END tans ;

PROCEDURE arctan (x: REAL) : REAL ;
BEGIN
   RETURN libm.atan (x)
END arctan ;

PROCEDURE arctans (x: SHORTREAL) : SHORTREAL ;
BEGIN
   RETURN libm.atanf (x)
END arctans ;

PROCEDURE arctanl (x: LONGREAL) : LONGREAL ;
BEGIN
   RETURN libm.atanl (x)
END arctanl ;

PROCEDURE entier (x: REAL) : INTEGER ;
BEGIN
   RETURN TRUNC (libm.floor (x))
END entier ;

PROCEDURE entiers (x: SHORTREAL) : INTEGER ;
BEGIN
   RETURN TRUNC (libm.floorf (x))
END entiers ;

PROCEDURE entierl (x: LONGREAL) : INTEGER ;
BEGIN
   RETURN TRUNC (libm.floorl (x))
END entierl ;


END MathLib0.
