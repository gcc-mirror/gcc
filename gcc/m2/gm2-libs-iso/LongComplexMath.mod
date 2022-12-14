(* LongComplexMath.mod implement the ISO LongComplexMath specification.

Copyright (C) 2009-2021 Free Software Foundation, Inc.
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

IMPLEMENTATION MODULE LongComplexMath ;

IMPORT cbuiltin ;


(* Returns the length of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cabsl)) abs (z: LONGCOMPLEX): LONGREAL;
BEGIN
   RETURN cbuiltin.cabsl (z)
END abs ;


(* Returns the angle that z subtends to the positive real axis *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cargl)) arg (z: LONGCOMPLEX): LONGREAL;
BEGIN
   RETURN cbuiltin.cargl (z)
END arg ;


(* Returns the complex conjugate of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_conjl)) conj (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.conjl (z)
END conj ;


(* Returns the value of the number base raised to the power exponent *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cpowerl)) power (base: LONGCOMPLEX; exponent: LONGREAL): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.cpowl (base, exponent)
END power ;


(* Returns the principal square root of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csqrtl)) sqrt (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.csqrtl (z)
END sqrt ;


(* Returns the complex exponential of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cexpl)) exp (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.cexpl (z)
END exp ;


(* Returns the principal value of the natural logarithm of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_clnl)) ln (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.clogl (z)
END ln ;


(* Returns the sine of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csinl)) sin (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.csinl (z)
END sin ;


(* Returns the cosine of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ccosl)) cos (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.ccosl (z)
END cos ;


(* Returns the tangent of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ctanl)) tan (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.ctanl (z)
END tan ;


(* Returns the arcsine of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carcsinl)) arcsin (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.casinl (z)
END arcsin ;


(* Returns the arccosine of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carccosl)) arccos (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.cacosl (z)
END arccos ;


(* Returns the arctangent of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carctanl)) arctan (z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN cbuiltin.catanl (z)
END arctan ;


(* Returns the complex number with the specified polar coordinates *)

PROCEDURE polarToComplex (abs, arg: LONGREAL): LONGCOMPLEX;
BEGIN
   RETURN CMPLX (abs*cbuiltin.cosl(arg), abs*cbuiltin.sinl(arg))
END polarToComplex ;


(* Returns the scalar product of scalar with z *)

PROCEDURE scalarMult (scalar: LONGREAL; z: LONGCOMPLEX): LONGCOMPLEX;
BEGIN
   RETURN CMPLX (RE(z)*scalar, IM(z)*scalar)
END scalarMult ;


(* Returns TRUE if the current coroutine is in the exceptional
   execution state because of the raising of an exception in a
   routine from this module; otherwise returns FALSE.
*)

PROCEDURE IsCMathException (): BOOLEAN;
BEGIN
   (* --fixme-- we should really attempt to catch sigfpe in these procedures *)
   RETURN( FALSE )
END IsCMathException ;


END LongComplexMath.
