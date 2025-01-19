(* ComplexMath.mod implement the ISO ComplexMath specification.

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

IMPLEMENTATION MODULE ComplexMath ;

IMPORT cbuiltin ;


(* Returns the length of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cabs)) abs (z: COMPLEX): REAL;
BEGIN
   RETURN cbuiltin.cabs (z)
END abs ;


(* Returns the angle that z subtends to the positive real axis *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carg)) arg (z: COMPLEX): REAL;
BEGIN
   RETURN cbuiltin.carg (z)
END arg ;


(* Returns the complex conjugate of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_conj)) conj (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.conj (z)
END conj ;


(* Returns the value of the number base raised to the power exponent *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cpower)) power (base: COMPLEX; exponent: REAL): COMPLEX;
BEGIN
   RETURN cbuiltin.cpow (base, exponent)
END power ;


(* Returns the principal square root of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csqrt)) sqrt (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.csqrt (z)
END sqrt ;


(* Returns the complex exponential of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cexp)) exp (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.cexp (z)
END exp ;


(* Returns the principal value of the natural logarithm of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cln)) ln (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.clog (z)
END ln ;


(* Returns the sine of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_csin)) sin (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.csin (z)
END sin ;


(* Returns the cosine of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ccos)) cos (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.ccos (z)
END cos ;


(* Returns the tangent of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_ctan)) tan (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.ctan (z)
END tan ;


(* Returns the arcsine of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carcsin)) arcsin (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.casin (z)
END arcsin ;


(* Returns the arccosine of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carccos)) arccos (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.cacos (z)
END arccos ;


(* Returns the arctangent of z *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_carctan)) arctan (z: COMPLEX): COMPLEX;
BEGIN
   RETURN cbuiltin.catan (z)
END arctan ;


(* Returns the complex number with the specified polar coordinates *)

PROCEDURE polarToComplex (abs, arg: REAL): COMPLEX;
BEGIN
   RETURN CMPLX (abs*cbuiltin.cos(arg), abs*cbuiltin.sin(arg))
END polarToComplex ;


(* Returns the scalar product of scalar with z *)

PROCEDURE scalarMult (scalar: REAL; z: COMPLEX): COMPLEX;
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


END ComplexMath.
