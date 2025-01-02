(* LongMath.mod implement the ISO LongMath specification.

Copyright (C) 2023-2025 Free Software Foundation, Inc.
Contributed by Gaius Mulley <gaiusmod2@gmail.com>.

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

IMPLEMENTATION MODULE ShortMath ;

IMPORT libm ;
IMPORT cbuiltin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sqrtf)) sqrt (x: SHORTREAL): SHORTREAL;
  (* Returns the positive square root of x *)
BEGIN
   RETURN cbuiltin.sqrtf (x)
END sqrt ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_expf)) exp (x: SHORTREAL): SHORTREAL;
  (* Returns the exponential of x *)
BEGIN
   RETURN cbuiltin.expf (x)
END exp ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_logf)) ln (x: SHORTREAL): SHORTREAL;
  (* Returns the natural logarithm of x *)
BEGIN
   RETURN cbuiltin.logf (x)
END ln ;

  (* The angle in all trigonometric functions is measured in radians *)

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_sinf)) sin (x: SHORTREAL): SHORTREAL;
  (* Returns the sine of x *)
BEGIN
   RETURN cbuiltin.sinf (x)
END sin ;

PROCEDURE __ATTRIBUTE__ __BUILTIN__ ((__builtin_cosf)) cos (x: SHORTREAL): SHORTREAL;
  (* Returns the cosine of x *)
BEGIN
   RETURN cbuiltin.cosf (x)
END cos ;

PROCEDURE tan (x: SHORTREAL): SHORTREAL;
  (* Returns the tangent of x *)
BEGIN
   RETURN libm.tanf (x)
END tan ;

PROCEDURE arcsin (x: SHORTREAL): SHORTREAL;
  (* Returns the arcsine of x *)
BEGIN
   RETURN libm.asinf (x)
END arcsin ;

PROCEDURE arccos (x: SHORTREAL): SHORTREAL;
  (* Returns the arccosine of x *)
BEGIN
   RETURN libm.acosf (x)
END arccos ;

PROCEDURE arctan (x: SHORTREAL): SHORTREAL;
  (* Returns the arctangent of x *)
BEGIN
   RETURN libm.atanf (x)
END arctan ;

PROCEDURE power (base, exponent: SHORTREAL): SHORTREAL;
  (* Returns the value of the number base raised to the power exponent *)
BEGIN
   RETURN libm.powf (base, exponent)
END power ;

PROCEDURE round (x: SHORTREAL) : INTEGER;
  (* Returns the value of x rounded to the nearest integer *)
BEGIN
   RETURN TRUNC (x)
END round ;

PROCEDURE IsRMathException (): BOOLEAN;
  (* Returns TRUE if the current coroutine is in the
     exceptional execution state because of the raising
     of an exception in a routine from this module; otherwise
     returns FALSE.
  *)
BEGIN
   RETURN FALSE
END IsRMathException ;

END ShortMath.
