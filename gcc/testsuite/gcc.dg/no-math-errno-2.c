/* Test __NO_MATH_ERRNO__ is defined with -ffast-math.  */
/* { dg-do compile } */
/* { dg-options "-ffast-math" } */

#ifndef __NO_MATH_ERRNO__
#error "__NO_MATH_ERRNO__ not defined"
#endif
