/* Test __ROUNDING_MATH__ is not defined with -fno-rounding-math.  */
/* { dg-do compile } */
/* { dg-options "-fno-rounding-math" } */

#ifdef __ROUNDING_MATH__
#error "__ROUNDING_MATH__ defined"
#endif

#pragma GCC optimize "-frounding-math"
#ifndef __ROUNDING_MATH__
#error "__ROUNDING_MATH__ not defined"
#endif

#pragma GCC optimize "-fno-rounding-math"
#ifdef __ROUNDING_MATH__
#error "__ROUNDING_MATH__ defined"
#endif
