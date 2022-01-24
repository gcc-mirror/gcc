/* Test __RECIPROCAL_MATH__ is not defined with -fno-reciprocal-math.  */
/* { dg-do compile } */
/* { dg-options "-fno-reciprocal-math" } */

#ifdef __RECIPROCAL_MATH__
#error "__RECIPROCAL_MATH__ defined"
#endif

#pragma GCC optimize "-freciprocal-math"
#ifndef __RECIPROCAL_MATH__
#error "__RECIPROCAL_MATH__ not defined"
#endif

#pragma GCC optimize "-fno-reciprocal-math"
#ifdef __RECIPROCAL_MATH__
#error "__RECIPROCAL_MATH__ defined"
#endif
