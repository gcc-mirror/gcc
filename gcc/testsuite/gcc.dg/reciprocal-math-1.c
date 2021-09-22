/* Test __RECIPROCAL_MATH__ is defined with -freciprocal-math.  */
/* { dg-do compile } */
/* { dg-options "-freciprocal-math" } */

#ifndef __RECIPROCAL_MATH__
#error "__RECIPROCAL_MATH__ not defined"
#endif

#pragma GCC optimize "-fno-reciprocal-math"
#ifdef __RECIPROCAL_MATH__
#error "__RECIPROCAL_MATH__ defined"
#endif

#pragma GCC optimize "-freciprocal-math"
#ifndef __RECIPROCAL_MATH__
#error "__RECIPROCAL_MATH__ not defined"
#endif
