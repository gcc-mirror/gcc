/* Test __NO_TRAPPING_MATH__ is defined with -fno-trapping-math.  */
/* { dg-do compile } */
/* { dg-options "-fno-trapping-math" } */

#ifndef __NO_TRAPPING_MATH__
#error "__NO_TRAPPING_MATH__ not defined"
#endif

#pragma GCC optimize "-ftrapping-math"
#ifdef __NO_TRAPPING_MATH__
#error "__NO_TRAPPING_MATH__ defined"
#endif

#pragma GCC optimize "-fno-trapping-math"
#ifndef __NO_TRAPPING_MATH__
#error "__NO_TRAPPING_MATH__ not defined"
#endif
