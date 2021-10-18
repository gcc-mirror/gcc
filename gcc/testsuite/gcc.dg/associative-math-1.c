/* Test __ASSOCIATIVE_MATH__ is defined with -fassociative-math.  */
/* { dg-do compile } */
/* { dg-options "-fassociative-math -fno-signed-zeros -fno-trapping-math" } */

#ifndef __ASSOCIATIVE_MATH__
#error "__ASSOCIATIVE_MATH__ not defined"
#endif

#pragma GCC optimize "-fno-associative-math"
#ifdef __ASSOCIATIVE_MATH__
#error "__ASSOCIATIVE_MATH__ defined"
#endif

#pragma GCC optimize "-fassociative-math"
#ifndef __ASSOCIATIVE_MATH__
#error "__ASSOCIATIVE_MATH__ not defined"
#endif
