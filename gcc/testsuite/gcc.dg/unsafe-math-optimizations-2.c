/* Test __UNSAFE_MATH_OPTIMIZATIONS__ with -fno-unsafe-math-optimizations.  */
/* { dg-do compile } */
/* { dg-options "-fno-unsafe-math-optimizations" } */

#ifdef __UNSAFE_MATH_OPTIMIZATIONS__
#error "__UNSAFE_MATH_OPTIMIZATIONS__ defined"
#endif

#pragma GCC optimize "-funsafe-math-optimizations"
#ifndef __UNSAFE_MATH_OPTIMIZATIONS__
#error "__UNSAFE_MATH_OPTIMIZATIONS__ not defined"
#endif

#pragma GCC optimize "-fno-unsafe-math-optimizations"
#ifdef __UNSAFE_MATH_OPTIMIZATIONS__
#error "__UNSAFE_MATH_OPTIMIZATIONS__ defined"
#endif
