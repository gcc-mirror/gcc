/* Test static initializer folding of implicit conversions to floating point
   types, even with -frounding-math and related options.  Bug 103031.  */
/* { dg-do compile } */
/* { dg-options "-frounding-math -ftrapping-math -fsignaling-nans" } */

float f1 = -1ULL;
float f2 = __DBL_MAX__;
float f3 = __DBL_MIN__;
float f4 = 0.1;
float f5 = __builtin_nans ("");
double d1 = -1ULL;
