/* { dg-do compile } */
/* { dg-options "-Ofast" } */

/* PR c++/48026 */
/* Make sure `#pragma GCC optimize` affects the pre-defined macros too */

#pragma GCC optimize ("no-fast-math")
#ifdef __FAST_MATH__
#  error Hey yo, What you doing on FAST_MATH??
#endif
