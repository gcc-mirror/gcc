/* Copyright (C) 2004  Free Software Foundation.

   Verify that built-in math function conversion into integer rounding
   functions is correctly performed by the compiler.

   Written by Kaveh ghazi, 2004-04-26.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */
/* { dg-options "-ffast-math -mmacosx-version-min=10.3" { target powerpc-*-darwin* } } */
/* { dg-options "-ffast-math -std=c99" { target *-*-solaris2* } } */

#include "../builtins-config.h"

/* Macro to do all FP type combinations.  The second half tests
   narrowing the FP type.  */
#define TEST_FP2FIXED(FN1, FN2) \
  extern void link_error_##FN1##_##FN2(void); \
  extern void link_error_##FN1##f_##FN2##f(void); \
  extern void link_error_##FN1##l_##FN2##l(void); \
  extern void link_error_##FN1##_l##FN2(void); \
  extern void link_error_##FN1##f_l##FN2##f(void); \
  extern void link_error_##FN1##l_l##FN2##l(void); \
  if ((long)__builtin_##FN1(d) != __builtin_##FN2(d)) \
    link_error_##FN1##_##FN2(); \
  if ((long)__builtin_##FN1##f(f) != __builtin_##FN2##f(f)) \
    link_error_##FN1##f_##FN2##f(); \
  if ((long)__builtin_##FN1##l(ld) != __builtin_##FN2##l(ld)) \
    link_error_##FN1##l_##FN2##l(); \
  if ((long long)__builtin_##FN1(d) != __builtin_l##FN2(d)) \
    link_error_##FN1##_l##FN2(); \
  if ((long long)__builtin_##FN1##f(f) != __builtin_l##FN2##f(f)) \
    link_error_##FN1##f_l##FN2##f(); \
  if ((long long)__builtin_##FN1##l(ld) != __builtin_l##FN2##l(ld)) \
    link_error_##FN1##l_l##FN2##l(); \
  extern void link_error_##FN1##_##FN2##f(void); \
  extern void link_error_##FN1##l_##FN2(void); \
  extern void link_error_##FN1##l_##FN2##f(void); \
  extern void link_error_##FN1##_l##FN2##f(void); \
  extern void link_error_##FN1##l_l##FN2(void); \
  extern void link_error_##FN1##l_l##FN2##f(void); \
  if (sizeof(double) > sizeof(float) \
      && (long)__builtin_##FN1(f) != __builtin_##FN2##f(f)) \
    link_error_##FN1##_##FN2##f(); \
  if (sizeof(long double) > sizeof(double) \
      && (long)__builtin_##FN1##l(d) != __builtin_##FN2(d)) \
    link_error_##FN1##l_##FN2(); \
  if (sizeof(long double) > sizeof(float) \
      && (long)__builtin_##FN1##l(f) != __builtin_##FN2##f(f)) \
    link_error_##FN1##l_##FN2##f(); \
  if (sizeof(double) > sizeof(float) \
      && (long long)__builtin_##FN1(f) != __builtin_l##FN2##f(f)) \
    link_error_##FN1##_l##FN2##f(); \
  if (sizeof(long double) > sizeof(double) \
      && (long long)__builtin_##FN1##l(d) != __builtin_l##FN2(d)) \
    link_error_##FN1##l_l##FN2(); \
  if (sizeof(long double) > sizeof(float) \
      && (long long)__builtin_##FN1##l(f) != __builtin_l##FN2##f(f)) \
    link_error_##FN1##l_l##FN2##f()

void __attribute__ ((__noinline__)) foo (double d, float f, long double ld)
{
#ifdef __OPTIMIZE__
# ifdef HAVE_C99_RUNTIME
  /* The resulting transformation functions are all C99.  */
  TEST_FP2FIXED (ceil, lceil);
  TEST_FP2FIXED (floor, lfloor);
  TEST_FP2FIXED (round, lround);
  TEST_FP2FIXED (nearbyint, lrint);
  TEST_FP2FIXED (rint, lrint);
# endif
#endif
}

int main()
{
  foo (1.0, 2.0, 3.0);
  return 0;
}
