/* Copyright (C) 2004  Free Software Foundation.

   Verify that built-in math function conversion into integer rounding
   functions is correctly performed by the compiler.

   Written by Kaveh ghazi, 2004-04-26.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */

#include "../builtins-config.h"

#define PROTOTYPE(FN) extern double FN(double); \
  extern float FN##f(float); \
  extern long double FN##l(long double);
#define PROTOTYPE_RET(FN, RET) extern RET FN(double); \
  extern RET FN##f(float); \
  extern RET FN##l(long double);

/* Macro to do all FP type combinations.  The second half tests
   narrowing the FP type.  */
#define TEST_FP2FIXED(FN1, FN2) \
  PROTOTYPE(FN1) \
  PROTOTYPE_RET(FN2, long) \
  PROTOTYPE_RET(l##FN2, long long) \
  extern void link_error_##FN1##_##FN2(void); \
  extern void link_error_##FN1##f_##FN2##f(void); \
  extern void link_error_##FN1##l_##FN2##l(void); \
  extern void link_error_##FN1##_l##FN2(void); \
  extern void link_error_##FN1##f_l##FN2##f(void); \
  extern void link_error_##FN1##l_l##FN2##l(void); \
  if ((long)FN1(d) != FN2(d)) \
    link_error_##FN1##_##FN2(); \
  if ((long)FN1##f(f) != FN2##f(f)) \
    link_error_##FN1##f_##FN2##f(); \
  if ((long)FN1##l(ld) != FN2##l(ld)) \
    link_error_##FN1##l_##FN2##l(); \
  if ((long long)FN1(d) != l##FN2(d)) \
    link_error_##FN1##_l##FN2(); \
  if ((long long)FN1##f(f) != l##FN2##f(f)) \
    link_error_##FN1##f_l##FN2##f(); \
  if ((long long)FN1##l(ld) != l##FN2##l(ld)) \
    link_error_##FN1##l_l##FN2##l(); \
  extern void link_error_##FN1##_##FN2##f(void); \
  extern void link_error_##FN1##l_##FN2(void); \
  extern void link_error_##FN1##l_##FN2##f(void); \
  extern void link_error_##FN1##_l##FN2##f(void); \
  extern void link_error_##FN1##l_l##FN2(void); \
  extern void link_error_##FN1##l_l##FN2##f(void); \
  if (sizeof(double) > sizeof(float) && (long)FN1(f) != FN2##f(f)) \
    link_error_##FN1##_##FN2##f(); \
  if (sizeof(long double) > sizeof(double) && (long)FN1##l(d) != FN2(d)) \
    link_error_##FN1##l_##FN2(); \
  if (sizeof(long double) > sizeof(float) && (long)FN1##l(f) != FN2##f(f)) \
    link_error_##FN1##l_##FN2##f(); \
  if (sizeof(double) > sizeof(float) && (long long)FN1(f) != l##FN2##f(f)) \
    link_error_##FN1##_l##FN2##f(); \
  if (sizeof(long double) > sizeof(double) && (long long)FN1##l(d) != l##FN2(d)) \
    link_error_##FN1##l_l##FN2(); \
  if (sizeof(long double) > sizeof(float) && (long long)FN1##l(f) != l##FN2##f(f)) \
    link_error_##FN1##l_l##FN2##f()

void __attribute__ ((__noinline__)) foo (double d, float f, long double ld)
{
#ifdef __OPTIMIZE__
# ifdef HAVE_C99_RUNTIME
  /* The resulting transformation functions are all C99.  */
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
