/* Copyright (C) 2004  Free Software Foundation.

   Verify that builtin math functions (with fixed point return types)
   are converted to smaller FP types correctly by the compiler.

   Written by Kaveh Ghazi, 2004-05-01.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */
/* { dg-add-options c99_runtime } */

#include "../builtins-config.h"

#define PROTOTYPE1_RET(FN, RET) \
  extern RET FN(double); \
  extern RET FN##f(float); \
  extern RET FN##l(long double);

/* Test converting math builtins to narrower FP types based on if the
   argument is a narrower type (perhaps implicitly) cast to a wider
   one.  */
#define INNER_CAST1(MATHFN, RET) \
 PROTOTYPE1_RET (MATHFN, RET); \
 extern void link_failure_inner_##MATHFN##l_##MATHFN(void); \
 extern void link_failure_inner_##MATHFN##l_##MATHFN##f(void); \
 extern void link_failure_inner_##MATHFN##_##MATHFN##f(void); \
 if (sizeof (long double) > sizeof (double) \
     && MATHFN##l(d1) != MATHFN(d1)) \
    link_failure_inner_##MATHFN##l_##MATHFN(); \
 if (sizeof (long double) > sizeof (float) \
     && MATHFN##l(f1) != MATHFN##f(f1)) \
    link_failure_inner_##MATHFN##l_##MATHFN##f(); \
 if (sizeof (long double) > sizeof (float) \
     && MATHFN##l((double)f1) != MATHFN##f(f1)) \
    link_failure_inner_##MATHFN##l_##MATHFN##f(); \
 if (sizeof (double) > sizeof (float) \
     && MATHFN(f1) != MATHFN##f(f1)) \
    link_failure_inner_##MATHFN##_##MATHFN##f()

void __attribute__ ((__noinline__)) test (double d1, float f1)
{
#ifdef __OPTIMIZE__
#ifdef HAVE_C99_RUNTIME
  /* We're converting to implicitly generated C99 functions.  */
  INNER_CAST1 (__builtin_lceil, long);
  INNER_CAST1 (__builtin_llceil, long long);
  INNER_CAST1 (__builtin_lfloor, long);
  INNER_CAST1 (__builtin_llfloor, long long);
  INNER_CAST1 (lround, long);
  INNER_CAST1 (llround, long long);
  INNER_CAST1 (lrint, long);
  INNER_CAST1 (llrint, long long);
#endif /* HAVE_C99_RUNTIME */
#endif /* __OPTIMIZE__ */
}

int main (void)
{
  test (1, 2);
  return 0;
}
