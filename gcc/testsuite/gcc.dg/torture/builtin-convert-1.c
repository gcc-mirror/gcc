/* Copyright (C) 2004  Free Software Foundation.

   Verify that built-in math function conversion to smaller FP types
   is correctly performed by the compiler.

   Written by Kaveh Ghazi, 2004-03-17.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */

#include "../builtins-config.h"

/* This check is necessary when converting to a C99 function.  */
#ifdef HAVE_C99_RUNTIME
#define C99CODE(CODE) (CODE)
#define MAYBEC99(CODE, C99) (CODE)
#else
#define C99CODE(CODE) 0
#define MAYBEC99(CODE, C99) (!(C99) && (CODE))
#endif

#define PROTOTYPE1(FN) extern double FN(double); extern float FN##f(float); \
  extern long double FN##l(long double);

void test(double d1, float f1, long double ld1)
{
  /* Test converting math builtins to narrower FP types based on a
     narrowing cast on the outside of the call.  MATHFN is the
     function to test, and C99 is 0/1 depending on whether the
     `double' version of MATHFN is a C99 function.  The optimization
     is only performed if the replacement function is actually
     narrower in width, so check that first.  */
#define OUTER_CAST1(MATHFN, C99) \
 PROTOTYPE1 (MATHFN) \
 extern void link_failure_outer_##MATHFN##l_##MATHFN##_1(void); \
 extern void link_failure_outer_##MATHFN##l_##MATHFN##_2(void); \
 extern void link_failure_outer_##MATHFN##l_##MATHFN##f_1(void); \
 extern void link_failure_outer_##MATHFN##l_##MATHFN##f_2(void); \
 extern void link_failure_outer_##MATHFN##_##MATHFN##f_1(void); \
 extern void link_failure_outer_##MATHFN##_##MATHFN##f_2(void); \
 if (sizeof (long double) > sizeof (double) \
     && MAYBEC99 ((double) MATHFN##l((double)ld1) != MATHFN(ld1), C99)) \
    link_failure_outer_##MATHFN##l_##MATHFN##_1(); \
 if (sizeof (long double) > sizeof (double) \
     && MAYBEC99 ((double) MATHFN##l(d1) != MATHFN(d1), C99)) \
    link_failure_outer_##MATHFN##l_##MATHFN##_1(); \
 if (sizeof (long double) > sizeof (double) \
     && MAYBEC99 ((double) MATHFN##l(f1) != MATHFN(f1), C99)) \
    link_failure_outer_##MATHFN##l_##MATHFN##_2(); \
 if (sizeof (long double) > sizeof (float) \
     && C99CODE ((float) MATHFN##l((float) ld1) != MATHFN##f(ld1))) \
    link_failure_outer_##MATHFN##l_##MATHFN##f_1(); \
 if (sizeof (long double) > sizeof (float) \
     && C99CODE ((float) MATHFN##l((float) d1) != MATHFN##f(d1))) \
    link_failure_outer_##MATHFN##l_##MATHFN##f_1(); \
 if (sizeof (long double) > sizeof (float) \
     && C99CODE ((float) MATHFN##l(f1) != MATHFN##f(f1))) \
    link_failure_outer_##MATHFN##l_##MATHFN##f_2(); \
 if (sizeof (double) > sizeof (float) \
     && C99CODE ((float) MATHFN((float) ld1) != MATHFN##f(ld1))) \
    link_failure_outer_##MATHFN##_##MATHFN##f_1(); \
 if (sizeof (double) > sizeof (float) \
     && C99CODE ((float) MATHFN((float) d1) != MATHFN##f(d1))) \
    link_failure_outer_##MATHFN##_##MATHFN##f_1(); \
 if (sizeof (double) > sizeof (float) \
     && C99CODE ((float) MATHFN(f1) != MATHFN##f(f1))) \
    link_failure_outer_##MATHFN##_##MATHFN##f_2()

  /* Test converting math builtins to narrower FP types based on if
     the argument is a narrower type (perhaps implicitly) cast to a
     wider one.  */
#define INNER_CAST1(MATHFN, C99) \
 PROTOTYPE1 (MATHFN) \
 extern void link_failure_inner_##MATHFN##l_##MATHFN(void); \
 extern void link_failure_inner_##MATHFN##l_##MATHFN##f(void); \
 extern void link_failure_inner_##MATHFN##_##MATHFN##f(void); \
 if (sizeof (long double) > sizeof (double) \
     && MAYBEC99 (MATHFN##l(d1) != (long double) MATHFN(d1), C99)) \
    link_failure_inner_##MATHFN##l_##MATHFN(); \
 if (sizeof (long double) > sizeof (float) \
     && C99CODE (MATHFN##l(f1) != (long double) MATHFN##f(f1))) \
    link_failure_inner_##MATHFN##l_##MATHFN##f(); \
 if (sizeof (long double) > sizeof (float) \
     && C99CODE (MATHFN##l((double)f1) != (long double) MATHFN##f(f1))) \
    link_failure_inner_##MATHFN##l_##MATHFN##f(); \
 if (sizeof (double) > sizeof (float) \
     && C99CODE (MATHFN(f1) != (double) MATHFN##f(f1))) \
    link_failure_inner_##MATHFN##_##MATHFN##f()


#ifdef __OPTIMIZE__
  OUTER_CAST1 (acos, /*C99=*/ 0);
  OUTER_CAST1 (acosh, /*C99=*/ 1);
  OUTER_CAST1 (asin, /*C99=*/ 1);
  OUTER_CAST1 (asinh, /*C99=*/ 1);
  OUTER_CAST1 (atan, /*C99=*/ 0);
  OUTER_CAST1 (atanh, /*C99=*/ 1);
  OUTER_CAST1 (cbrt, /*C99=*/ 1);
  OUTER_CAST1 (cos, /*C99=*/ 0);
  OUTER_CAST1 (cosh, /*C99=*/ 0);
  OUTER_CAST1 (erf, /*C99=*/ 1);
  OUTER_CAST1 (erfc, /*C99=*/ 1);
  OUTER_CAST1 (exp, /*C99=*/ 0);
  OUTER_CAST1 (exp2, /*C99=*/ 1);
  OUTER_CAST1 (expm1, /*C99=*/ 1);
  OUTER_CAST1 (fabs, /*C99=*/ 0);
  OUTER_CAST1 (lgamma, /*C99=*/ 1);
  OUTER_CAST1 (log, /*C99=*/ 0);
  OUTER_CAST1 (log10, /*C99=*/ 0);
  OUTER_CAST1 (log1p, /*C99=*/ 1);
  OUTER_CAST1 (log2, /*C99=*/ 1);
  OUTER_CAST1 (logb, /*C99=*/ 1);
  OUTER_CAST1 (sin, /*C99=*/ 0);
  OUTER_CAST1 (sinh, /*C99=*/ 0);
  OUTER_CAST1 (sqrt, /*C99=*/ 0);
  OUTER_CAST1 (tan, /*C99=*/ 0);
  OUTER_CAST1 (tanh, /*C99=*/ 0);
  OUTER_CAST1 (tgamma, /*C99=*/ 1);

  INNER_CAST1 (ceil, /*C99=*/ 0);
  OUTER_CAST1 (ceil, /*C99=*/ 0);
  INNER_CAST1 (floor, /*C99=*/ 0);
  OUTER_CAST1 (floor, /*C99=*/ 0);
  INNER_CAST1 (nearbyint, /*C99=*/ 1);
  OUTER_CAST1 (nearbyint, /*C99=*/ 1);
  INNER_CAST1 (rint, /*C99=*/ 1);
  OUTER_CAST1 (rint, /*C99=*/ 1);
  INNER_CAST1 (round, /*C99=*/ 1);
  OUTER_CAST1 (round, /*C99=*/ 1);
  INNER_CAST1 (trunc, /*C99=*/ 1);
  OUTER_CAST1 (trunc, /*C99=*/ 1);
#endif /* __OPTIMIZE__ */
}

int main (void)
{
  return 0;
}
