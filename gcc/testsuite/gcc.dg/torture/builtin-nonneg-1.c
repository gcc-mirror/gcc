/* Copyright (C) 2004  Free Software Foundation.

   Verify that GCC can determine which built-in functions produce a
   nonnegative result.

   Written by Kaveh Ghazi, 2004-03-10.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */

#define PROTOTYPE_RTYPE(FN,RTYPE) extern RTYPE FN(double); \
  extern RTYPE FN##f(float); \
  extern RTYPE FN##l(long double);
#define PROTOTYPE(FN) extern double FN(double); extern float FN##f(float); \
  extern long double FN##l(long double);
#define PROTOTYPE2(FN) extern double FN(double, double); \
  extern float FN##f(float, float); \
  extern long double FN##l(long double, long double);
#define CPROTOTYPE1(FN) extern double FN(_Complex double); \
  extern float FN##f(_Complex float); \
  extern long double FN##l(_Complex long double);
#define CPROTOTYPE1(FN) extern double FN(_Complex double); \
  extern float FN##f(_Complex float); \
  extern long double FN##l(_Complex long double);
#define IPROTOTYPE(FN) extern int FN(int); extern int FN##l(long); \
  extern int FN##ll(long long);
#define PROTOTYPE2TYPE2(FN,A2TYPE) extern double FN(double, A2TYPE); \
  extern float FN##f(float, A2TYPE); \
  extern long double FN##l(long double, A2TYPE);
#define PROTOTYPE2_A2FPTR(FN) extern double FN(double, double *); \
  extern float FN##f(float, float *); \
  extern long double FN##l(long double, long double *);

extern int signbit (double);
extern int signbitf (float);
extern int signbitl (long double);

void test(double d1, double d2, float f1, float f2,
	  long double ld1, long double ld2)
{
  /* These are always nonnegative.  */

#define TEST1(FN) \
 extern void link_failure_##FN (void); PROTOTYPE(FN) \
 if (signbit(FN(d1)) || signbitf(FN##f(f1)) || signbitl(FN##l(ld1))) \
   link_failure_##FN()

#define TEST2(FN) \
 extern void link_failure_##FN (void); PROTOTYPE2(FN) \
 if (signbit(FN(d1,d2)) || signbitf(FN##f(f1,f2)) || signbitl(FN##l(ld1,ld2))) \
   link_failure_##FN()

#define CTEST1(FN) \
 extern void link_failure_##FN (void); CPROTOTYPE1(FN) \
 if (signbit(FN(d1)) || signbitf(FN##f(f1)) || signbitl(FN##l(ld1))) \
   link_failure_##FN()

#define ITEST1(FN) \
 extern void link_failure_##FN (void); IPROTOTYPE(FN) \
 if (signbit(FN(d1)) || signbitf(FN##l(f1)) || signbitl(FN##ll(ld1))) \
   link_failure_##FN()

  TEST1 (acos);
  TEST1 (acosh);
  CTEST1 (cabs);
  TEST1 (cosh);
  TEST1 (erfc);
  TEST1 (exp);
  TEST1 (exp10);
  TEST1 (exp2);
  TEST1 (fabs);
  TEST2 (fdim);
  TEST2 (hypot);
  TEST1 (pow10);
  TEST1 (sqrt);
  ITEST1 (ffs);
  ITEST1 (__builtin_parity);
  ITEST1 (__builtin_popcount);

  /* These are nonnegative if the first argument is.  */
#define ARG1TEST1(FN) \
 extern void link_failure_##FN (void); PROTOTYPE(FN) \
 if (signbit(FN(fabs(d1))) || signbitf(FN##f(fabsf(f1))) \
     || signbitl(FN##l(fabsl(ld1)))) \
   link_failure_##FN()

  /* Same, but allow specifying the return type.  */
#define ARG1TEST1_RTYPE(FN,RTYPE) \
 extern void link_failure_##FN (void); PROTOTYPE_RTYPE(FN,RTYPE) \
 if (signbit(FN(fabs(d1))) || signbitf(FN##f(fabsf(f1))) \
     || signbitl(FN##l(fabsl(ld1)))) \
   link_failure_##FN()

  /* These are nonnegative if the first argument is.  */
#define ARG1TEST2(FN) \
 extern void link_failure_##FN (void); PROTOTYPE2(FN) \
 if (signbit(FN(fabs(d1),d2)) || signbitf(FN##f(fabsf(f1),f2)) \
     || signbitl(FN##l(fabsl(ld1),ld2))) \
   link_failure_##FN()

  /* These are nonnegative if the second argument is.  */
#define ARG2TEST2(FN) \
 extern void link_failure_##FN (void); PROTOTYPE2(FN) \
 if (signbit(FN(d1,fabs(d2))) || signbitf(FN##f(f1,fabsf(f2))) \
     || signbitl(FN##l(ld1,fabsl(ld2)))) \
   link_failure_##FN()

  /* These are nonnegative if the first OR second argument is.  */
#define ARG2TESTor(FN) \
 extern void link_failure_##FN (void); PROTOTYPE2(FN) \
 if (signbit(FN(fabs(d1),d2)) || signbitf(FN##f(fabsf(f1),f2)) \
     || signbitl(FN##l(fabsl(ld1),ld2)) || signbit(FN(d1,fabs(d2))) \
     || signbitf(FN##f(f1,fabsf(f2))) || signbitl(FN##l(ld1,fabsl(ld2)))) \
   link_failure_##FN()

  /* These are nonnegative if the first AND second argument is.  */
#define ARG2TESTand(FN) \
 extern void link_failure_##FN (void); PROTOTYPE2(FN) \
 if (signbit(FN(fabs(d1),fabs(d2))) || signbitf(FN##f(fabsf(f1),fabsf(f2))) \
     || signbitl(FN##l(fabsl(ld1),fabsl(ld2)))) \
   link_failure_##FN()

  /* These are nonnegative if the first argument is, 2nd arg is an int.  */
#define ARG1TEST2_A2INT(FN, INTTYPE) \
 extern void link_failure_##FN (void); PROTOTYPE2TYPE2(FN, INTTYPE) \
 if (signbit(FN(fabs(d1),d2)) || signbitf(FN##f(fabsf(f1),f2)) \
     || signbitl(FN##l(fabsl(ld1),ld2))) \
   link_failure_##FN()

  /* These are nonnegative if the first argument is, specify 2nd arg.  */
#define ARG2TEST1_A2FPTR(FN) \
 extern void link_failure_##FN (void); PROTOTYPE2_A2FPTR(FN) \
 if (signbit(FN(fabs(d1),&d2)) || signbitf(FN##f(fabsf(f1),&f2)) \
     || signbitl(FN##l(fabsl(ld1),&ld2))) \
   link_failure_##FN()

  ARG1TEST1 (asinh);
  ARG1TEST1 (atan);
  ARG1TEST1 (atanh);
  ARG1TEST1 (cbrt);
  ARG1TEST1 (ceil);
  ARG1TEST1 (erf);
  ARG1TEST1 (expm1);
  ARG1TEST1 (floor);
  ARG1TEST2 (fmod);
  ARG1TEST2_A2INT (ldexp, int);
  ARG1TEST1_RTYPE (__builtin_llceil, long long);
  ARG1TEST1_RTYPE (__builtin_llfloor, long long);
  ARG1TEST1_RTYPE (llrint, long long);
  ARG1TEST1_RTYPE (llround, long long);
  ARG1TEST1_RTYPE (__builtin_lceil, long);
  ARG1TEST1_RTYPE (__builtin_lfloor, long);
  ARG1TEST1_RTYPE (lrint, long);
  ARG1TEST1_RTYPE (lround, long);
  /* The modf* functions aren't ever "const" or "pure" even with
     -ffast-math so they won't be eliminated and yield a link failure.  */
  /* ARG2TEST1_A2FPTR (modf);*/
  ARG1TEST1 (nearbyint);
  ARG1TEST2 (pow);
  ARG1TEST1 (rint);
  ARG1TEST1 (round);
  ARG1TEST2 (scalb);
  ARG1TEST2_A2INT (scalbln, long);
  ARG1TEST2_A2INT (scalbn, int);
  ARG1TEST1_RTYPE (signbit, int);
  ARG1TEST1 (significand);
  ARG1TEST1 (sinh);
  ARG1TEST1 (tanh);
  ARG1TEST1 (trunc);

  ARG2TESTor (fmax);
  ARG2TESTand (fmin);
  ARG2TEST2 (copysign);

}

int main (void)
{
  return 0;
}
