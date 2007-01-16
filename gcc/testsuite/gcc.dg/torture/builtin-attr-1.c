/* Copyright (C) 2003, 2005  Free Software Foundation.

   Verify that the `const' function attribute is applied to various
   builtins and that these functions are optimized away by the
   compiler under the appropriate circumstances.

   Written by Kaveh Ghazi, 2003-08-04.  */

/* { dg-do link } */
/* { dg-options "-ffast-math" } */

/* These are helper macros to test combinations of functions.  We test
   foo() != foo() with the same arguments, and expect the compiler to
   optimize away these tests of const functions.  */

/* Just test the __builtin_ functions.  */
#define BUILTIN_TEST1(FN, TYPE) \
extern void link_failure_builtin_##FN(void); \
void test_builtin_##FN(TYPE x) \
{ if (__builtin_##FN(x) != __builtin_##FN(x)) link_failure_builtin_##FN(); }

/* Just test the __builtin_ functions.  */
#define BUILTIN_TEST2(FN, TYPE) \
extern void link_failure_builtin_##FN(void); \
void test_builtin_##FN(TYPE x, TYPE y) \
{ if (__builtin_##FN(x,y) != __builtin_##FN(x,y)) link_failure_builtin_##FN(); }

/* Also test the regular (non-__builtin_) function.  */
#define TEST1(FN, TYPE, RTYPE) \
BUILTIN_TEST1(FN, TYPE) \
extern void link_failure_##FN(void); \
extern RTYPE FN(TYPE); \
void test_##FN(TYPE x) { if (FN(x) != FN(x)) link_failure_##FN(); }

/* Test the __builtin_ functions taking void arguments (with the "f"
   and "l" variants).  */
#define BUILTIN_FPTEST0(FN) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(void) \
{ if (__builtin_##FN() != __builtin_##FN()) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(void) \
{ if (__builtin_##FN##f() != __builtin_##FN##f()) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(void) \
{ if (__builtin_##FN##l() != __builtin_##FN##l()) link_failure_builtin_##FN##l(); }

/* Test the __builtin_ functions taking one FP argument (with the "f"
   and "l" variants).  */
#define BUILTIN_FPTEST1(FN) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(double d) \
{ if (__builtin_##FN(d) != __builtin_##FN(d)) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(float f) \
{ if (__builtin_##FN##f(f) != __builtin_##FN##f(f)) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(long double ld) \
{ if (__builtin_##FN##l(ld) != __builtin_##FN##l(ld)) link_failure_builtin_##FN##l(); }

/* Test the __builtin_ functions taking one argument of supplied type
   (with the "f" and "l" variants).  */
#define BUILTIN_FPTEST1ARG(FN, TYPE) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(TYPE x) \
{ if (__builtin_##FN(x) != __builtin_##FN(x)) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(TYPE x) \
{ if (__builtin_##FN##f(x) != __builtin_##FN##f(x)) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(TYPE x) \
{ if (__builtin_##FN##l(x) != __builtin_##FN##l(x)) link_failure_builtin_##FN##l(); }

/* Test the __builtin_ functions taking two FP arguments (with the "f"
   and "l" variants).  */
#define BUILTIN_FPTEST2(FN) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(double d1, double d2) \
{ if (__builtin_##FN(d1,d2) != __builtin_##FN(d1,d2)) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(float f1, float f2) \
{ if (__builtin_##FN##f(f1,f2) != __builtin_##FN##f(f1,f2)) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(long double ld1, long double ld2) \
{ if (__builtin_##FN##l(ld1,ld2) != __builtin_##FN##l(ld1,ld2)) link_failure_builtin_##FN##l(); }

/* Test the __builtin_ functions taking two arguments, the first one
   is of a supplied type and the second one one is of FP type (with
   the "f" and "l" variants).  */
#define BUILTIN_FPTEST2ARG1(FN, TYPE) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(TYPE x, double d) \
{ if (__builtin_##FN(x,d) != __builtin_##FN(x,d)) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(TYPE x, float f) \
{ if (__builtin_##FN##f(x,f) != __builtin_##FN##f(x,f)) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(TYPE x, long double ld) \
{ if (__builtin_##FN##l(x,ld) != __builtin_##FN##l(x,ld)) link_failure_builtin_##FN##l(); }

/* Test the __builtin_ functions taking two arguments, the first one
   is of FP type and the second one one is of a supplied type (with
   the "f" and "l" variants).  */
#define BUILTIN_FPTEST2ARG2(FN, TYPE) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(double d, TYPE x) \
{ if (__builtin_##FN(d,x) != __builtin_##FN(d,x)) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(float f, TYPE x) \
{ if (__builtin_##FN##f(f,x) != __builtin_##FN##f(f,x)) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(long double ld, TYPE x) \
{ if (__builtin_##FN##l(ld,x) != __builtin_##FN##l(ld,x)) link_failure_builtin_##FN##l(); }

/* Test the __builtin_ functions taking three FP arguments (with the
   "f" and "l" variants).  */
#define BUILTIN_FPTEST3(FN) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(double d1, double d2, double d3) \
{ if (__builtin_##FN(d1,d2,d3) != __builtin_##FN(d1,d2,d3)) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(float f1, float f2, float f3) \
{ if (__builtin_##FN##f(f1,f2,f3) != __builtin_##FN##f(f1,f2,f3)) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(long double ld1, long double ld2, long double ld3) \
{ if (__builtin_##FN##l(ld1,ld2,ld3) != __builtin_##FN##l(ld1,ld2,ld3)) link_failure_builtin_##FN##l(); }

/* Test the __builtin_ functions taking one complex argument (with the
   "f" and "l" variants).  */
#define BUILTIN_CPTEST1(FN) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(_Complex double d) \
{ if (__builtin_##FN(d) != __builtin_##FN(d)) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(_Complex float f) \
{ if (__builtin_##FN##f(f) != __builtin_##FN##f(f)) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(_Complex long double ld) \
{ if (__builtin_##FN##l(ld) != __builtin_##FN##l(ld)) link_failure_builtin_##FN##l(); }

/* Test the __builtin_ functions taking two complex arguments (with
   the "f" and "l" variants).  */
#define BUILTIN_CPTEST2(FN) \
extern void link_failure_builtin_##FN(void); \
extern void link_failure_builtin_##FN##f(void); \
extern void link_failure_builtin_##FN##l(void); \
void test_builtin_##FN(_Complex double d1, _Complex double d2) \
{ if (__builtin_##FN(d1,d2) != __builtin_##FN(d1,d2)) link_failure_builtin_##FN(); } \
void test_builtin_##FN##f(_Complex float f1, _Complex float f2) \
{ if (__builtin_##FN##f(f1,f2) != __builtin_##FN##f(f1,f2)) link_failure_builtin_##FN##f(); } \
void test_builtin_##FN##l(_Complex long double ld1, _Complex long double ld2) \
{ if (__builtin_##FN##l(ld1,ld2) != __builtin_##FN##l(ld1,ld2)) link_failure_builtin_##FN##l(); }

/* These macros additionally test the non-__builtin_ functions.  */

/* Test the functions taking one FP argument (with the "f" and "l"
   variants) and returning that type.  */
#define FPTEST1(FN) \
BUILTIN_FPTEST1(FN) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern double FN(double); \
extern float FN##f(float); \
extern long double FN##l(long double); \
void test_##FN(double d) \
{ if (FN(d) != FN(d)) link_failure_##FN(); } \
void test_##FN##f(float f) \
{ if (FN##f(f) != FN##f(f)) link_failure_##FN##f(); } \
void test_##FN##l(long double ld) \
{ if (FN##l(ld) != FN##l(ld)) link_failure_##FN##l(); }

/* Test the functions taking one FP argument (with the "f" and "l"
   variants) and returning TYPE.  */
#define FPTEST1T(FN, TYPE) \
BUILTIN_FPTEST1(FN) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern TYPE FN(double); \
extern TYPE FN##f(float); \
extern TYPE FN##l(long double); \
void test_##FN(double d) \
{ if (FN(d) != FN(d)) link_failure_##FN(); } \
void test_##FN##f(float f) \
{ if (FN##f(f) != FN##f(f)) link_failure_##FN##f(); } \
void test_##FN##l(long double ld) \
{ if (FN##l(ld) != FN##l(ld)) link_failure_##FN##l(); }

/* Test the functions taking two FP arguments (with the "f" and "l"
   variants).  */
#define FPTEST2(FN) \
BUILTIN_FPTEST2(FN) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern double FN(double, double); \
extern float FN##f(float, float); \
extern long double FN##l(long double, long double); \
void test_##FN(double d1, double d2) \
{ if (FN(d1,d2) != FN(d1,d2)) link_failure_##FN(); } \
void test_##FN##f(float f1, float f2) \
{ if (FN##f(f1,f2) != FN##f(f1,f2)) link_failure_##FN##f(); } \
void test_##FN##l(long double ld1, long double ld2) \
{ if (FN##l(ld1,ld2) != FN##l(ld1,ld2)) link_failure_##FN##l(); }

/* Test the functions taking two arguments, the first one is of a
   supplied type and the second one one is of FP type (with the "f"
   and "l" variants).  */
#define FPTEST2ARG1(FN, TYPE) \
BUILTIN_FPTEST2ARG1(FN, TYPE) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern double FN(TYPE, double); \
extern float FN##f(TYPE, float); \
extern long double FN##l(TYPE, long double); \
void test_##FN(TYPE x, double d) \
{ if (FN(x,d) != FN(x,d)) link_failure_##FN(); } \
void test_##FN##f(TYPE x, float f) \
{ if (FN##f(x,f) != FN##f(x,f)) link_failure_##FN##f(); } \
void test_##FN##l(TYPE x, long double ld) \
{ if (FN##l(x,ld) != FN##l(x,ld)) link_failure_##FN##l(); }

/* Test the functions taking two arguments, the first one is of FP
   type and the second one one is of a supplied type (with the "f" and
   "l" variants).  */
#define FPTEST2ARG2(FN, TYPE) \
BUILTIN_FPTEST2ARG2(FN, TYPE) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern double FN(double, TYPE); \
extern float FN##f(float, TYPE); \
extern long double FN##l(long double, TYPE); \
void test_##FN(double d, TYPE x) \
{ if (FN(d,x) != FN(d,x)) link_failure_##FN(); } \
void test_##FN##f(float f, TYPE x) \
{ if (FN##f(f,x) != FN##f(f,x)) link_failure_##FN##f(); } \
void test_##FN##l(long double ld, TYPE x) \
{ if (FN##l(ld,x) != FN##l(ld,x)) link_failure_##FN##l(); }

/* Test the functions taking three FP arguments (with the "f" and "l"
   variants).  */
#define FPTEST3(FN) \
BUILTIN_FPTEST3(FN) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern double FN(double, double, double); \
extern float FN##f(float, float, float); \
extern long double FN##l(long double, long double, long double); \
void test_##FN(double d1, double d2, double d3) \
{ if (FN(d1,d2,d3) != FN(d1,d2,d3)) link_failure_##FN(); } \
void test_##FN##f(float f1, float f2, float f3) \
{ if (FN##f(f1,f2,f3) != FN##f(f1,f2,f3)) link_failure_##FN##f(); } \
void test_##FN##l(long double ld1, long double ld2, long double ld3) \
{ if (FN##l(ld1,ld2,ld3) != FN##l(ld1,ld2,ld3)) link_failure_##FN##l(); }

/* Test the functions taking one complex argument (with the "f" and
   "l" variants) and returning that type.  */
#define CPTEST1(FN) \
BUILTIN_CPTEST1(FN) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern _Complex double FN(_Complex double); \
extern _Complex float FN##f(_Complex float); \
extern _Complex long double FN##l(_Complex long double); \
void test_##FN(_Complex double d) \
{ if (FN(d) != FN(d)) link_failure_##FN(); } \
void test_##FN##f(_Complex float f) \
{ if (FN##f(f) != FN##f(f)) link_failure_##FN##f(); } \
void test_##FN##l(_Complex long double ld) \
{ if (FN##l(ld) != FN##l(ld)) link_failure_##FN##l(); }

/* Test the functions taking one complex argument (with the "f" and
   "l" variants) and returning the real type.  */
#define CPTEST1R(FN) \
BUILTIN_CPTEST1(FN) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern double FN(_Complex double); \
extern float FN##f(_Complex float); \
extern long double FN##l(_Complex long double); \
void test_##FN(_Complex double d) \
{ if (FN(d) != FN(d)) link_failure_##FN(); } \
void test_##FN##f(_Complex float f) \
{ if (FN##f(f) != FN##f(f)) link_failure_##FN##f(); } \
void test_##FN##l(_Complex long double ld) \
{ if (FN##l(ld) != FN##l(ld)) link_failure_##FN##l(); }

/* Test the functions taking two complex arguments (with the "f" and
   "l" variants).  */
#define CPTEST2(FN) \
BUILTIN_CPTEST2(FN) \
extern void link_failure_##FN(void); \
extern void link_failure_##FN##f(void); \
extern void link_failure_##FN##l(void); \
extern _Complex double FN(_Complex double, _Complex double); \
extern _Complex float FN##f(_Complex float, _Complex float); \
extern _Complex long double FN##l(_Complex long double, _Complex long double); \
void test_##FN(_Complex double d1, _Complex double d2) \
{ if (FN(d1,d2) != FN(d1,d2)) link_failure_##FN(); } \
void test_##FN##f(_Complex float f1, _Complex float f2) \
{ if (FN##f(f1,f2) != FN##f(f1,f2)) link_failure_##FN##f(); } \
void test_##FN##l(_Complex long double ld1, _Complex long double ld2) \
{ if (FN##l(ld1,ld2) != FN##l(ld1,ld2)) link_failure_##FN##l(); }


/* Test the math builtins.  */
FPTEST1            (acos)
FPTEST1            (acosh)
FPTEST1            (asin)
FPTEST1            (asinh)
FPTEST1            (atan)
FPTEST2            (atan2)
FPTEST1            (atanh)
FPTEST1            (cbrt)
FPTEST1            (ceil)
FPTEST2            (copysign)
FPTEST1            (cos)
FPTEST1            (cosh)
FPTEST2            (drem)
FPTEST1            (erf)
FPTEST1            (erfc)
FPTEST1            (exp)
FPTEST1            (exp10)
FPTEST1            (exp2)
FPTEST1            (expm1)
FPTEST1            (fabs)
FPTEST2            (fdim)
FPTEST1            (floor)
FPTEST3            (fma)
FPTEST2            (fmax)
FPTEST2            (fmin)
FPTEST2            (fmod)
BUILTIN_FPTEST0    (huge_val)
FPTEST2            (hypot)
FPTEST1T           (ilogb, int)
BUILTIN_FPTEST0    (inf) /* { dg-warning "does not support infinity" "INF unsupported" { target vax-*-* c4x-*-* pdp11-*-* } } */
FPTEST1            (j0)
FPTEST1            (j1)
FPTEST2ARG1        (jn, int)
FPTEST2ARG2        (ldexp, int)
FPTEST1T           (llrint, long long)
FPTEST1T           (llround, long long)
FPTEST1            (log)
FPTEST1            (log10)
FPTEST1            (log1p)
FPTEST1            (log2)
FPTEST1            (logb)
FPTEST1T           (lrint, long)
FPTEST1T           (lround, long)
BUILTIN_FPTEST1ARG (nan, char *)
BUILTIN_FPTEST1ARG (nans, char *)
FPTEST1            (nearbyint)
FPTEST2            (nextafter)
FPTEST2ARG2        (nexttoward, long double)
FPTEST2            (pow)
FPTEST1            (pow10)
FPTEST2            (remainder)
FPTEST1            (rint)
FPTEST1            (round)
FPTEST2            (scalb)
FPTEST2ARG2        (scalbn, int)
FPTEST2ARG2        (scalbln, long int)
FPTEST1            (significand)
FPTEST1            (sin)
FPTEST1            (sinh)
FPTEST1            (sqrt)
FPTEST1            (tan)
FPTEST1            (tanh)
FPTEST1            (tgamma)
FPTEST1            (trunc)
FPTEST1            (y0)
FPTEST1            (y1)
FPTEST2ARG1        (yn, int)

/* Test the complex math builtins.  */
/*CPTEST1 (cabs) See http://gcc.gnu.org/ml/gcc-patches/2003-09/msg00040.html */
CPTEST1 (cacos)
CPTEST1 (cacosh)
CPTEST1R (carg)
CPTEST1 (casin)
CPTEST1 (casinh)
CPTEST1 (catan)
CPTEST1 (catanh)
CPTEST1 (ccos)
CPTEST1 (ccosh)
CPTEST1 (cexp)
CPTEST1R (cimag)
CPTEST1 (clog)
CPTEST1 (conj)
CPTEST2 (cpow)
CPTEST1 (cproj)
CPTEST1R (creal)
CPTEST1 (csin)
CPTEST1 (csinh)
CPTEST1 (csqrt)
CPTEST1 (ctan)
CPTEST1 (ctanh)

typedef __INTMAX_TYPE__ intmax_t;

/* Various other const builtins.  */
TEST1         (abs, int, int)
BUILTIN_TEST1 (clz, int)
BUILTIN_TEST1 (clzl, long)
BUILTIN_TEST1 (clzll, long long)
BUILTIN_TEST1 (ctz, int)
BUILTIN_TEST1 (ctzl, long)
BUILTIN_TEST1 (ctzll, long long)
TEST1         (ffs, int, int)
TEST1         (ffsl, long, int)
TEST1         (ffsll, long long, int)
TEST1         (imaxabs, intmax_t, intmax_t)
TEST1         (labs, long, long)
TEST1         (llabs, long long, long long)
BUILTIN_TEST1 (parity, int)
BUILTIN_TEST1 (parityl, long)
BUILTIN_TEST1 (parityll, long long)
BUILTIN_TEST1 (popcount, int)
BUILTIN_TEST1 (popcountl, long)
BUILTIN_TEST1 (popcountll, long long)

int main(void)
{
  return 0;
}
