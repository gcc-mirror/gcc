/* Copyright (C) 2002, 2003  Free Software Foundation.

   Verify that all the __builtin_ math functions are recognized
   by the compiler.

   Written by Roger Sayle, 11th July 2002.  */

/* { dg-do compile } */
/* { dg-options "" } */
/* { dg-final { scan-assembler-not "__builtin_" } } */

/* These helper macros ensure we also check the float and long double
   cases.  */

/* Test FP functions taking void.  */
#define FPTEST0(FN) \
double test_##FN(void) { return __builtin_##FN(); } \
float test_##FN##f(void) { return __builtin_##FN##f(); } \
long double test_##FN##l(void) { return __builtin_##FN##l(); } 

/* Test FP functions taking one FP argument.  */
#define FPTEST1(FN) \
double test_##FN(double x) { return __builtin_##FN(x); } \
float test_##FN##f(float x) { return __builtin_##FN##f(x); } \
long double test_##FN##l(long double x) { return __builtin_##FN##l(x); } 

/* Test FP functions taking one argument of a supplied type.  */
#define FPTEST1ARG(FN, TYPE) \
double test_##FN(TYPE x) { return __builtin_##FN(x); } \
float test_##FN##f(TYPE x) { return __builtin_##FN##f(x); } \
long double test_##FN##l(TYPE x) { return __builtin_##FN##l(x); } 

/* Test FP functions taking two arguments, the first argument is of a
   supplied type.  */
#define FPTEST2ARG1(FN, TYPE) \
double test_##FN(TYPE x, double y) { return __builtin_##FN(x, y); } \
float test_##FN##f(TYPE x, float y) { return __builtin_##FN##f(x, y); } \
long double test_##FN##l(TYPE x, long double y) { return __builtin_##FN##l(x, y); } 

/* Test FP functions taking two arguments, the second argument is of a
   supplied type.  */
#define FPTEST2ARG2(FN, TYPE) \
double test_##FN(double x, TYPE y) { return __builtin_##FN(x, y); } \
float test_##FN##f(float x, TYPE y) { return __builtin_##FN##f(x, y); } \
long double test_##FN##l(long double x, TYPE y) { return __builtin_##FN##l(x, y); } 

/* Test FP functions taking two arguments, the second argument is a
   FP pointer.  */
#define FPTEST2FPP2(FN) \
double test_##FN(double x, double *y) { return __builtin_##FN(x, y); } \
float test_##FN##f(float x, float *y) { return __builtin_##FN##f(x, y); } \
long double test_##FN##l(long double x, long double *y) { return __builtin_##FN##l(x, y); } 

/* Test FP functions taking one FP argument and a supplied return
   type.  */
#define FPTEST1RET(FN, TYPE) \
TYPE test_##FN(double x) { return __builtin_##FN(x); } \
TYPE test_##FN##f(float x) { return __builtin_##FN##f(x); } \
TYPE test_##FN##l(long double x) { return __builtin_##FN##l(x); } 

/* Test FP functions taking two FP arguments.  */
#define FPTEST2(FN) \
double test_##FN(double x, double y) { return __builtin_##FN(x, y); } \
float test_##FN##f(float x, float y) { return __builtin_##FN##f(x, y); } \
long double test_##FN##l(long double x, long double y) { return __builtin_##FN##l(x, y); } 

/* Test FP functions taking three FP arguments.  */
#define FPTEST3(FN) \
double test_##FN(double x, double y, double z) { return __builtin_##FN(x, y, z); } \
float test_##FN##f(float x, float y, float z) { return __builtin_##FN##f(x, y, z); } \
long double test_##FN##l(long double x, long double y, long double z) { return __builtin_##FN##l(x, y, z); } 

/* Test FP functions taking three arguments, two FP and the third is
   of a supplied type.  */
#define FPTEST3ARG3(FN, TYPE) \
double test_##FN(double x, double y, TYPE z) { return __builtin_##FN(x, y, z); } \
float test_##FN##f(float x, float y, TYPE z) { return __builtin_##FN##f(x, y, z); } \
long double test_##FN##l(long double x, long double y, TYPE z) { return __builtin_##FN##l(x, y, z); } 

/* Test FP functions taking three FP arguments.  The second and third
   are FP pointers.  The return type is void.  */
#define FPTEST3FPP23VOID(FN) \
double test_##FN(double x, double *y, double *z) { __builtin_##FN(x, y, z); return *y * *z; } \
float test_##FN##f(float x, float *y, float *z) { __builtin_##FN##f(x, y, z); return *y * *z; } \
long double test_##FN##l(long double x, long double *y, long double *z) { __builtin_##FN##l(x, y, z); return *y * *z; } 

/* Keep this list sorted alphabetically by function name.  */
FPTEST1     (acos)
FPTEST1     (acosh)
FPTEST1     (asin)
FPTEST1     (asinh)
FPTEST1     (atan)
FPTEST2     (atan2)
FPTEST1     (atanh)
FPTEST1     (cbrt)
FPTEST1     (ceil)
FPTEST2     (copysign)
FPTEST1     (cos)
FPTEST1     (cosh)
FPTEST2     (drem)
FPTEST1     (erf)
FPTEST1     (erfc)
FPTEST1     (exp)
FPTEST1     (exp10)
FPTEST1     (exp2)
FPTEST1     (expm1)
FPTEST1     (fabs)
FPTEST2     (fdim)
FPTEST1     (floor)
FPTEST3     (fma)
FPTEST2     (fmax)
FPTEST2     (fmin)
FPTEST2     (fmod)
FPTEST2ARG2 (frexp, int *)
FPTEST1     (gamma)
FPTEST0     (huge_val)
FPTEST2     (hypot)
FPTEST1     (ilogb)
FPTEST0     (inf)
FPTEST1     (j0)
FPTEST1     (j1)
FPTEST2ARG1 (jn, int)
FPTEST2ARG2 (ldexp, int)
FPTEST1     (lgamma)
FPTEST1RET  (llrint, long long)
FPTEST1RET  (llround, long long)
FPTEST1     (log)
FPTEST1     (log10)
FPTEST1     (log1p)
FPTEST1     (log2)
FPTEST1     (logb)
FPTEST1RET  (lrint, long)
FPTEST1RET  (lround, long)
FPTEST2FPP2 (modf)
FPTEST1ARG  (nan, char *)
FPTEST1ARG  (nans, char *)
FPTEST1     (nearbyint)
FPTEST2     (nextafter)
FPTEST2     (nexttoward)
FPTEST2     (pow)
FPTEST1     (pow10)
FPTEST2     (remainder)
FPTEST3ARG3 (remquo, int *)
FPTEST1     (rint)
FPTEST1     (round)
FPTEST2     (scalb)
FPTEST2ARG2 (scalbln, int)
FPTEST2ARG2 (scalbn, int)
FPTEST1     (significand)
FPTEST1     (sin)
FPTEST3FPP23VOID (sincos)
FPTEST1     (sinh)
FPTEST1     (sqrt)
FPTEST1     (tan)
FPTEST1     (tanh)
FPTEST1     (tgamma)
FPTEST1     (trunc)
FPTEST1     (y0)
FPTEST1     (y1)
FPTEST2ARG1 (yn, int)
