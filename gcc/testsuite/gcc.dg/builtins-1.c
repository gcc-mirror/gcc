/* Copyright (C) 2002, 2003  Free Software Foundation.

   Verify that all the __builtin_ math functions are recognized
   by the compiler.

   Written by Roger Sayle, 11th July 2002.  */

/* { dg-do compile } */
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
#define FPTEST1TYPE(FN, TYPE) \
double test_##FN(TYPE x) { return __builtin_##FN(x); } \
float test_##FN##f(TYPE x) { return __builtin_##FN##f(x); } \
long double test_##FN##l(TYPE x) { return __builtin_##FN##l(x); } 

/* Test FP functions taking two FP arguments.  */
#define FPTEST2(FN) \
double test_##FN(double x, double y) { return __builtin_##FN(x, y); } \
float test_##FN##f(float x, float y) { return __builtin_##FN##f(x, y); } \
long double test_##FN##l(long double x, long double y) { return __builtin_##FN##l(x, y); } 

/* Keep this list sorted alphabetically by function name.  */
FPTEST1     (atan)
FPTEST2     (atan2)
FPTEST1     (ceil)
FPTEST1     (cos)
FPTEST1     (exp)
FPTEST1     (fabs)
FPTEST1     (floor)
FPTEST2     (fmod)
FPTEST0     (huge_val)
FPTEST0     (inf)
FPTEST1     (log)
FPTEST1TYPE (nan, char *)
FPTEST1TYPE (nans, char *)
FPTEST1     (nearbyint)
FPTEST2     (pow)
FPTEST1     (round)
FPTEST1     (sin)
FPTEST1     (sqrt)
FPTEST1     (tan)
FPTEST1     (trunc)
