/* Copyright (C) 2002  Free Software Foundation.

   Verify that all the __builtin_ math functions are recognized
   by the compiler.

   Written by Roger Sayle, 11th July 2002.  */

/* { dg-do compile } */
/* { dg-final { scan-assembler-not "__builtin_" } } */

double test1(double x) { return __builtin_sqrt(x); }
double test2(double x) { return __builtin_cos(x); }
double test3(double x) { return __builtin_sin(x); }
double test4(double x) { return __builtin_exp(x); }
double test5(double x) { return __builtin_log(x); }

float test1f(float x) { return __builtin_sqrtf(x); }
float test2f(float x) { return __builtin_cosf(x); }
float test3f(float x) { return __builtin_sinf(x); }
float test4f(float x) { return __builtin_expf(x); }
float test5f(float x) { return __builtin_logf(x); }

long double test1l(long double x) { return __builtin_sqrtl(x); }
long double test2l(long double x) { return __builtin_cosl(x); }
long double test3l(long double x) { return __builtin_sinl(x); }
long double test4l(long double x) { return __builtin_expl(x); }
long double test5l(long double x) { return __builtin_logl(x); }

