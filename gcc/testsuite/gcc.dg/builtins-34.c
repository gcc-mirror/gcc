/* Copyright (C) 2004 Free Software Foundation.

   Check that various built-in functions compile.

   Written by Uros Bizjak, 13th February 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double exp10(double);
extern double exp2(double);
extern double pow10(double);
extern double expm1(double);
extern double ldexp(double, int);
extern double scalb(double, double);
extern double scalbn(double, int);
extern double scalbln(double, long);
extern double significand(double);
extern float exp10f(float);
extern float exp2f(float);
extern float pow10f(float);
extern float expm1f(float);
extern float ldexpf(float, int);
extern float scalbf(float, float);
extern float scalbnf(float, int);
extern float scalblnf(float, long);
extern float significandf(float);
extern long double exp10l(long double);
extern long double exp2l(long double);
extern long double pow10l(long double);
extern long double expm1l(long double);
extern long double ldexpl(long double, int);
extern long double scalbl(long double, long double);
extern long double scalbnl(long double, int);
extern long double scalblnl(long double, long);
extern long double significandl(long double);


double test1(double x)
{
  return exp10(x);
}

double test2(double x)
{
  return exp2(x);
}

double test3(double x)
{
  return pow10(x);
}

double test4(double x)
{
  return expm1(x);
}

double test5(double x, int exp)
{
  return ldexp(x, exp);
}

double test6(double x, double exp)
{
  return scalb(x, exp);
}

double test7(double x, int exp)
{
  return scalbn(x, exp);
}

double test8(double x, long exp)
{
  return scalbln(x, exp);
}

double test9(double x)
{
  return significand(x);
}

float test1f(float x)
{
  return exp10f(x);
}

float test2f(float x)
{
  return exp2f(x);
}

float test3f(float x)
{
  return pow10f(x);
}

float test4f(float x)
{
  return expm1f(x);
}

float test5f(float x, int exp)
{
  return ldexpf(x, exp);
}

float test6f(float x, float exp)
{
  return scalbf(x, exp);
}

float test7f(float x, int exp)
{
  return scalbnf(x, exp);
}

float test8f(float x, long exp)
{
  return scalblnf(x, exp);
}

float test9f(float x)
{
  return significandf(x);
}

long double test1l(long double x)
{
  return exp10l(x);
}

long double test2l(long double x)
{
  return exp2l(x);
}

long double test3l(long double x)
{
  return pow10l(x);
}

long double test4l(long double x)
{
  return expm1l(x);
}

long double test5l(long double x, int exp)
{
  return ldexpl(x, exp);
}

long double test6l(long double x, long double exp)
{
  return scalbl(x, exp);
}

long double test7l(long double x, int exp)
{
  return scalbnl(x, exp);
}

long double test8l(long double x, long exp)
{
  return scalblnl(x, exp);
}

long double test9l(long double x)
{
  return significandl(x);
}
