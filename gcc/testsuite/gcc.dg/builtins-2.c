/* Copyright (C) 2002  Free Software Foundation.

   Verify that built-in math function constant folding doesn't
   cause any problems for the compiler.

   Written by Roger Sayle, 16th August 2002.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

double test1(double x)
{
  return log(exp(x));
}

double test2(double x)
{
  return exp(log(x));
}

double test3(double x)
{
  return sqrt(exp(x));
}

double test4(double x)
{
  return log(sqrt(x));
}

double test5(double x, double y)
{
  return sqrt(x)*sqrt(y);
}

double test6(double x, double y)
{
  return exp(x)*exp(y);
}

double test7(double x, double y)
{
  return x/exp(y);
}

double test8(double x)
{
  return fabs(sqrt(x));
}

double test9(double x)
{
  return fabs(exp(x));
}

double test10(double x)
{
  return tan(atan(x));
}

double test11(double x)
{
  return fabs(fabs(x));
}

double test12(double x)
{
  return fabs(atan(x));
}

double test13(double x)
{
  return fabs(pow(2.0,x));
}

float test1f(float x)
{
  return logf(expf(x));
}

float test2f(float x)
{
  return expf(logf(x));
}

float test3f(float x)
{
  return sqrtf(expf(x));
}

float test4f(float x)
{
  return logf(sqrtf(x));
}

float test5f(float x, float y)
{
  return sqrtf(x)*sqrtf(y);
}

float test6f(float x, float y)
{
  return expf(x)*expf(y);
}

float test7f(float x, float y)
{
  return x/expf(y);
}

float test8f(float x)
{
  return fabsf(sqrtf(x));
}

float test9f(float x)
{
  return fabsf(expf(x));
}

float test10f(float x)
{
  return tanf(atanf(x));
}

float test11f(float x)
{
  return fabsf(fabsf(x));
}

float test12f(float x)
{
  return fabsf(atanf(x));
}

float test13f(float x)
{
  return fabsf(powf(2.0f,x));
}

long double test1l(long double x)
{
  return logl(expl(x));
}

long double test2l(long double x)
{
  return expl(logl(x));
}

long double test3l(long double x)
{
  return sqrtl(expl(x));
}

long double test4l(long double x)
{
  return logl(sqrtl(x));
}

long double test5l(long double x, long double y)
{
  return sqrtl(x)*sqrtl(y);
}

long double test6l(long double x, long double y)
{
  return expl(x)*expl(y);
}

long double test7l(long double x, long double y)
{
  return x/expl(y);
}

long double test8l(long double x)
{
  return fabsl(sqrtl(x));
}

long double test9l(long double x)
{
  return fabsl(expl(x));
}

long double test10l(long double x)
{
  return tanl(atanl(x));
}

long double test11l(long double x)
{
  return fabsl(fabsl(x));
}

long double test12l(long double x)
{
  return fabsl(atanl(x));
}

long double test13l(long double x)
{
  return fabsl(powl(2.0l,x));
}

