/* Copyright (C) 2004 Free Software Foundation.

   Check that asin, asinf, asinl, acos, acosf
   and acosl built-in functions compile.

   Written by Uros Bizjak, 20th April 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double asin(double);
extern double acos(double);
extern float asinf(float);
extern float acosf(float);
extern long double asinl(long double);
extern long double acosl(long double);


double test1(double x)
{
  return asin(x);
}

double test2(double x)
{
  return acos(x);
}

float test1f(float x)
{
  return asinf(x);
}

float test2f(float x)
{
  return acosf(x);
}

long double test1l(long double x)
{
  return asinl(x);
}

long double test2l(long double x)
{
  return acosl(x);
}

