/* Copyright (C) 2004 Free Software Foundation.

   Check that exp10, exp10f, exp10l, exp2, exp2f and exp2l
   built-in functions compile.

   Written by Uros Bizjak, 13th February 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double exp10(double);
extern double exp2(double);
extern float exp10f(float);
extern float exp2f(float);
extern long double exp10l(long double);
extern long double exp2l(long double);


double test1(double x)
{
  return exp10(x);
}

double test2(double x)
{
  return exp2(x);
}

float test1f(float x)
{
  return exp10f(x);
}

float test2f(float x)
{
  return exp2f(x);
}

long double test1l(long double x)
{
  return exp10l(x);
}

long double test2l(long double x)
{
  return exp2l(x);
}

