/* Copyright (C) 2004 Free Software Foundation.

   Check tan, tanf and tanl built-in functions.

   Written by Uros Bizjak, 7th April 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double tan(double);
extern float tanf(float);
extern long double tanl(long double);


double test1(double x)
{
  return tan(x);
}

float test1f(float x)
{
  return tanf(x);
}

long double test1l(long double x)
{
  return tanl(x);
}

