/* Copyright (C) 2004 Free Software Foundation.

   Check that logb, logbf, logbl, ilogb, ilogbf and ilogbl
   built-in functions compile.

   Written by Uros Bizjak, 14th April 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double logb(double);
extern float logbf(float);
extern long double logbl(long double);
extern int ilogb(double);
extern int ilogbf(float);
extern int ilogbl(long double);


double test1(double x)
{
  return logb(x);
}

float test1f(float x)
{
  return logbf(x);
}

long double test1l(long double x)
{
  return logbl(x);
}

int test2(double x)
{
  return ilogb(x);
}

int test2f(float x)
{
  return ilogbf(x);
}

int test2l(long double x)
{
  return ilogbl(x);
}

