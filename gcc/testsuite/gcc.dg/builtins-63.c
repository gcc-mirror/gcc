/* Copyright (C) 2007 Free Software Foundation.

   Check that isinf, isinff and isinfl built-in functions compile.

   Written by Uros Bizjak, 31st January 2007.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

extern int isinf(double);
extern int isinff(float);
extern int isinfl(long double);

int test1(double x)
{
  return isinf(x);
}

int test1f(float x)
{
  return isinff(x);
}

int test1l(long double x)
{
  return isinfl(x);
}

