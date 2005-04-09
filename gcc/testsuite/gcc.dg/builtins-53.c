/* Copyright (C) 2005 Free Software Foundation.

   Check that (long)floor, (long)floorf, (long)floorl,
   (long long)floor, (long long)floorf and (long long)floorl
   built-in functions compile.

   Written by Uros Bizjak, 5th April 2005.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double floor(double);
extern float floorf(float);
extern long double floorl(long double);


long int test1(double x)
{
  return floor(x);
}

long long int test2(double x)
{
  return floor(x);
}

long int test1f(float x)
{
  return floorf(x);
}

long long int test2f(float x)
{
  return floorf(x);
}

long int test1l(long double x)
{
  return floorl(x);
}

long long int test2l(long double x)
{
  return floorl(x);
}
