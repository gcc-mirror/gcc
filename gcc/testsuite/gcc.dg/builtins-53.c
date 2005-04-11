/* Copyright (C) 2005 Free Software Foundation.

   Check that (long)floor, (long)floorf, (long)floorl,
   (long long)floor, (long long)floorf, (long long)floorl,
   (long)ceil, (long)ceilf, (long)ceill,
   (long long)ceil, (long long)ceilf, (long long)ceill
   built-in functions compile.

   Written by Uros Bizjak, 5th April 2005.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double floor(double);
extern double ceil(double);

extern float floorf(float);
extern float ceilf(float);

extern long double floorl(long double);
extern long double ceill(long double);


long int test1(double x)
{
  return floor(x);
}

long long int test2(double x)
{
  return floor(x);
}

long int test3(double x)
{
  return ceil(x);
}

long long int test4(double x)
{
  return ceil(x);
}

long int test1f(float x)
{
  return floorf(x);
}

long long int test2f(float x)
{
  return floorf(x);
}

long int test3f(float x)
{
  return ceilf(x);
}

long long int test4f(float x)
{
  return ceilf(x);
}

long int test1l(long double x)
{
  return floorl(x);
}

long long int test2l(long double x)
{
  return floorl(x);
}

long int test3l(long double x)
{
  return ceill(x);
}

long long int test4l(long double x)
{
  return ceill(x);
}
