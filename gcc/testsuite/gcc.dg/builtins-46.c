/* Copyright (C) 2004 Free Software Foundation.

   Check that rint, rintf, rintl, floor, floorf, floorl,
   ceil, ceilf, ceill, trunc, truncf, truncl,
   nearbyint, nearbyintf and nearbyintl
   built-in functions compile.

   Written by Uros Bizjak, 25th Aug 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double rint(double);
extern double floor(double);
extern double ceil(double);
extern double trunc(double);
extern double nearbyint(double);

extern float rintf(float);
extern float floorf(float);
extern float ceilf(float);
extern float truncf(float);
extern float nearbyintf(float);

extern long double rintl(long double);
extern long double floorl(long double);
extern long double ceill(long double);
extern long double truncl(long double);
extern long double nearbyintl(long double);


double test1(double x)
{
  return rint(x);
}

double test2(double x)
{
  return floor(x);
}

double test3(double x)
{
  return ceil(x);
}

double test4(double x)
{
  return trunc(x);
}

double test5(double x)
{
  return nearbyint(x);
}

float test1f(float x)
{
  return rintf(x);
}

float test2f(float x)
{
  return floorf(x);
}

float test3f(float x)
{
  return ceilf(x);
}

float test4f(float x)
{
  return truncf(x);
}

float test5f(float x)
{
  return nearbyintf(x);
}

long double test1l(long double x)
{
  return rintl(x);
}

long double test2l(long double x)
{
  return floorl(x);
}

long double test3l(long double x)
{
  return ceill(x);
}

long double test4l(long double x)
{
  return truncl(x);
}

long double test5l(long double x)
{
  return nearbyintl(x);
}
