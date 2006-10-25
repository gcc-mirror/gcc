/* Copyright (C) 2004 Free Software Foundation.

   Check that fmod, fmodf, fmodl, drem, dremf, dreml,
   remainder, remainderf and remainderl
   built-in functions compile.

   Written by Uros Bizjak, 5th May 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2" } */

extern double fmod(double, double);
extern float fmodf(float, float);
extern long double fmodl(long double, long double);

extern double remainder(double, double);
extern float remainderf(float, float);
extern long double remainderl(long double, long double);

extern double drem(double, double);
extern float dremf(float, float);
extern long double dreml(long double, long double);


double test1(double x, double y)
{
  return fmod(x, y);
}

float test1f(float x, float y)
{
  return fmodf(x, y);
}

long double test1l(long double x, long double y)
{
  return fmodl(x, y);
}

double test2(double x, double y)
{
  return remainder(x, y);
}

float test2f(float x, float y)
{
  return remainderf(x, y);
}

long double test2l(long double x, long double y)
{
  return remainderl(x, y);
}

double test3(double x, double y)
{
  return drem(x, y);
}

float test3f(float x, float y)
{
  return dremf(x, y);
}

long double test3l(long double x, long double y)
{
  return dreml(x, y);
}
