/* Copyright (C) 2004 Free Software Foundation.

   Check that fmod, fmodf, fmodl, drem, dremf and dreml
   built-in functions compile.

   Written by Uros Bizjak, 5th May 2004.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double fmod(double, double);
extern double drem(double, double);
extern float fmodf(float, float);
extern float dremf(float, float);
extern long double fmodl(long double, long double);
extern long double dreml(long double, long double);


double test1(double x, double y)
{
  return fmod(x, y);
}

double test2(double x, double y)
{
  return drem(x, y);
}

float test1f(float x, float y)
{
  return fmodf(x, y);
}

float test2f(float x, float y)
{
  return dremf(x, y);
}

long double test1l(long double x, long double y)
{
  return fmodl(x, y);
}

long double test2l(long double x, long double y)
{
  return dreml(x, y);
}
