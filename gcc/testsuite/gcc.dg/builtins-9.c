/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding of built-in math functions doesn't
   break anything.

   Written by Roger Sayle, 2nd April 2003.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

extern double log(double);
extern double exp(double);
extern double sqrt(double);
extern double pow(double,double);

extern float logf(float);
extern float expf(float);
extern float sqrtf(float);
extern float powf(float,float);

extern long double logl(long double);
extern long double expl(long double);
extern long double sqrtl(long double);
extern long double powl(long double,long double);


double test1(double x, double y)
{
  return log(pow(x,y));
}

double test2(double x, double y)
{
  return sqrt(pow(x,y));
}

double test3(double x, double y)
{
  return pow(exp(x),y);
}

double test4(double x, double y)
{
  return pow(sqrt(x),y);
}

double test5(double x, double y, double z)
{
  return pow(pow(x,y),z);
}


float test1f(float x, float y)
{
  return logf(powf(x,y));
}

float test2f(float x, float y)
{
  return sqrtf(powf(x,y));
}

float test3f(float x, float y)
{
  return powf(expf(x),y);
}

float test4f(float x, float y)
{
  return powf(sqrtf(x),y);
}

float test5f(float x, float y, float z)
{
  return powf(powf(x,y),z);
}


long double test1l(long double x, long double y)
{
  return logl(powl(x,y));
}

long double test2l(long double x, long double y)
{
  return sqrtl(powl(x,y));
}

long double test3l(long double x, long double y)
{
  return powl(expl(x),y);
}

long double test4l(long double x, long double y)
{
  return powl(sqrtl(x),y);
}

long double test5l(long double x, long double y, long double z)
{
  return powl(powl(x,y),z);
}

