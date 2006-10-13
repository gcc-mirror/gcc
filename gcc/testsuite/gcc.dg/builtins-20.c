/* Copyright (C) 2003  Free Software Foundation.

   Verify that built-in math function constant folding doesn't break
   anything and produces the expected results.

   Written by Roger Sayle, 8th June 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

#include "builtins-config.h"

extern double cos (double);
extern double sin (double);
extern double tan (double);
extern float cosf (float);
extern float sinf (float);
extern float tanf (float);
extern long double cosl (long double);
extern long double sinl (long double);
extern long double tanl (long double);

extern void link_error(void);

void test1(double x)
{
  if (cos(x) != cos(-x))
    link_error ();

  if (sin(x)/cos(x) != tan(x))
    link_error ();

  if (cos(x)/sin(x) != 1.0/tan(x))
    link_error ();

  if (tan(x)*cos(x) != sin(x))
    link_error ();

  if (cos(x)*tan(x) != sin(x))
    link_error ();
}

void test2(double x, double y)
{
  if (-tan(x-y) != tan(y-x))
    link_error ();

  if (-sin(x-y) != sin(y-x))
    link_error ();
}

void test1f(float x)
{
  if (cosf(x) != cosf(-x))
    link_error ();

#ifdef HAVE_C99_RUNTIME
  if (sinf(x)/cosf(x) != tanf(x))
    link_error ();

  if (cosf(x)/sinf(x) != 1.0f/tanf(x))
    link_error ();

  if (tanf(x)*cosf(x) != sinf(x))
    link_error ();

  if (cosf(x)*tanf(x) != sinf(x))
    link_error ();
#endif
}

void test2f(float x, float y)
{
  if (-tanf(x-y) != tanf(y-x))
    link_error ();

  if (-sinf(x-y) != sinf(y-x))
    link_error ();
}


void test1l(long double x)
{
  if (cosl(x) != cosl(-x))
    link_error ();

#ifdef HAVE_C99_RUNTIME
  if (sinl(x)/cosl(x) != tanl(x))
    link_error ();

  if (cosl(x)/sinl(x) != 1.0l/tanl(x))
    link_error ();

  if (tanl(x)*cosl(x) != sinl(x))
    link_error ();

  if (cosl(x)*tanl(x) != sinl(x))
    link_error ();
#endif
}

void test2l(long double x, long double y)
{
  if (-tanl(x-y) != tanl(y-x))
    link_error ();

  if (-sinl(x-y) != sinl(y-x))
    link_error ();
}

int main()
{
  test1 (1.0);
  test2 (1.0, 2.0);

  test1f (1.0f);
  test2f (1.0f, 2.0f);

  test1l (1.0l);
  test2l (1.0l, 2.0l);

  return 0;
}

