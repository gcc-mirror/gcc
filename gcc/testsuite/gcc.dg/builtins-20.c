/* Copyright (C) 2003  Free Software Foundation.

   Verify that built-in math function constant folding doesn't break
   anything and produces the expected results.

   Written by Roger Sayle, 8th June 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */
/* { dg-options "-O2 -ffast-math -mmacosx-version-min=10.3" { target powerpc-*-darwin* } } */
/* { dg-options "-O2 -ffast-math -std=c99" { target *-*-solaris2* } } */

#include "builtins-config.h"

extern double cos (double);
extern double sin (double);
extern double tan (double);
extern double fabs (double);
extern double hypot (double, double);
extern double pure (double) __attribute__ ((__pure__));
extern float cosf (float);
extern float sinf (float);
extern float tanf (float);
extern float fabsf (float);
extern float hypotf (float, float);
extern float puref (float) __attribute__ ((__pure__));
extern long double cosl (long double);
extern long double sinl (long double);
extern long double tanl (long double);
extern long double fabsl (long double);
extern long double hypotl (long double, long double);
extern long double purel (long double) __attribute__ ((__pure__));

extern void link_error(void);

void test1(double x)
{
  if (cos(x) != cos(-x))
    link_error ();

  if (cos(x) != cos(fabs(x)))
    link_error ();

  if (cos(x) != cos(-fabs(x)))
    link_error ();

  if (cos(tan(x)) != cos(tan(-fabs(x))))
    link_error ();

  if (sin(x)/cos(x) != tan(x))
    link_error ();

  if (cos(x)/sin(x) != 1.0/tan(x))
    link_error ();

  if (tan(x)*cos(x) != sin(x))
    link_error ();

  if (cos(x)*tan(x) != sin(x))
    link_error ();

  if (sin(x)/tan(x) != cos(x))
    link_error ();

  if (tan(x)/sin(x) != 1.0/cos(x))
    link_error ();
}

void test2(double x, double y)
{
  if (-tan(x-y) != tan(y-x))
    link_error ();

  if (-sin(x-y) != sin(y-x))
    link_error ();

  if (cos(-x*y) != cos(x*y))
    link_error ();

  if (cos(x*-y) != cos(x*y))
    link_error ();

  if (cos(-x/y) != cos(x/y))
    link_error ();

  if (cos(x/-y) != cos(x/y))
    link_error ();

  if (cos(-fabs(tan(x/-y))) != cos(tan(x/y)))
    link_error ();

  if (hypot (x, 0) != fabs(x))
    link_error ();

  if (hypot (0, x) != fabs(x))
    link_error ();

  if (hypot (x, x) != fabs(x) * __builtin_sqrt(2))
    link_error ();

  if (hypot (-x, y) != hypot (x, y))
    link_error ();

  if (hypot (x, -y) != hypot (x, y))
    link_error ();

  if (hypot (-x, -y) != hypot (x, y))
    link_error ();

  if (hypot (fabs(x), y) != hypot (x, y))
    link_error ();

  if (hypot (x, fabs(y)) != hypot (x, y))
    link_error ();

  if (hypot (fabs(x), fabs(y)) != hypot (x, y))
    link_error ();

  if (hypot (-fabs(-x), -fabs(fabs(fabs(-y)))) != hypot (x, y))
    link_error ();

  if (hypot (-x, 0) != fabs(x))
    link_error ();

  if (hypot (-x, x) != fabs(x) * __builtin_sqrt(2))
    link_error ();

  if (hypot (pure(x), -pure(x)) != fabs(pure(x)) * __builtin_sqrt(2))
    link_error ();

  if (hypot (tan(-x), tan(-fabs(y))) != hypot (tan(x), tan(y)))
    link_error ();
}

void test1f(float x)
{
  if (cosf(x) != cosf(-x))
    link_error ();

  if (cosf(x) != cosf(fabsf(x)))
    link_error ();

  if (cosf(x) != cosf(-fabsf(x)))
    link_error ();

  if (cosf(tanf(x)) != cosf(tanf(-fabsf(x))))
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

  if (sinf(x)/tanf(x) != cosf(x))
    link_error ();

  if (tanf(x)/sinf(x) != 1.0f/cosf(x))
    link_error ();
#endif
}

void test2f(float x, float y)
{
  if (-tanf(x-y) != tanf(y-x))
    link_error ();

  if (-sinf(x-y) != sinf(y-x))
    link_error ();

  if (cosf(-x*y) != cosf(x*y))
    link_error ();

  if (cosf(x*-y) != cosf(x*y))
    link_error ();

  if (cosf(-x/y) != cosf(x/y))
    link_error ();

  if (cosf(x/-y) != cosf(x/y))
    link_error ();

  if (cosf(-fabsf(tanf(x/-y))) != cosf(tanf(x/y)))
    link_error ();

  if (hypotf (x, 0) != fabsf(x))
    link_error ();

  if (hypotf (0, x) != fabsf(x))
    link_error ();

  if (hypotf (x, x) != fabsf(x) * __builtin_sqrtf(2))
    link_error ();

  if (hypotf (-x, y) != hypotf (x, y))
    link_error ();

  if (hypotf (x, -y) != hypotf (x, y))
    link_error ();

  if (hypotf (-x, -y) != hypotf (x, y))
    link_error ();

  if (hypotf (fabsf(x), y) != hypotf (x, y))
    link_error ();

  if (hypotf (x, fabsf(y)) != hypotf (x, y))
    link_error ();

  if (hypotf (fabsf(x), fabsf(y)) != hypotf (x, y))
    link_error ();

  if (hypotf (-fabsf(-x), -fabsf(fabsf(fabsf(-y)))) != hypotf (x, y))
    link_error ();

  if (hypotf (-x, 0) != fabsf(x))
    link_error ();

  if (hypotf (-x, x) != fabsf(x) * __builtin_sqrtf(2))
    link_error ();

  if (hypotf (puref(x), -puref(x)) != fabsf(puref(x)) * __builtin_sqrtf(2))
    link_error ();

  if (hypotf (tanf(-x), tanf(-fabsf(y))) != hypotf (tanf(x), tanf(y)))
    link_error ();
}


void test1l(long double x)
{
  if (cosl(x) != cosl(-x))
    link_error ();

  if (cosl(x) != cosl(fabsl(x)))
    link_error ();

  if (cosl(x) != cosl(-fabsl(x)))
    link_error ();

  if (cosl(tanl(x)) != cosl(tanl(-fabsl(x))))
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

  if (sinl(x)/tanl(x) != cosl(x))
    link_error ();

  if (tanl(x)/sinl(x) != 1.0l/cosl(x))
    link_error ();
#endif
}

void test2l(long double x, long double y)
{
  if (-tanl(x-y) != tanl(y-x))
    link_error ();

  if (-sinl(x-y) != sinl(y-x))
    link_error ();

  if (cosl(-x*y) != cosl(x*y))
    link_error ();

  if (cosl(x*-y) != cosl(x*y))
    link_error ();

  if (cosl(-x/y) != cosl(x/y))
    link_error ();

  if (cosl(x/-y) != cosl(x/y))
    link_error ();

  if (cosl(-fabsl(tanl(x/-y))) != cosl(tanl(x/y)))
    link_error ();

  if (hypotl (x, 0) != fabsl(x))
    link_error ();

  if (hypotl (0, x) != fabsl(x))
    link_error ();

  if (hypotl (x, x) != fabsl(x) * __builtin_sqrtl(2))
    link_error ();

  if (hypotl (-x, y) != hypotl (x, y))
    link_error ();

  if (hypotl (x, -y) != hypotl (x, y))
    link_error ();

  if (hypotl (-x, -y) != hypotl (x, y))
    link_error ();

  if (hypotl (fabsl(x), y) != hypotl (x, y))
    link_error ();

  if (hypotl (x, fabsl(y)) != hypotl (x, y))
    link_error ();

  if (hypotl (fabsl(x), fabsl(y)) != hypotl (x, y))
    link_error ();

  if (hypotl (-fabsl(-x), -fabsl(fabsl(fabsl(-y)))) != hypotl (x, y))
    link_error ();

  if (hypotl (-x, 0) != fabsl(x))
    link_error ();

  if (hypotl (-x, x) != fabsl(x) * __builtin_sqrtl(2))
    link_error ();

  if (hypotl (purel(x), -purel(x)) != fabsl(purel(x)) * __builtin_sqrtl(2))
    link_error ();

  if (hypotl (tanl(-x), tanl(-fabsl(y))) != hypotl (tanl(x), tanl(y)))
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

