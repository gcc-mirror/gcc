/* Copyright (C) 2003  Free Software Foundation.

   Check that cabs of a non-complex argument is converted into fabs.

   Written by Roger Sayle, 1st June 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

double cabs (__complex__ double);
float cabsf (__complex__ float);
long double cabsl (__complex__ long double);

void link_error (void);

void test (double x)
{
  if (cabs (x) != fabs (x))
    link_error ();
}

void testf (float x)
{
  if (cabsf (x) != fabsf (x))
    link_error ();
}

void testl (long double x)
{
  if (cabsl (x) != fabsl (x))
    link_error ();
}

int main ()
{
  test (1.0);
  testf (1.0f);
  testl (1.0l);
  return 0;
}

