/* Copyright (C) 2003  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the by the compiler.

   Written by Roger Sayle, 30th March 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

extern void link_error(void);

void test(double x)
{
  if (pow (x, 1.0) != x)
    link_error ();
  if (tan (atan (x)) != x)
    link_error ();
}

void testf(float x)
{
  if (powf (x, 1.0f) != x)
    link_error ();
  if (tanf (atanf (x)) != x)
    link_error ();
}

void testl(long double x)
{
  if (powl (x, 1.0l) != x)
    link_error ();
  if (tanl (atanl (x)) != x)
    link_error ();
}

int main()
{
  test (2.0);
  testf (2.0f);
  testl (2.0l);

  return 0;
}

