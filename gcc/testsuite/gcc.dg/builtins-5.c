/* Copyright (C) 2003  Free Software Foundation.

   Verify that built-in math function constant folding of constant
   arguments is correctly performed by the by the compiler.

   Written by Roger Sayle, 20th February 2003.  */

/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error(void);

void test(double x)
{
  if (pow (x, 0.0) != 1.0)
    link_error ();
  if (pow (1.0, x) != 1.0)
    link_error ();
}

void testf(float x)
{
  if (powf (x, 0.0f) != 1.0f)
    link_error ();
  if (powf (1.0f, x) != 1.0f)
    link_error ();
}

void testl(long double x)
{
  if (powl (x, 0.0l) != 1.0l)
    link_error ();
  if (powl (1.0l, x) != 1.0l)
    link_error ();
}

int main()
{
  test (0.0);
  testf (0.0f);
  testl (0.0l);

  return 0;
}

