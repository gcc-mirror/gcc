/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding of built-in math functions doesn't
   break anything and produces the expected results.

   Written by Roger Sayle, 29th July 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

extern void link_error(void);

extern double pow(double,double);

void test(double x)
{
  if (pow(x,2.0) != x*x)
    link_error ();

  if (x*pow(x,2.0) != pow(x,3.0))
    link_error ();

  if (pow(x,2.0)*x != pow(x,3.0))
    link_error ();

  if (pow(x,3.0) != x*x*x)
    link_error ();

  if (pow(x,2.0)*x != x*x*x)
    link_error ();

  if (x*pow(x,2.0) != x*x*x)
    link_error ();

  if (pow(x,3.0)/x != pow(x,2.0))
    link_error ();

  if (pow(x,3.0)/x != x*x)
    link_error ();
}

int main()
{
  test (2.0);
  return 0;
}

