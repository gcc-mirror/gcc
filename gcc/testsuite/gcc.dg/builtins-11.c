/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding of built-in math functions doesn't
   break anything and produces the expected results.

   Written by Roger Sayle, 5th April 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

extern void link_error(void);

extern double exp(double);
extern double sqrt(double);
extern double pow(double,double);

void test(double x, double y, double z)
{
  if (sqrt(x)*sqrt(x) != x)
    link_error ();

  if (sqrt(x)*sqrt(y) != sqrt(x*y))
    link_error ();

  if (exp(x)*exp(y) != exp(x+y))
    link_error ();

  if (pow(x,y)*pow(z,y) != pow(z*x,y))
    link_error ();

  if (pow(x,y)*pow(x,z) != pow(x,y+z))
    link_error ();

  if (x/exp(y) != x*exp(-y))
    link_error ();

  if (x/pow(y,z) != x*pow(y,-z))
    link_error ();
}

int main()
{
  test (2.0, 3.0, 4.0);
  return 0;
}

