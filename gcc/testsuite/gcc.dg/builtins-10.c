/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding of built-in math functions doesn't
   break anything and produces the expected results.

   Written by Roger Sayle, 2nd April 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

extern void link_error(void);

extern double exp(double);
extern double log(double);
extern double sqrt(double);
extern double pow(double,double);
extern double fabs(double);

void test(double x)
{
  /*if (sqrt(pow(x,4.0)) != x*x)
    link_error (); */

  if (pow(sqrt(x),4.0) != x*x)
    link_error ();

  if (pow(pow(x,4.0),0.25) != x)
    link_error ();
}

void test2(double x, double y, double z)
{
  if (sqrt(pow(x,y)) != pow(fabs(x),y*0.5))
    link_error ();

  if (log(pow(x,y)) != y*log(x))
    link_error ();

  if (pow(exp(x),y) != exp(x*y))
    link_error ();

  if (pow(sqrt(x),y) != pow(x,y*0.5))
    link_error ();

  if (pow(pow(x,y),z) != pow(x,y*z))
    link_error ();
}

int main()
{
  test (2.0);
  test2 (2.0, 3.0, 4.0);
  return 0;
}

