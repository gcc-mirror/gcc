/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding of built-in math functions doesn't
   break anything and produces the expected results.

   Written by Roger Sayle, 25th May 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

extern void link_error(void);

extern double exp(double);


int main()
{
  if (exp (1.0) < 2.71 || exp (1.0) > 2.72)
    link_error ();
  if (exp (2.0) < 7.38 || exp (2.0) > 7.39)
    link_error ();
  if (exp (-2.0) < 0.13 || exp (-2.0) > 0.14)
    link_error ();
  if (atan (1.0) < 0.78 || atan (1.0) > 0.79)
    link_error ();

  return 0;
}

