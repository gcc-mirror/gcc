/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding of built-in math functions doesn't
   break anything and produces the expected results.

   Written by Roger Sayle, 9th April 2003.  */

/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error(void);

extern double pow(double,double);


int main()
{
  if (pow (2.0, 3.0) != 8.0)
    link_error ();

  if (pow (2.0, -3.0) != 0.125)
    link_error ();

  return 0;
}

