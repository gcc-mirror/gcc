/* Copyright (C) 2004 Free Software Foundation.

   Check that constant folding of copysign, copysignf and copysignl math
   functions doesn't break anything and produces the expected results.

   Written by Roger Sayle, 6th June 2004.  */

/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error(void);

extern double copysign(double, double);
extern float copysignf(float, float);
extern long double copysignl(long double, long double);

int main()
{
  if (copysign (2.0, 1.0) != 2.0)
    link_error ();
  if (copysign (2.0, -1.0) != -2.0)
    link_error ();
  if (copysign (-2.0, 1.0) != 2.0)
    link_error ();
  if (copysign (-2.0, -1.0) != -2.0)
    link_error ();

  if (copysign (2.0, 1.0) != 2.0)
    link_error ();
  if (copysign (2.0, -1.0) != -2.0)
    link_error ();
  if (copysign (-2.0, 1.0) != 2.0)
    link_error ();
  if (copysign (-2.0, -1.0) != -2.0)
    link_error ();

  if (copysignf (2.0f, 1.0f) != 2.0f)
    link_error ();
  if (copysignf (2.0f, -1.0f) != -2.0f)
    link_error ();
  if (copysignf (-2.0f, 1.0f) != 2.0f)
    link_error ();
  if (copysignf (-2.0f, -1.0f) != -2.0f)
    link_error ();

  if (copysignl (2.0l, 1.0l) != 2.0l)
    link_error ();
  if (copysignl (2.0l, -1.0l) != -2.0l)
    link_error ();
  if (copysignl (-2.0l, 1.0l) != 2.0l)
    link_error ();
  if (copysignl (-2.0l, -1.0l) != -2.0l)
    link_error ();

  return 0;
}

