/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding of built-in math functions doesn't
   break anything and produces the expected results.

   Written by Roger Sayle, 28th June 2003.  */

/* { dg-do link } */
/* { dg-options "-O2 -ffast-math" } */

extern void link_error(void);

extern double trunc(double);
extern double floor(double);
extern double ceil(double);

extern float truncf(float);
extern float floorf(float);
extern float ceilf(float);

extern long double truncl(long double);
extern long double floorl(long double);
extern long double ceill(long double);

void test(double x)
{
  if (trunc (trunc (x)) != trunc (x))
    link_error ();
  if (trunc (floor (x)) != floor (x))
    link_error ();
  if (trunc (ceil (x)) != ceil (x))
    link_error ();

  if (floor (trunc (x)) != trunc (x))
    link_error ();
  if (floor (floor (x)) != floor (x))
    link_error ();
  if (floor (ceil (x)) != ceil (x))
    link_error ();

  if (ceil (trunc (x)) != trunc (x))
    link_error ();
  if (ceil (floor (x)) != floor (x))
    link_error ();
  if (ceil (ceil (x)) != ceil (x))
    link_error ();
}

void testf(float x)
{
  if (truncf (truncf (x)) != truncf (x))
    link_error ();
  if (truncf (floorf (x)) != floorf (x))
    link_error ();
  if (truncf (ceilf (x)) != ceilf (x))
    link_error ();

  if (floorf (truncf (x)) != truncf (x))
    link_error ();
  if (floorf (floorf (x)) != floorf (x))
    link_error ();
  if (floorf (ceilf (x)) != ceilf (x))
    link_error ();

  if (ceilf (truncf (x)) != truncf (x))
    link_error ();
  if (ceilf (floorf (x)) != floorf (x))
    link_error ();
  if (ceilf (ceilf (x)) != ceilf (x))
    link_error ();
}

void testl(long double x)
{
  if (truncl (truncl (x)) != truncl (x))
    link_error ();
  if (truncl (floorl (x)) != floorl (x))
    link_error ();
  if (truncl (ceill (x)) != ceill (x))
    link_error ();

  if (floorl (truncl (x)) != truncl (x))
    link_error ();
  if (floorl (floorl (x)) != floorl (x))
    link_error ();
  if (floorl (ceill (x)) != ceill (x))
    link_error ();

  if (ceill (truncl (x)) != truncl (x))
    link_error ();
  if (ceill (floorl (x)) != floorl (x))
    link_error ();
  if (ceill (ceill (x)) != ceill (x))
    link_error ();
}


int main()
{
  test (3.2);
  testf (3.2f);
  testl (3.2l);
  return 0;
}

