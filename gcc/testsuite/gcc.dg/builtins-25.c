/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding of built-in math functions doesn't
   break anything and produces the expected results.

   Written by Roger Sayle, 28th June 2003.  */

/* { dg-do link } */
/* { dg-options "-O2" } */

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

void test()
{
  if (trunc (0.0) != 0.0)
    link_error ();
  if (floor (0.0) != 0.0)
    link_error ();
  if (ceil (0.0) != 0.0)
    link_error ();

  if (trunc (6.0) != 6.0)
    link_error ();
  if (floor (6.0) != 6.0)
    link_error ();
  if (ceil (6.0) != 6.0)
    link_error ();

  if (trunc (-8.0) != -8.0)
    link_error ();
  if (floor (-8.0) != -8.0)
    link_error ();
  if (ceil (-8.0) != -8.0)
    link_error ();

  if (trunc (3.2) != 3.0)
    link_error ();
  if (floor (3.2) != 3.0)
    link_error ();
  if (ceil (3.2) != 4.0)
    link_error ();

  if (trunc (-2.8) != -2.0)
    link_error ();
  if (floor (-2.8) != -3.0)
    link_error ();
  if (ceil (-2.8) != -2.0)
    link_error ();

  if (trunc (0.01) != 0.0)
    link_error ();
  if (floor (0.01) != 0.0)
    link_error ();
  if (ceil (0.01) != 1.0)
    link_error ();

  if (trunc (-0.7) != 0.0)
    link_error ();
  if (floor (-0.7) != -1.0)
    link_error ();
  if (ceil (-0.7) != 0.0)
    link_error ();
}

void testf()
{
  if (truncf (0.0f) != 0.0f)
    link_error ();
  if (floorf (0.0f) != 0.0f)
    link_error ();
  if (ceilf (0.0f) != 0.0f)
    link_error ();

  if (truncf (6.0f) != 6.0f)
    link_error ();
  if (floorf (6.0f) != 6.0f)
    link_error ();
  if (ceilf (6.0f) != 6.0f)
    link_error ();

  if (truncf (-8.0f) != -8.0f)
    link_error ();
  if (floorf (-8.0f) != -8.0f)
    link_error ();
  if (ceilf (-8.0f) != -8.0f)
    link_error ();

  if (truncf (3.2f) != 3.0f)
    link_error ();
  if (floorf (3.2f) != 3.0f)
    link_error ();
  if (ceilf (3.2f) != 4.0f)
    link_error ();

  if (truncf (-2.8f) != -2.0f)
    link_error ();
  if (floorf (-2.8f) != -3.0f)
    link_error ();
  if (ceilf (-2.8f) != -2.0f)
    link_error ();

  if (truncf (0.01f) != 0.0f)
    link_error ();
  if (floorf (0.01f) != 0.0f)
    link_error ();
  if (ceilf (0.01f) != 1.0f)
    link_error ();

  if (truncf (-0.7f) != 0.0f)
    link_error ();
  if (floorf (-0.7f) != -1.0f)
    link_error ();
  if (ceilf (-0.7f) != 0.0f)
    link_error ();
}

void testl()
{
  if (truncl (0.0l) != 0.0l)
    link_error ();
  if (floorl (0.0l) != 0.0l)
    link_error ();
  if (ceill (0.0l) != 0.0l)
    link_error ();

  if (truncl (6.0l) != 6.0l)
    link_error ();
  if (floorl (6.0l) != 6.0l)
    link_error ();
  if (ceill (6.0l) != 6.0l)
    link_error ();

  if (truncl (-8.0l) != -8.0l)
    link_error ();
  if (floorl (-8.0l) != -8.0l)
    link_error ();
  if (ceill (-8.0l) != -8.0l)
    link_error ();

  if (truncl (3.2l) != 3.0l)
    link_error ();
  if (floorl (3.2l) != 3.0l)
    link_error ();
  if (ceill (3.2l) != 4.0l)
    link_error ();

  if (truncl (-2.8l) != -2.0l)
    link_error ();
  if (floorl (-2.8l) != -3.0l)
    link_error ();
  if (ceill (-2.8l) != -2.0l)
    link_error ();

  if (truncl (0.01l) != 0.0l)
    link_error ();
  if (floorl (0.01l) != 0.0l)
    link_error ();
  if (ceill (0.01l) != 1.0l)
    link_error ();

  if (truncl (-0.7l) != 0.0l)
    link_error ();
  if (floorl (-0.7l) != -1.0l)
    link_error ();
  if (ceill (-0.7l) != 0.0l)
    link_error ();
}

int main()
{
  test ();
  testf ();
  testl ();
  return 0;
}

