/* Copyright (C) 2004 Free Software Foundation.

   Check that constant folding of round, roundf and roundl math functions
   doesn't break anything and produces the expected results.

   Written by Roger Sayle, 22nd January 2004.  */

/* { dg-do link } */
/* { dg-options "-O2" } */

extern void link_error(void);

extern double round(double);
extern float roundf(float);
extern long double roundl(long double);

void test()
{
  if (round (0.0) != 0.0)
    link_error ();
  if (round (6.0) != 6.0)
    link_error ();
  if (round (-8.0) != -8.0)
    link_error ();

  if (round (3.2) != 3.0)
    link_error ();
  if (round (-2.8) != -3.0)
    link_error ();
  if (round (0.01) != 0.0)
    link_error ();
  if (round (-0.7) != -1.0)
    link_error ();

  if (round (2.5) != 3.0)
    link_error ();
  if (round (-1.5) != -2.0)
    link_error ();
}

void testf()
{
  if (roundf (0.0f) != 0.0f)
    link_error ();
  if (roundf (6.0f) != 6.0f)
    link_error ();
  if (roundf (-8.0f) != -8.0f)
    link_error ();

  if (roundf (3.2f) != 3.0f)
    link_error ();
  if (roundf (-2.8f) != -3.0f)
    link_error ();
  if (roundf (0.01f) != 0.0f)
    link_error ();
  if (roundf (-0.7f) != -1.0f)
    link_error ();

  if (roundf (2.5f) != 3.0f)
    link_error ();
  if (roundf (-1.5f) != -2.0f)
    link_error ();
}

void testl()
{
  if (roundl (0.0l) != 0.0l)
    link_error ();
  if (roundl (6.0l) != 6.0l)
    link_error ();
  if (roundl (-8.0l) != -8.0l)
    link_error ();

  if (roundl (3.2l) != 3.0l)
    link_error ();
  if (roundl (-2.8l) != -3.0l)
    link_error ();
  if (roundl (0.01l) != 0.0l)
    link_error ();
  if (roundl (-0.7l) != -1.0l)
    link_error ();

  if (roundl (2.5l) != 3.0l)
    link_error ();
  if (roundl (-1.5l) != -2.0l)
    link_error ();
}

int main()
{
  test ();
  testf ();
  testl ();
  return 0;
}

