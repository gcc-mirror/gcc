/* Copyright (C) 2004 Free Software Foundation.

   Check that constant folding of signbit, signbitf and signbitl math
   functions doesn't break anything and produces the expected results.

   Written by Roger Sayle, 28th January 2004.  */

/* { dg-do run } */
/* { dg-options "-O2" } */

extern void abort(void);

extern int signbit(double);
extern int signbitf(float);
extern int signbitl(long double);

int test (double x)
{
  return signbit(x);
}

int testf (float x)
{
  return signbitf(x);
}

int testl (long double x)
{
  return signbitl(x);
}


int main()
{
  if (test (0.0) != 0)
    abort ();
  if (test (1.0) != 0)
    abort ();
  if (test (-2.0) == 0)
    abort ();

  if (testf (0.0f) != 0)
    abort ();
  if (testf (1.0f) != 0)
    abort ();
  if (testf (-2.0f) == 0)
    abort ();

  if (testl (0.0l) != 0)
    abort ();
  if (testl (1.0l) != 0)
    abort ();
  if (testl (-2.0l) == 0)
    abort ();

  return 0;
}

