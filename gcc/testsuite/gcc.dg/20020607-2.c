/* Copyright (C) 2002 Free Software Foundation.

   Test for correctness of floating point comparisons.

   Written by Roger Sayle, 3rd June 2002.  */

/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern void abort (void);

int test1 (double x, int ok)
{
  if ((x - 1.0) > 0.0)
    {
      if (!ok) abort ();
    }
  else
    if (ok) abort ();
}

int test1f (float x, int ok)
{
  if ((x - 1.0f) > 0.0f)
    {
      if (!ok) abort ();
    }
  else
    if (ok) abort ();
}

int test2 (double x, int ok)
{
  if ((x + 1.0) < 0.0)
    {
      if (!ok) abort ();
    }
  else
    if (ok) abort ();
}

int test2f (float x, int ok)
{
  if ((x + 1.0f) < 0.0f)
    {
      if (!ok) abort ();
    }
  else
    if (ok) abort ();
}


int
main ()
{
  test1 (-2.0, 0);
  test1 ( 0.0, 0);
  test1 ( 2.0, 1);

  test1f (-2.0f, 0);
  test1f ( 0.0f, 0);
  test1f ( 2.0f, 1);

  test2 (-2.0, 1);
  test2 ( 0.0, 0);
  test2 ( 2.0, 0);

  test2f (-2.0f, 1);
  test2f ( 0.0f, 0);
  test2f ( 2.0f, 0);

  return 0;
}

