/* Copyright (C) 2003 Free Software Foundation.

   Check that constant folding (c1 - x) op c2 into x swap(op) c1-c2
   doesn't break anything.

   Written by Roger Sayle, 27th March 2003.  */

/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern void abort (void);

int foo(double x)
{
  return (10.0 - x) > 3.0;
}

int bar (double x)
{
  return (10.0 - x) == 5.0;
}

int main()
{
  if (foo (8.0))
    abort ();

  if (! foo (6.0))
    abort ();

  if (bar (1.0))
    abort ();

  if (! bar (5.0))
    abort ();
  return 0;
}

