/* Copyright (C) 2004 Free Software Foundation.

   Ensure that the composite comparison optimization doesn't misfire
   and attempt to combine an integer comparison with a floating-point one.

   Written by Paolo Bonzini, 26th May 2004.  */

extern void abort (void);

int
foo (double x, double y)
{
  /* If miscompiled the following may become false.  */
  return (x > y) && ((int)x == (int)y);
}

int
main ()
{
  if (! foo (1.3,1.0))
    abort ();
  return 0;
}

