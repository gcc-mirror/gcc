/* Copyright (C) 2002 Free Software Foundation.
  
   Test floating point negation produces the expected results.
  
   Written by Roger Sayle, 21st May 2002.  */

/* { dg-do run } */
/* { dg-options "-O2 -ffast-math" } */

extern void abort ();


double
dneg (double x)
{
  return -x;
}

double
dmult (double x)
{
  return -1.0 * x;
}

double
ddiv (double x)
{
  return x / -1.0;
}


float
fneg (float x)
{
  return -x;
}

float
fmult (float x)
{
  return -1.0f * x;
}

float
fdiv (float x)
{
  return x / -1.0f;
}


void
ftest(float src, float dst)
{
  if (fneg (src) != dst)
    abort ();

  if (src != fneg (dst))
    abort ();

  if (fmult (src) != dst)
    abort ();

  if (src != fmult (dst))
    abort ();

  if (fdiv (src) != dst)
    abort ();

  if (src != fdiv(dst))
    abort ();
}

void
dtest(double src, double dst)
{
  if (dneg (src) != dst)
    abort ();

  if (src != dneg (dst))
    abort ();

  if (dmult (src) != dst)
    abort ();

  if (src != dmult (dst))
    abort ();

  if (ddiv (src) != dst)
    abort ();

  if (src != ddiv(dst))
    abort ();
}


int
main ()
{
  ftest (1.0f, -1.0f);
  ftest (2.0f, -2.0f);
  ftest (-3.0f, 3.0f);
  ftest (0.0f, -0.0f);
  ftest (-0.0f, 0.0f);

  dtest (1.0, -1.0);
  dtest (2.0, -2.0);
  dtest (-3.0, 3.0);
  dtest (0.0, -0.0);
  dtest (-0.0, 0.0);

  return 0;
}

