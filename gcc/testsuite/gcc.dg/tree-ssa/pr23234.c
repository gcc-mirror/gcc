/* The problem in this PR was mostly finding a suitable place to insert
   the reciprocals of the function arguments.  This test case tries to
   test three possible ways of how this may go wrong.  */
/* { dg-options "-O2 -ffast-math" } */
/* { dg-do compile } */

/* The original test case.  */
double
f1 (double a, double b, double c)
{
  double y0;

  if (a == 0.0)
    {
      y0 = -c / b;
      return y0;
    }
  y0 = c / b;
  return y0;
}

/* Labels may end up in the middle of a block.  Also bad.  */
double
f2 (double a, double b, double c)
{
  double y0;

a_label:
another_label:
  if (a == 0.0)
    {
      y0 = -c / b;
      return y0;
    }
  y0 = c / b;
  return y0;
}

/* Uses must still be dominated by their defs.  */
double
f3 (double a, double b, double c)
{
  double y0;

  y0 = -c / b;
  if (a == 0.0)
    {
      return y0;
    }
  y0 = c / b;
  return y0;
}
