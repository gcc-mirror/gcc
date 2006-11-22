/* With -ffast-math, the latice value for t changes from -0.0 to 0.0 in this
   testcase.  */

/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math" } */

double test (int param)
{
  double a = 0.0, b = -1.0, t;
  int i;

  for (i = 0; i < 100; i++)
    {
      t = a * b;
      if (param)
	b = 2.0;
    }

  return t;
}
