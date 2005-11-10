/* { dg-do compile } */
/* { dg-options "-O1 -fschedule-insns2 -fsched2-use-traces" } */

extern double f (double x);

double g (int a)
{
  int b, c, d, e = 0;
  double h;

  for (d = 0; d < a; d++)
    for (c = 0; c < a; c++)
      b = 1;

  h = (double) e / (double) a;

  if (h)
    {
      h = 1.0 / h;
      h = f (h);
    }
  else
    h = 1.0;

  return h;
}
