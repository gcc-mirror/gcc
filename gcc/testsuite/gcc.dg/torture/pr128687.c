/* { dg-do run } */
/* { dg-additional-options "-fsigned-zeros" } */

__attribute__((noipa)) int
f (int n, double m)
{
  double x = 0.0;
  int s = 0;
  int i;
  for (i = 0 ; i < n; i++)
    {
      double y = x * m;		/* m == -1.0: x is +0.0, then -0.0.  */
      if (y != x)
	break;
      s += __builtin_signbit (x) != 0;
      x = y;
    }
  return s;
}

int
main (void)
{
  if (f (2, -1.0) != 1)
    __builtin_abort ();
  return 0;
}
