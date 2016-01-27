/* { dg-do compile } */

void fn2(double *e, double a)
{
  int b = 0;
  for (; b < 256; b++)
    {
      int c = 0;
      double x = e[b];
      for (; c < 256; ++c)
	x /= a;
      e[b] = x;
    }
}
