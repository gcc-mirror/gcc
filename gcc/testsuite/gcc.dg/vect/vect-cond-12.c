/* { dg-do compile } */

int
f (int *x, short *y)
{
  int res = 100;
  for (int i = 0; i < 40; ++i)
    {
      if (y[i] > 1)
	res = x[i];
      x[i] += y[i];
    }
  return res;
}
