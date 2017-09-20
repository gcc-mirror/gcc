/* { dg-do compile } */

void
f (long *x, int n)
{
  for (int i = 0; i < n; i++)
    {
      x[i * 2] = 1L << i;
      x[i * 2 + 1] = 1L << i;
    }
}
