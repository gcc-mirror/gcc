/* { dg-do compile } */

void f(long *restrict x, int *restrict y, short *restrict z, int *restrict a)
{
  for (int i = 0; i < 100; i += 4)
    {
      x[i] = (long) y[z[i]] + 1;
      x[i + 1] = (long) y[z[i + 1]] + 2;
      x[i + 2] = (long) y[z[i + 2]] + 3;
      x[i + 3] = (long) y[z[i + 3]] + 4;
      a[i] += 1;
    }
}
