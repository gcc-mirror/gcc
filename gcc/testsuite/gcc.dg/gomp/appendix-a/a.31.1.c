/* { dg-do compile } */

void
a31_1 (float *x, int *y, int n)
{
  int i, b;
  float a;
  a = 0.0;
  b = 0;
#pragma omp parallel for private(i) shared(x, y, n) \
                         reduction(+:a) reduction(^:b)
  for (i = 0; i < n; i++)
    {
      a += x[i];
      b ^= y[i];
    }
}
