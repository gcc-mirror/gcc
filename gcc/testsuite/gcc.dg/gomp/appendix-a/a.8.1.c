/* { dg-do compile } */

#include <math.h>
void
a8 (int n, int m, float *a, float *b, float *y, float *z)
{
  int i;
#pragma omp parallel
  {
#pragma omp for nowait
    for (i = 1; i < n; i++)
      b[i] = (a[i] + a[i - 1]) / 2.0;
#pragma omp for nowait
    for (i = 0; i < m; i++)
      y[i] = sqrt (z[i]);
  }
}
