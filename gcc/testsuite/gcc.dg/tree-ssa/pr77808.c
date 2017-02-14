/* PR tree-optimization/77808 */
/* { dg-options "-O2 -fprefetch-loop-arrays --param prefetch-latency=0 -w" } */

void
daxpy(int n, double da, double * __restrict dx, double * __restrict dy)
{
  int i;

  for (i = 0;i < n; i++)
    dy[i] = dy[i] + da*dx[i];
}
