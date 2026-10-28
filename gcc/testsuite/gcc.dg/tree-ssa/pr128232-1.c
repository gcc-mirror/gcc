/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-require-effective-target double64 } */
/* { dg-options "-O2 -fno-tree-vectorize -fprefetch-loop-arrays --param l1-cache-line-size=64 --param l2-cache-size=512 -fdump-tree-aprefetch-details" } */

/* The initial volume estimate, 64 * N * N = 4 GiB, must not wrap to zero.
   Self-reuse reduces the saturated estimate to 64 * N = 512 KiB.  */
#define N 8192

double a[N][N];

double
sum_columns (void)
{
  unsigned i, j;
  double sum = 0;

  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      sum += a[j][i];

  return sum;
}

/* { dg-final { scan-tree-dump-times {reference [0-9]+:[0-9]+ distance 524288} 1 "aprefetch" } } */
/* { dg-final { scan-tree-dump-not {reference [0-9]+:[0-9]+ distance 0(\n|$)} "aprefetch" } } */
