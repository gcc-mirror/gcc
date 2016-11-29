/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -fprefetch-loop-arrays -march=amdfam10 --param simultaneous-prefetches=100 --param min-insn-to-prefetch-ratio=6 -fdump-tree-aprefetch-details" } */

#define N 1000
#define K 900

double a[N][N];

double test(void)
{
  unsigned i, j;
  double sum = 0;

  /* Here, we should use non-temporal prefetch instruction.  */
  for (i = 0; i < K; i++)
    for (j = 0; j < K; j++)
      sum += a[i][j];

  /* Here, we should not use non-temporal prefetch instruction, since the
     value of a[i+10][j] is reused in L2 cache.  */
  for (i = 0; i < K; i++)
    for (j = 0; j < K; j++)
      sum += a[i][j] * a[i + 10][j];

  /* Here, we should use non-temporal prefetch instruction, since the
     value of a[i+100][j] is too far to be reused in L2 cache.  */
  for (i = 0; i < K; i++)
    for (j = 0; j < K; j++)
      sum += a[i][j] * a[i + 100][j];

  /* Here, temporal prefetches should be used, since the volume of the
     memory accesses is smaller than L2 cache.  */
  for (i = 0; i < 100; i++)
    for (j = 0; j < 100; j++)
      sum += a[i][j] * a[i + 100][j];

  /* Temporal prefetches should be used here (even though the accesses to
     a[j][i] are independent, the same cache line is almost always hit
     every N iterations).  */
  for (i = 0; i < N; i++)
    for (j = 0; j < N; j++)
      sum += a[j][i];

  return sum;
}

/* { dg-final { scan-tree-dump-times "Issued prefetch" 5 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times "Issued nontemporal prefetch" 3 "aprefetch" } } */
