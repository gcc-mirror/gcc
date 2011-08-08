/* Prefetching used to prefer nonsensical unroll factor of 5 in this testcase.  */

/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-options "-O2 -fprefetch-loop-arrays -march=athlon -msse2 -mfpmath=sse -fdump-tree-aprefetch-details" } */

#define N 1000000

double a[N];

double test(void)
{
  unsigned i;
  double sum = 0;

  for (i = 0; i < N; i += 2)
    sum += (a[i] * a[i+1]);

  return sum;
}

/* { dg-final { scan-tree-dump-times "unroll factor 4" 1 "aprefetch" } } */
/* { dg-final { cleanup-tree-dump "aprefetch" } } */
