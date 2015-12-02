/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 2

void
foo (void)
{
  unsigned int a[N];

#pragma acc kernels
  {
    a[0]++;
  }
}

/* { dg-final { scan-tree-dump-times "map\\(tofrom" 1 "gimple" } } */
