/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fipa-pta -fdump-tree-optimized" } */

#define N 2

void
foo (void)
{
  unsigned int a[N];
  unsigned int b[N];
  unsigned int c[N];

#pragma acc kernels pcopyout (a, b, c)
  {
    a[0] = 0;
    b[0] = 1;
    c[0] = a[0];
  }
}

/* XFAIL: see PR middle-end/95622; fails if ENABLE_OFFLOAD is set.  */
/* { dg-final { scan-tree-dump-times "(?n)= 0;$" 2 "optimized" { xfail offloading_enabled } } } */
/* { dg-final { scan-tree-dump-times "(?n)= 1;$" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "(?n)= \\*_\[0-9\]\\\[0\\\];$" 0 "optimized" } } */
