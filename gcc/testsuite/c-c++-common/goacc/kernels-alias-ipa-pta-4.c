/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fipa-pta -fdump-tree-optimized" } */

void
foo (void)
{
  unsigned int a;
  unsigned int b;
  unsigned int c;

#pragma acc kernels pcopyout (a, b, c)
  {
    a = 0;
    b = 1;
    c = a;
  }
}

/* { dg-final { scan-tree-dump-times "(?n)= 0;$" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "(?n)= 1;$" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "(?n)= \\*_\[0-9\];$" 0 "optimized" } } */
