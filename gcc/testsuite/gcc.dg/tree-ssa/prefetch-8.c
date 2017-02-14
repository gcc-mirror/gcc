/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-options "-O2 -fprefetch-loop-arrays -march=amdfam10 --param simultaneous-prefetches=100 -fdump-tree-aprefetch-details -fdump-tree-optimized" } */

#define K 1000000
int a[K];

void test()
{
  unsigned i;

  /* Nontemporal store should be used for a.  */
  for (i = 0; i < K; i++)
    a[i] = 0;
}

/* { dg-final { scan-tree-dump-times "a nontemporal store" 1 "aprefetch" } } */

/* { dg-final { scan-tree-dump "=\\{nt\\}" "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ia32_mfence" 1 "optimized" } } */
