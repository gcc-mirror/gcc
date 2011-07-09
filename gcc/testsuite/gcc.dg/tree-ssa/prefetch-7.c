/* { dg-do compile { target { { i?86-*-* x86_64-*-* } && ia32 } } } */
/* { dg-require-effective-target sse2 } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=athlon" } } */
/* { dg-options "-O2 -fprefetch-loop-arrays -march=athlon -msse2 -mfpmath=sse --param simultaneous-prefetches=100 -fdump-tree-aprefetch-details -fdump-tree-optimized" } */

#define K 1000000
int a[K];

void test(int *p)
{
  unsigned i;

  /* Nontemporal store should not be used here (only write and read temporal
     prefetches).  */
  for (i = 0; i < K - 10000; i++)
    a[i + 10000] = a[i];

  /* Nontemporal store should not be used here (only write and read nontemporal
     prefetches).  */
  for (i = 0; i < K - 100000; i++)
    a[i + 100000] = a[i];

  /* Nontemporal store should be used neither for a nor for p, as we do not know
     whether they alias or not.  */
  for (i = 0; i < K; i++)
    {
      a[i] = 0;
      *p++ = 1;
    }

  /* Nontemporal store should not be used for a, as we do not know whether its
     value will be reused or not.  */
  for (i = 0; i < 1000; i++)
    a[i] = 0;
}

/* { dg-final { scan-tree-dump-times "Issued prefetch" 5 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times "Issued nontemporal prefetch" 2 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times "a nontemporal store" 0 "aprefetch" } } */

/* { dg-final { scan-tree-dump-times "builtin_prefetch" 7 "optimized" } } */

/* { dg-final { scan-assembler-times "prefetchw" 5 } } */
/* { dg-final { scan-assembler-times "prefetcht" 1 } } */
/* { dg-final { scan-assembler-times "prefetchnta" 1 } } */

/* { dg-final { cleanup-tree-dump "aprefetch" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
