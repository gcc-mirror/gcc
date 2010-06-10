/* { dg-do compile { target i?86-*-* x86_64-*-* } } */
/* { dg-require-effective-target ilp32 } */
/* { dg-require-effective-target sse2 } */
/* { dg-skip-if "" { i?86-*-* x86_64-*-* } { "-march=*" } { "-march=athlon" } } */
/* { dg-options "-O2 -fprefetch-loop-arrays -march=athlon -msse2 -mfpmath=sse --param simultaneous-prefetches=100 -fdump-tree-aprefetch-details -fdump-tree-optimized" } */

#define K 1000000
int a[K], b[K];

void test()
{
  unsigned i;

  /* Nontemporal store should be used for a, nontemporal prefetch for b.  */
  for (i = 0; i < K; i++)
    a[i] = b[i];

}

/* { dg-final { scan-tree-dump-times "Issued nontemporal prefetch" 1 "aprefetch" } } */
/* { dg-final { scan-tree-dump-times "a nontemporal store" 1 "aprefetch" } } */

/* { dg-final { scan-tree-dump-times "builtin_prefetch" 1 "optimized" } } */
/* { dg-final { scan-tree-dump "=\\{nt\\}" "optimized" } } */
/* { dg-final { scan-tree-dump-times "__builtin_ia32_mfence" 1 "optimized" } } */

/* { dg-final { scan-assembler-times "prefetchnta" 1 } } */
/* { dg-final { scan-assembler "movnti" } } */
/* { dg-final { scan-assembler-times "mfence" 1 } } */

/* { dg-final { cleanup-tree-dump "aprefetch" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
