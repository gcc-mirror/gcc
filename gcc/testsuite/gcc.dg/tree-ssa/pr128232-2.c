/* { dg-do compile { target { i?86-*-* x86_64-*-* } } } */
/* { dg-require-effective-target lp64 } */
/* { dg-options "-O2 -fno-tree-vectorize -fno-tree-loop-distribute-patterns -fprefetch-loop-arrays -fdump-tree-aprefetch-details" } */

/* Each iteration reads and writes a 4-byte int.  The 4 GiB total volume
   must not wrap to zero and suppress non-temporal stores.  */

void
store (int *__restrict b, const int *__restrict c, int scalar)
{
  for (unsigned long j = 0; j < 536870912UL; ++j)
    b[j] = scalar * c[j];
}

/* { dg-final { scan-tree-dump-times {reference [0-9]+:[0-9]+ distance 4294967295} 2 "aprefetch" } } */
/* { dg-final { scan-tree-dump-not {reference [0-9]+:[0-9]+ distance 0(\n|$)} "aprefetch" } } */
