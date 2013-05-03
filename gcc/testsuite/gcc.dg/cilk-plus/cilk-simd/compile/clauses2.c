/* { dg-do compile } */
/* { dg-options "-O3 -std=c99 -fcilkplus -fdump-tree-original" } */

volatile int *a, *b;

void foo()
{
  int i, j, k;

#pragma simd linear(j : 4, k) vectorlength(4)
  for (i=0; i < 1000; ++i)
    a[i] = b[j];
}

/* { dg-final { scan-tree-dump-times "linear\\(j:4\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "linear\\(k:1\\)" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "safelen\\(4\\)" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
