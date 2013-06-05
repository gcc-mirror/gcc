/* { dg-do compile } */
/* { dg-options "-O3 -fdump-tree-gimple -fcilkplus" } */

int *a, *b;

void foo()
{
#pragma simd vectorlength(8)
  for (int i=0; i < 1000; ++i)
    a[i] = b[i];
}

/* { dg-final { scan-tree-dump-times "safelen\\(8\\)" 1 "gimple" } } */
/* { dg-final { cleanup-tree-dump "gimple" } } */
