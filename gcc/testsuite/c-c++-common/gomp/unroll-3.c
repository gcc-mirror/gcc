/* { dg-additional-options "-O2 -fdump-tree-cunroll -fdump-tree-original -fdump-tree-gimple" } */

extern void dummy (int);

void
test1 (void)
{
#pragma omp unroll full
  for (int i = 0; i < 10; i++)
    dummy (i);
}

/* Loop should be removed with 10 copies of the body remaining */
/* { dg-final { scan-tree-dump-times "dummy" 10 "cunroll" } } */
/* { dg-final { scan-tree-dump "#pragma omp unroll" "original" } } */
/* { dg-final { scan-tree-dump-not "#pragma omp" "gimple" } } */
