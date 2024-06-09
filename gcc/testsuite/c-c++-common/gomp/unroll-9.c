/* { dg-additional-options "-O2 -fdump-tree-gimple" } */

void bar (int);

void
foo (void)
{
  #pragma omp unroll full
  for (int i = 1; i <= 100; i += 6)
    bar (i);
}

/* { dg-final { scan-tree-dump "\.ANNOTATE \\\(\[^\n\r]*, 1, 17\\\);" "gimple" } } */
