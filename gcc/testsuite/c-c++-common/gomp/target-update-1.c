/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 1000

void
foo (void)
{
  int a[N], b[N];

  /* Should be able to parse present in to/from clauses of 'target update'.  */
  #pragma omp target update to(present: a) from(present: b)
}

/* { dg-final { scan-tree-dump "pragma omp target update from\\(present:b \\\[len: \[0-9\]+\\\]\\) to\\(present:a \\\[len: \[0-9\]+\\\]\\)" "gimple" } } */
