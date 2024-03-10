/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 1000

void
foo (void)
{
  int a[N], b[N], c, d, e;

  /* Should be able to parse present in to/from clauses of 'target update'.  */
  #pragma omp target update to(c) to(present: a) from(d) from(present: b) to(e)
}

/* { dg-final { scan-tree-dump "#pragma omp target update to\\(e \\\[len: \[0-9\]+\\\]\\) from\\(present:b \\\[len: \[0-9\]+\\\]\\) from\\(d \\\[len: \[0-9\]+\\\]\\) to\\(present:a \\\[len: \[0-9\]+\\\]\\) to\\(c \\\[len: \[0-9\]+\\\]\\)" "gimple" } } */
