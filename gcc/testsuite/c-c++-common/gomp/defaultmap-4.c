/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 1000

void
foo (void)
{
  int a[N], b[N], c[N];

  /* Should generate implicit 'map(present, alloc)' clauses.  */
  #pragma omp target defaultmap (present: aggregate)
    for (int i = 0; i < N; i++)
      c[i] = a[i] + b[i];

  /* Should generate implicit 'map(present, alloc)' clauses,
     and they should go before other non-present clauses.  */
  #pragma omp target map(from: c) defaultmap (present: aggregate)
    for (int i = 0; i < N; i++)
      c[i] = a[i] + b[i];
}

/* { dg-final { scan-tree-dump "pragma omp target.*defaultmap\\(present:aggregate\\) map\\(force_present:c \\\[len: \[0-9\]+\\\]\\\[implicit\\\]\\) map\\(force_present:b \\\[len: \[0-9\]+\\\]\\\[implicit\\\]\\) map\\(force_present:a \\\[len: \[0-9\]+\\\]\\\[implicit\\\]\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "pragma omp target.*defaultmap\\(present:aggregate\\) map\\(force_present:b \\\[len: \[0-9\]+\\\]\\\[implicit\\\]\\) map\\(force_present:a \\\[len: \[0-9\]+\\\]\\\[implicit\\\]\\) map\\(from:c \\\[len: \[0-9\]+\\\]\\)" "gimple" } } */
