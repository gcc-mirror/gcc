/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 1000

void
foo (void)
{
  int a[N], b[N], c[N];

  /* Should be able to parse 'present' map modifier.  */
  #pragma omp target enter data map (present, to: a, b)

  #pragma omp target data map (present, to: a, b) map (always, present, from: c)

  #pragma omp target map (present, to: a, b) map (present, from: c)
    for (int i = 0; i < N; i++)
      c[i] = a[i] + b[i];

  #pragma omp target exit data map (always, present, from: c)

  /* Map clauses with 'present' modifier should go ahead of those without.  */
  #pragma omp target map (to: a) map (present, to: b) map (from: c)
    for (int i = 0; i < N; i++)
      c[i] = a[i] + b[i];
}

/* { dg-final { scan-tree-dump "pragma omp target enter data map\\(force_present:b \\\[len: \[0-9\]+\\\]\\) map\\(force_present:a \\\[len: \[0-9\]+\\\]\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "pragma omp target data map\\(force_present:b \\\[len: \[0-9\]+\\\]\\) map\\(force_present:a \\\[len: \[0-9\]+\\\]\\) map\\(always,present,from:c \\\[len: \[0-9\]+\\\]\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "pragma omp target.*map\\(force_present:c \\\[len: \[0-9\]+\\\]\\) map\\(force_present:b \\\[len: \[0-9\]+\\\]\\) map\\(force_present:a \\\[len: \[0-9\]+\\\]\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "pragma omp target exit data map\\(always,present,from:c \\\[len: \[0-9\]+\\\]\\)" "gimple" } } */
/* { dg-final { scan-tree-dump "pragma omp target.*map\\(force_present:b \\\[len: \[0-9\]+\\\]\\) map\\(from:c \\\[len: \[0-9\]+\\\]\\) map\\(to:a \\\[len: \[0-9\]+\\\]\\)" "gimple" } } */
