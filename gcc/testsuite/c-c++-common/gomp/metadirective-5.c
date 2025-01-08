/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */

#define N 100

void
f (int a[], int flag)
{
  int i;
  #pragma omp metadirective \
	when (user={condition(flag)}: \
		target teams distribute parallel for map(from: a[0:N])) \
	default (parallel for)
  for (i = 0; i < N; i++)
    a[i] = i;
}

/* The metadirective should be resolved at parse time.  */

/* { dg-final { scan-tree-dump-not "#pragma omp metadirective" "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp target" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp teams" 1 "original" } } */
/* { dg-final { scan-tree-dump-times  "#pragma omp distribute" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp parallel" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp for" 2 "original" } } */
