/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-fopenmp -std=c23" { target { c } } } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 100

#pragma omp declare target
void
f (double a[], double x) {
  int i;

  [[omp::directive (metadirective
	when (construct={target}: distribute parallel for)
	default (parallel for simd))]]
    for (i = 0; i < N; i++)
      a[i] = x * i;
}
#pragma omp end declare target

int
main (void)
{
  double a[N];

#pragma omp target teams map(from: a[0:N])
  f (a, 3.14159);

  f (a, 2.71828);

  return 0;
 }

 /* The metadirective should be resolved during Gimplification.  */

/* { dg-final { scan-tree-dump-times "#pragma omp metadirective" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "when \\(construct = .*target.*\\):" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "otherwise:" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp parallel" 2 "original" } } */

/* { dg-final { scan-tree-dump-not "#pragma omp metadirective" "gimple" } } */
