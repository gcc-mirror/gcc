/* { dg-do compile } */
/* { dg-additional-options "-fdump-tree-original" } */
/* { dg-additional-options "-fdump-tree-gimple" } */
/* { dg-additional-options "-fdump-tree-optimized" } */

#define N 100

void f (int x[], int y[], int z[])
{
  int i;

  #pragma omp target map(to: x, y) map(from: z)
    #pragma omp metadirective \
	when (device={arch("nvptx")}: teams loop) \
	default (parallel loop)
      for (i = 0; i < N; i++)
	z[i] = x[i] * y[i];
}

/* The metadirective should be resolved after Gimplification.  */

/* { dg-final { scan-tree-dump-times "#pragma omp metadirective" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "when \\(device arch .nvptx.\\):" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp teams" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "default:" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp parallel" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "#pragma omp loop" 2 "original" } } */

/* { dg-final { scan-tree-dump-times "#pragma omp metadirective" 1 "gimple" } } */

/* { dg-final { scan-tree-dump-not "#pragma omp metadirective" "optimized" } } */
