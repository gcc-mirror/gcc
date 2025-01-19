/* { dg-do compile { target { c || c++11 } } } */
/* { dg-options "-fopenmp -std=c23" { target { c } } } */
/* { dg-additional-options "-fdump-tree-gimple" } */

#define N 100

void
f (int x[], int y[], int z[])
{
  int i;

  [[omp::sequence (directive (target map(to: x, y) map(from: z)),
		   directive (metadirective
			      when (device={arch("nvptx")}: teams loop)
			      default (parallel loop)))]]
   for (i = 0; i < N; i++)
     z[i] = x[i] * y[i];
}

/* If offload device "nvptx" isn't supported, the front end can eliminate
   that alternative and not produce a metadirective at all.  Otherwise this
   won't be resolved until late.  */
/* { dg-final { scan-tree-dump-not "#pragma omp metadirective" "gimple" { target { ! offload_nvptx } } } } */
/* { dg-final { scan-tree-dump "#pragma omp metadirective" "gimple" { target { offload_nvptx } } } } */
