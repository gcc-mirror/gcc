/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-gimple" } */

#define DIM1 10
#define DIM2 20
#define DIM3 30

void f (int ***x, float ***y, double **z)
{
  #pragma omp target update to (iterator(i=0:DIM1, j=0:DIM2): x[i][j][:DIM3], y[i][j][:DIM3])
  #pragma omp target update from (iterator(i=0:DIM1): z[i][:DIM2])
}

/* { dg-final { scan-tree-dump-times "if \\(i <= 9\\) goto <D\.\[0-9\]+>; else goto <D\.\[0-9\]+>;" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "if \\(j <= 19\\) goto <D\.\[0-9\]+>; else goto <D\.\[0-9\]+>;" 1 "gimple" } } */
/* { dg-final { scan-tree-dump-times "to\\(iterator\\(int i=0:10:1, int j=0:20:1, loop_label=<D\.\[0-9\]+>, elems=omp_iter_data\.\[0-9\]+, index=D\.\[0-9\]+\\):\\*D\.\[0-9\]+" 2 "gimple" } } */
/* { dg-final { scan-tree-dump-times "from\\(iterator\\(int i=0:10:1, loop_label=<D\.\[0-9\]+>, elems=omp_iter_data\.\[0-9\]+, index=D\.\[0-9\]+\\):\\*D\.\[0-9\]+" 1 "gimple" } } */
