/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-omplower" } */

#define DIM2 17

void f (int **x, int lbound, int ubound, int stride)
{
  #pragma omp target map(to:x) map(iterator(i=lbound:ubound:stride), to: x[i][:DIM2])
    ;
}

/* { dg-final { scan-tree-dump-times "_\[0-9\]+ = ubound - lbound;" 2 "omplower" } } */
/* { dg-final { scan-tree-dump-times "D\\\.\[0-9\]+ = __builtin_malloc \\(D\\\.\[0-9\]+\\);" 2 "omplower" } } */
/* { dg-final { scan-tree-dump-times "__builtin_free \\(omp_iter_data\\\.\[0-9\]+\\);" 2 "omplower" } } */
