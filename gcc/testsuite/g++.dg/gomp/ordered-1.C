/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-omplower" } */

extern void bar(int);

void foo (void)
{
  #pragma omp ordered
    bar(0);

  #pragma omp ordered
  {
    bar(1);
    bar(2);
  }
}

/* { dg-final { scan-tree-dump-times "GOMP_ordered_start" 2 "omplower" } } */
/* { dg-final { scan-tree-dump-times "GOMP_ordered_end" 2 "omplower" } } */
