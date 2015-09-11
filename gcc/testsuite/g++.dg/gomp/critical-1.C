/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-omplower" } */

extern void bar(int);

void foo (void)
{
  #pragma omp critical
    bar(0);

  /* Note that "name" is in its own namespace, thus this foo is not
     the same as the function.  */
  #pragma omp critical(foo)
  {
    bar(1);
    bar(2);
  }

  #pragma omp critical
  #pragma omp critical(foo)
    bar(3);
}

/* { dg-final { scan-tree-dump-times "GOMP_critical_start" 2 "omplower" } } */
/* { dg-final { scan-tree-dump-times "GOMP_critical_end" 2 "omplower" } } */
/* { dg-final { scan-tree-dump-times "GOMP_critical_name_start" 2 "omplower" } } */
/* { dg-final { scan-tree-dump-times "GOMP_critical_name_end" 2 "omplower" } } */
