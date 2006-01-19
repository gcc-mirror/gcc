/* { dg-do compile } */
/* { dg-options "-fopenmp -fdump-tree-ompexp" } */

extern void bar(int);

void foo (void)
{
  #pragma omp master
    bar(0);
}

/* { dg-final { scan-tree-dump-times "omp_get_thread_num" 1 "ompexp" } } */
/* { dg-final { cleanup-tree-dump "ompexp" } } */
