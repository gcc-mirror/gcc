/* { dg-do compile } */
/* { dg-additional-options "-fopenmp-ompt=basic -fdump-tree-ompexp" } */

void
foo (void)
{
  #pragma omp single
    ;
}

/* { dg-final { scan-tree-dump-times "GOMP_single_start_with_end" 1 "ompexp" } } */
/* { dg-final { scan-tree-dump-times "GOMP_single_end" 1 "ompexp" } } */
/* { dg-final { scan-tree-dump-not "GOMP_single_start \\(" "ompexp" } } */
