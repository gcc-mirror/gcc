/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-ealias-all" } */

void
foo (int *a)
{
  int *p = a;

#pragma acc kernels pcopyin (a[0:1], p[0:1])
  {
    *a = 0;
    *p = 1;
  }
}

/* Only the omp_data_i related loads should be annotated with cliques.  */
/* { dg-final { scan-tree-dump-times "clique 1 base 1" 2 "ealias" } } */
/* { dg-final { scan-tree-dump-times "(?n)clique 1 base 0" 2 "ealias" } } */

