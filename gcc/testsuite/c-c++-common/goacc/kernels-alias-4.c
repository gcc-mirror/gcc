/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-ealias-all" } */

#define N 2

void
foo (void)
{
  unsigned int a[N];
  unsigned int *p = &a[0];

#pragma acc kernels pcopyin (a, p[0:2])
  {
    a[0] = 0;
    *p = 1;
  }
}

/* Only the omp_data_i related loads should be annotated with
   non-base 0 cliques.  */
/* { dg-final { scan-tree-dump-times "clique 1 base 1" 2 "ealias" } } */
/* { dg-final { scan-tree-dump-times "(?n)clique 1 base 0" 2 "ealias" } } */

