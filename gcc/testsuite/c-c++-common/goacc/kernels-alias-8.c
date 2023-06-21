/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-ealias-all" } */

typedef __SIZE_TYPE__ size_t;
extern void *acc_copyin (void *, size_t);

void
foo (int *a, size_t n)
{
  int *p = (int *)acc_copyin (a, n);

#pragma acc kernels deviceptr (p) pcopy(a[0:n])
  {
    a = 0;
    *p = 1;
  }
}

/* Only the omp_data_i related loads should be annotated with cliques.
   Note ccp can remove one of the omp_data_i loads which is why there
   is there only one clique base still there.  */
/* { dg-final { scan-tree-dump-times "clique 1 base 1" 1 "ealias" } } */
/* { dg-final { scan-tree-dump-times "(?n)clique 1 base 0" 2 "ealias" } } */

