/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fdump-tree-ealias-all" } */

typedef __SIZE_TYPE__ size_t;
extern void *acc_copyin (void *, size_t);

#define N 2

void
foo (void)
{
  int a[N];
  int *p = (int *)acc_copyin (&a[0], sizeof (a));

#pragma acc kernels deviceptr (p) pcopy(a)
  {
    a[0] = 0;
    *p = 1;
  }
}

/* Only the omp_data_i related loads should be annotated with cliques.  */
/* { dg-final { scan-tree-dump-times "clique 1 base 1" 2 "ealias" } } */
/* { dg-final { scan-tree-dump-times "(?n)clique 1 base 0" 3 "ealias" } } */

