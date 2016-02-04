/* { dg-additional-options "-O2" } */
/* { dg-additional-options "-fipa-pta -fdump-tree-optimized" } */

#ifdef __cplusplus
extern "C" {
#endif
typedef __SIZE_TYPE__ size_t;
void *malloc (size_t);
void free (void *);
#ifdef __cplusplus
}
#endif

#define N 2

void
foo (void)
{
  unsigned int *a = (unsigned int *)malloc (N * sizeof (unsigned int));
  unsigned int *b = (unsigned int *)malloc (N * sizeof (unsigned int));
  unsigned int *c = (unsigned int *)malloc (N * sizeof (unsigned int));

#pragma acc kernels pcopyout (a[0:N], b[0:N], c[0:N])
  {
    a[0] = 0;
    b[0] = 1;
    c[0] = a[0];
  }

  free (a);
  free (b);
  free (c);
}

/* { dg-final { scan-tree-dump-times "(?n)= 0;$" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "(?n)= 1;$" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "(?n)= \\*a" 0 "optimized" } } */
