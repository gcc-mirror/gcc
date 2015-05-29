/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

int posix_memalign(void **memptr, __SIZE_TYPE__ alignment, __SIZE_TYPE__ size);
void bar (int *);

int *foo (int n)
{
  int *p;
  int *q;
  void *tem;
  if (posix_memalign (&tem, 256, n * sizeof (int)) != 0)
    return (void *)0;
  p = (int *)tem;
  if (posix_memalign (&tem, 256, n * sizeof (int)) != 0)
    return (void *)0;
  q = (int *)tem;
  bar (q);
  int i;
  for (i = 0; i < n; ++i)
    p[i] = q[i] + q[i];
  return p;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "Peeling for alignment will be applied" "vect" } } */
/* { dg-final { scan-tree-dump-not "Vectorizing an unaligned access" "vect" } } */
