/* { dg-do compile } */
/* { dg-require-effective-target vect_int } */

void bar (int *);

int *foo (int n)
{
  int *p = __builtin_aligned_alloc (256, n * sizeof (int));
  int *q = __builtin_aligned_alloc (256, n * sizeof (int));
  bar (q);
  int i;
  for (i = 0; i < n; ++i)
    p[i] = q[i] + q[i];
  return p;
}

/* { dg-final { scan-tree-dump "LOOP VECTORIZED" "vect" } } */
/* { dg-final { scan-tree-dump-not "Peeling for alignment will be applied" "vect" } } */
/* { dg-final { scan-tree-dump-not "Vectorizing an unaligned access" "vect" } } */
