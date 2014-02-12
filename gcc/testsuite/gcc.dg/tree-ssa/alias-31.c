/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-sra -fdump-tree-cddce1" } */

extern int posix_memalign(void **memptr,
			  __SIZE_TYPE__ alignment, __SIZE_TYPE__ size);

int foo (float *p)
{
  int res = *p;
  struct { void *q1; void *q2; } q;
  if (posix_memalign (&q.q1, 128, 128 * sizeof (int)) != 0)
    return 0;
  if (posix_memalign (&q.q2, 128, 128 * sizeof (int)) != 0)
    return 0;
  *((int *)q.q1) = 1;
  *((int *)q.q2) = 2;
  return res + *p + *((int *)q.q1) + *((int *)q.q2);
}

/* There should be only one load from *p left.  All stores and all
   other loads should be removed.  */

/* { dg-final { scan-tree-dump-times "\\\*\[^ \]" 1 "cddce1" } } */
/* { dg-final { cleanup-tree-dump "cddce1" } } */
