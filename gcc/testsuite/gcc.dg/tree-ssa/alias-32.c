/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-cddce1" } */

int bar (short *p)
{
  int res = *p;
  struct { int *q1; int *q2; } q;
  q.q1 = __builtin_aligned_alloc (128, 128 * sizeof (int));
  q.q2 = __builtin_aligned_alloc (128, 128 * sizeof (int));
  *q.q1 = 1;
  *q.q2 = 2;
  return res + *p + *q.q1 + *q.q2;
}

/* There should be only one load from *p left.  All stores and all
   other loads should be removed.  Likewise the calls to aligned_alloc.  */

/* { dg-final { scan-tree-dump-times "\\\*\[^ \]" 1 "cddce1" } } */
/* { dg-final { scan-tree-dump-not "aligned_alloc" "cddce1" } } */
/* { dg-final { cleanup-tree-dump "cddce1" } } */
