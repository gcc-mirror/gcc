/* PR tree-optimization/82929 */
/* { dg-do compile { target store_merge } } */
/* { dg-options "-O2 -fdump-tree-store-merging" } */

void
foo (short *p, short *q, short *r)
{
  p = __builtin_assume_aligned (p, __alignof__ (int));
  q = __builtin_assume_aligned (q, __alignof__ (int));
  r = __builtin_assume_aligned (r, __alignof__ (int));
  short a = q[0];
  short b = q[1];
  short c = ~a;
  short d = r[0];
  short e = r[1];
  short f = ~b;
  p[0] = c & d;
  p[1] = e & f;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 1 "store-merging" } } */
