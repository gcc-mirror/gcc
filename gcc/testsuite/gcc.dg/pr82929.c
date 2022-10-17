/* PR tree-optimization/82929 */
/* { dg-do compile { target store_merge } } */
/* { dg-options "-O2 -fno-tree-vectorize -fdump-tree-store-merging" } */

void
foo (short *p, short *q, short *r)
{
  short a = q[0];
  short b = q[1];
  short c = ~a;
  short d = r[0];
  short e = r[1];
  short f = ~b;
  p[0] = c & d;
  p[1] = e & f;
}

/* { dg-final { scan-tree-dump-times "Merging successful" 1 "store-merging" { target { ! arm*-*-* } } } } */
