/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-forwprop1 -fdump-tree-optimized" } */

unsigned char foo(int x)
{
  int t = -x;
  unsigned char t1 = t;
  unsigned char t2 = t;
  /* We may not rewrite this as (unsigned char)(t - x).  */
  return t1 + t2;
}

/* { dg-final { scan-tree-dump-times "x_" 1 "forwprop1" } } */
/* { dg-final { scan-tree-dump-times "x_" 1 "optimized" } } */
