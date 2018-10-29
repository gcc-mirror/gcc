/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-optimized" } */

unsigned int __GIMPLE() f(int a)
{
  int t0;
  unsigned int t1;
  t0 = -a;
  t1 = __ABSU a;
  return t1;
}


/* { dg-final { scan-tree-dump-times "= -" 0 "optimized" } } */
