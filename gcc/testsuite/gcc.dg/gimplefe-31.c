/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-optimized" } */


unsigned int __GIMPLE() f(int a)
{
  unsigned int t0;
  int t1;
  unsigned int t2;
  t0 = __ABSU a;
  t1 = (int) t0;
  t2 = __ABSU t1;
  return t2;
}

/* { dg-final { scan-tree-dump-times "ABSU" 1 "optimized" } } */
