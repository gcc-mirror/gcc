/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-optimized" } */

int __GIMPLE() f(int c)
{
  int D;
  int _1;
  unsigned int _2;
  _1 = __ABS c;
  _2 = __ABSU _1;
  D = (int) _2;
  return D;
}


/* { dg-final { scan-tree-dump-times "ABSU" 0 "optimized" } } */
