/* { dg-do compile } */
/* { dg-options "-fgimple -O0 -fdump-tree-optimized " } */
/* PR tree-optimization/116101 */

int __GIMPLE() f(int a, int b, int c, int d, int e)
{
  _Bool t;
  int ff;
  int gg;
  int res;
  t = a == b;
  ff = t ? a : e;
  gg = t ? d : b;
  res = ff+gg;
  return res;
}

/* At -O0 we should not duplicate the comparison. */
/* { dg-final { scan-tree-dump-times " == " 1 "optimized" } } */
