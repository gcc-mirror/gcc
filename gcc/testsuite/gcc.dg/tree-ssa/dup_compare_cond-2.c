/* { dg-do compile } */
/* { dg-options "-fgimple -O2 -fdump-tree-optimized " } */
/* PR middle-end/105715 */

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

/* At -O2 we should have duplicate the comparison. */
/* { dg-final { scan-tree-dump-times " == " 2 "optimized" } } */
