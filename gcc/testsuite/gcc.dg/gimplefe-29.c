/* { dg-do compile } */
/* { dg-options "-O -fgimple -fdump-tree-ssa-gimple" } */

unsigned int __GIMPLE() f(int a)
{
  unsigned int t0;
  t0_1 = __ABSU a;
  return t0_1;
}

/* { dg-final { scan-tree-dump "__ABSU a" "ssa" } } */
