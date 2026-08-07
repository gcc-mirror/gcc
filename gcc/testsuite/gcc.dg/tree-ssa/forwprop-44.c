/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/111959 */

int divbypow2(int a, int b)
{
  if (a & ~0xff) __builtin_unreachable();
  return a / (1<<b);
}

/* divbypow2 should be able to optimize to just a/b as a is known to be always positive. */
/* { dg-final { scan-tree-dump-not " / " "optimized" } } */
/* { dg-final { scan-tree-dump-not " << " "optimized" } } */
/* { dg-final { scan-tree-dump-times " >> " 1 "optimized" } } */
