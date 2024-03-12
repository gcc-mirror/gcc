/* PR tree-optimization/110134 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int fu(unsigned a, unsigned b)
{
  a = -a;
  b = -b;
  return a != b;
}
int fs(signed a, signed b)
{
  a = -a;
  b = -b;
  return a != b;
}

/* We should have optimized out the a = -; statements. */
/* { dg-final { scan-tree-dump-not "= -a" "optimized" } } */
/* { dg-final { scan-tree-dump-not "= -b" "optimized" } } */
