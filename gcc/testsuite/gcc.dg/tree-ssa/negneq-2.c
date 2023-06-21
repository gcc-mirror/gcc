/* PR tree-optimization/110134 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

int fu(unsigned a)
{
  a = -a;
  return a == 1;
}
int fs(signed a)
{
  a = -a;
  return a == 1;
}

/* We should have optimized out the a = -a; statements. */
/* { dg-final { scan-tree-dump-not "= -a" "optimized" } } */
