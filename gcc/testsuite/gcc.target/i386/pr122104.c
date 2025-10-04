/* PR tree-optimization/122104 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-widening_mul-alias" } */
/* { dg-final { scan-tree-dump "\\.MUL_OVERFLOW" "widening_mul" } } */
/* { dg-final { scan-tree-dump-not "# RANGE \\\[irange\\\] unsigned int \\\[1, " "widening_mul" } } */

int
foo (int x)
{
  int r = (unsigned) x * 35;
  return x && ((unsigned) r / x) != 35U;
}
