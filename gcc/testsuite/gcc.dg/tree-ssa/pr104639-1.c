/* PR tree-optimization/104639 */
/* { dg-do compile } */
/* { dg-options "-O2 -g -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-not "PHI <" "optimized" } } */
/* { dg-final { scan-tree-dump-times "i_\[0-9]*\\\(D\\\) != 0;" 1 "optimized" } } */

_Bool
foo (int i)
{
  while (i == 4)
    i += 2;
  return i;
}
