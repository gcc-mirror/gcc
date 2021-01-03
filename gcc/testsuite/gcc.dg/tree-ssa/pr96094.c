/* PR tree-optimization/96094 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 34;" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return y_\[0-9]*\\\(D\\\);" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return \[^\n\r;]*;" 4 "optimized" } } */

int
foo (int x)
{
  if (x >= 2U)
    return 34;
  return 34 / x;
}

int
bar (int x, int y)
{
  if (x >= 2U)
    return y;
  return y / x;
}

int
baz (_Bool x)
{
  return 34 / x;
}

int
qux (_Bool x, int y)
{
  return y / x;
}
