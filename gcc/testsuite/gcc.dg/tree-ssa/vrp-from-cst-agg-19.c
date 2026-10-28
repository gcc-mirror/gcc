/* PR tree-optimization/127879 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump " = a\\\[" "optimized" } } */
/* { dg-final { scan-tree-dump " = b.b\\\[" "optimized" } } */

const signed char a[] = { 28, -36, -2, -1, 2, 4, -3, -4, 1, 3, 0, -5, -37, 29 };
const struct { int a; signed char b[]; } b = { 0, { 28, -36, -2, -1, 2, 4, -3, -4, 1, 3, 0, -5, -37, 29 } };

int
foo (int x)
{
  if (x < 2 || x >= 12)
    return -1;
  if (a[x] >= -4 && a[x] <= 5)
    return 1;
  return 0;
}

int
bar (int x)
{
  if (x < 2 || x >= 12)
    return -1;
  if (b.b[x] >= -4 && b.b[x] <= 5)
    return 1;
  return 0;
}
