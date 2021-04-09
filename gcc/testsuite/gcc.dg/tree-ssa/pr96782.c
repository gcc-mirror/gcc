/* PR tree-optimization/96782 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 0;" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "return 1;" 1 "optimized" } } */

int
foo (int a)
{
  return a == ~a;
}

int
bar (int b)
{
  return ~b != b;
}
