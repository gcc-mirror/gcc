/* PR tree-optimization/93156 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "return 0;" 3 "optimized" } } */

int
foo (int x)
{
  return (x * x) & 2;
}

unsigned long long
bar (unsigned long long x)
{
  return (x * x) & 2;
}

int
baz (int x)
{
  x &= -2;
  return (x * x) & 3;
}
