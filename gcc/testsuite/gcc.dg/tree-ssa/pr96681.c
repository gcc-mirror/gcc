/* PR tree-optimization/96681 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times " \\^ " 5 "optimized" } } */
/* { dg-final { scan-tree-dump-times " (?:<|>=) 0" 5 "optimized" } } */

int
foo (int x, int y)
{
  return (x < 0) ^ (y < 0);
}

int
bar (int x, int y)
{
  return (x > -1) ^ (y > -1);
}

int
baz (int x, int y)
{
  return (x ^ y) < 0;
}

int
qux (int x, int y)
{
  return (x ^ y) >= 0;
}

int
corge (int x, int y)
{
  return (x >= 0) ^ (y < 0);
}
