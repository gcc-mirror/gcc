/* PR tree-optimization/94786 */
/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "= ~\[xy\]_" 4 "optimized" } } */
/* { dg-final { scan-tree-dump-times " & \[xy\]_" 4 "optimized" } } */

unsigned
foo_u(unsigned x, unsigned y)
{
  return (x | y) - y;
}

int
foo_i(int x, int y)
{
  return (x | y) - y;
}

unsigned long long
foo_ull(unsigned long long x, unsigned long long y)
{
  return (x | y) - y;
}

long long
foo_ll(long long x, long long y)
{
  return (x | y) - y;
}
