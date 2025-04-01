/* PR tree-optimization/119493 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2 -fdump-tree-tailr1-details" } */
/* { dg-final { scan-tree-dump-times "tail recursion with accumulation mixed with musttail non-recursive call" 2 "tailr1" } } */

[[gnu::noipa]] int
bar (int x, int y)
{
  return x + y;
}

[[gnu::noinline, gnu::noclone]] int
foo (int x, int y)
{
  if (x < 10)
    [[gnu::musttail]] return bar (x, y);
  if (y & 2)
    return foo (x - 1, y) * 2;
  if (y & 1)
    [[gnu::musttail]] return foo (x - 1, y);
  return foo (x - 1, y) * 3;
}
