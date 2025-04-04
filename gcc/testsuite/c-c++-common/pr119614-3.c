/* PR tree-optimization/119614 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2" } */

volatile int v;

[[gnu::noinline]] double
foo (int x)
{
  v += x;
  return 0.5;
}

double
bar (int x)
{
  if (x == 42)
    [[gnu::musttail]] return foo (42);
  [[gnu::musttail]] return foo (32);
}

double
baz (int x)
{
  if (x == 5)
    return foo (42);
  return foo (32);
}
