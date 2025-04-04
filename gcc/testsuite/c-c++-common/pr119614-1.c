/* PR tree-optimization/119614 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2" } */

volatile int v;

[[gnu::noinline]] const char *
foo (int x)
{
  v += x;
  return 0;
}

const char *
bar (int x)
{
  if (x == 42)
    [[gnu::musttail]] return foo (42);
  [[gnu::musttail]] return foo (32);
}

const char *
baz (int x)
{
  if (x == 5)
    return foo (42);
  return foo (32);
}
