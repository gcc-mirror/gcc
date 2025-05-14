/* PR tree-optimization/119483 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "bar\[.a-z0-9]* \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "baz \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */

[[gnu::noreturn]] extern void foo (void);

[[gnu::noinline]] static int
bar (int x)
{
  (void) x;
  foo ();
  return 0;
}

[[gnu::noipa]] int
baz (int x)
{
  return x + 42;
}

int
qux (int x)
{
  if (x == 1)
    [[gnu::musttail]] return bar (1);
  [[gnu::musttail]] return baz (x);
}
