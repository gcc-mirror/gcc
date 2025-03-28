/* PR ipa/119484 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "bar\[.a-z0-9]* \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */

void foo (int);

[[gnu::noinline]] static int
bar (int x)
{
  foo (x);
  return 0;
}

int
baz (int x)
{
  if (x == 1)
    [[gnu::musttail]] return bar (x);
  return 0;
}
