/* PR c/119311 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = bar \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = baz \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "  (?:bar|baz) \\\(\[^\n\r]*\\\); \\\[tail call\\\]" "optimized" } } */


[[gnu::noipa]] int
bar (int x, int y)
{
  return x + y;
}

[[gnu::noipa]] int
baz (int x, int y)
{
  return x * y;
}

int
foo (int a, int b)
{
  if (a > b)
    [[gnu::musttail]] return bar (a - b, b);
  else
    [[gnu::musttail]] return baz (a, b - a);
}
