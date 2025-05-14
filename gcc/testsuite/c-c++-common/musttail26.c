/* PR ipa/119376 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(3, \[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(4, \[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "  foo \\\(\[12], \[^\n\r]*\\\); \\\[tail call\\\]" "optimized" } } */

int foo (int, int);
int v, w[10];

static inline __attribute__((always_inline)) int
bar (int x, int y)
{
  [[gnu::musttail]] return foo (x, y);
}

static int
baz (int x, int y)
{
  [[gnu::musttail]] return foo (x, x + y + (v | y) * (v & y));
}

int
qux (int x, int y)
{
  w[0] = bar (1, x + y);
  w[1] = baz (2, x + y);
  if (x == 42)
    [[gnu::musttail]] return bar (3, x + y);
  if (x == -42)
    [[gnu::musttail]] return baz (4, x + y);
  return 0;
}
