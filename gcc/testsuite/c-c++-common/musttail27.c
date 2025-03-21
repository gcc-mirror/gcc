/* PR ipa/119376 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(\[^\n\r]*\\\); \\\[tail call\\\]" 4 "optimized" } } */

int foo (int);

int
bar (int x)
{
  [[gnu::musttail]] return foo (x + 1);
}

int
baz (int x)
{
  return foo (x + 1);
}

int
qux (int x)
{
  return foo (x + 2);
}

int
corge (int x)
{
  [[gnu::musttail]] return foo (x + 2);
}
