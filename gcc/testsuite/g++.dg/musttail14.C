// PR tree-optimization/118430
// { dg-do compile { target musttail } }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = bar \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = baz \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = qux \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = corge \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = freddy \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */

__attribute__ ((noipa)) int
foo (int x)
{
  return x;
}

__attribute__ ((noipa)) int
bar (int x)
{
  return x;
}

__attribute__ ((noipa)) int
baz (int x)
{
  return x;
}

__attribute__ ((noipa)) int
qux (int x)
{
  return x;
}

__attribute__ ((noipa)) int
corge (int x)
{
  return x;
}

__attribute__ ((noipa)) int
freddy (int x)
{
  return x;
}

int
garply (int x)
{
  switch (x)
    {
    case 0:
      [[]] __attribute__(()) [[]] __attribute__((musttail)) [[]] return foo (42);
    case 1:
      [[]] __attribute__(()) [[gnu::musttail]] __attribute__(()) [[]] return bar (43);
    case 2:
      __attribute__(()) [[]] __attribute__((musttail)) [[]] __attribute__(()) return baz (44);
    case 3:
      __attribute__(()) [[gnu::musttail]] __attribute__(()) [[]] __attribute__(()) return qux (45);
    case 4:
      [[]] __attribute__(()) [[clang::musttail]] __attribute__(()) [[]] return corge (46);
    default:
      __attribute__(()) [[clang::musttail]] __attribute__(()) [[]] __attribute__(()) return freddy (47);
    }
}
