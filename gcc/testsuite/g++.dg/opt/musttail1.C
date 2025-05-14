// PR ipa/119376
// { dg-do compile { target musttail } }
// { dg-options "-O2 -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 2 "optimized" } }
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(\[^\n\r]*\\\); \\\[tail call\\\]" 3 "optimized" } }

int foo (int x);
typedef int (*F) (int);
int v;

inline int
bar (int x)
{
  if (__builtin_expect (x == 42, 1))
    return 1;
  [[gnu::musttail]] return foo (x + v * (x | v) * (x & v) * (x - v) * (x + v * v));
}

int
baz (int x)
{
  [[gnu::musttail]] return bar (x);
}

int
qux (int x)
{
  return bar (x + 1);
}

F
corge ()
{
  return &bar;
}
