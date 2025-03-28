// PR ipa/119376
// { dg-do compile { target musttail } }
// { dg-options "-O2 -fno-early-inlining -fdump-tree-optimized" }
// { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } }

struct S { S () {} };

[[gnu::noipa]] char *
foo (S)
{
  return 0;
}

char *
bar (S)
{
  S t;
  [[clang::musttail]] return foo (t);
}
