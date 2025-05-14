// PR ipa/119376
// { dg-do compile { target musttail } }
// { dg-additional-options "-ffat-lto-objects -fdump-tree-optimized" }
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = foo \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */

struct S { int s; };
int foo (int);

int
bar (int a)
{
  S b = {a};
  b.s++;
  [[gnu::musttail]] return foo (a);
}
