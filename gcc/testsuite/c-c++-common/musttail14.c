/* PR tree-optimization/118430 */
/* { dg-do compile { target musttail } } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = bar \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "  \[^\n\r]* = freddy \\\(\[^\n\r]*\\\); \\\[tail call\\\] \\\[must tail call\\\]" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "  (?:bar|freddy) \\\(\[^\n\r]*\\\); \\\[tail call\\\]" "optimized" } } */

__attribute__ ((noipa)) void
foo (int x)
{
  (void) x;
}

__attribute__ ((noinline)) int
bar (int x)
{
  foo (x);
  return 1;
}

__attribute__ ((noinline)) int
baz (int *x)
{
  foo (*x);
  return 2;
}

__attribute__((noipa)) int
qux (int x)
{
  {
    int v;
    foo (x);
    baz (&v);
  }
  [[gnu::musttail]]
  return bar (x);
}

__attribute__((noipa)) int
corge (int x)
{
  {
    int v;
    foo (x);
    baz (&v);
  }
  return bar (x) + 1;
}

__attribute__ ((noinline)) float
freddy (int x)
{
  foo (x);
  return 1.75f;
}

__attribute__((noipa)) float
garply (int x)
{
  {
    int v;
    foo (x);
    baz (&v);
  }
  [[gnu::musttail]]
  return freddy (x);
}

__attribute__((noipa)) float
quux (int x)
{
  {
    int v;
    foo (x);
    baz (&v);
  }
  return freddy (x) + 0.25f;
}

int v;

int
main ()
{
  qux (v);
  corge (v);
  garply (v);
  quux (v);
}
