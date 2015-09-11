/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fdump-tree-pre" } */

int bar (int);
void baz (int);

int foo (int y)
{
  int a, b;
  a = bar (7) + 6;
  b = bar (7) + 6;
  return a + b;
}

/* { dg-final { scan-tree-dump-times "bar \\(" 2 "pre"} } */
