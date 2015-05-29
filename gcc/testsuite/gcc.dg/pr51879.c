/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fdump-tree-pre" } */

int bar (int);
void baz (int);

void
foo (int y)
{
  int a;
  if (y == 6)
    a = bar (7);
  else
    a = bar (7);
  baz (a);
}

/* { dg-final { scan-tree-dump-times "bar \\(" 1 "pre"} } */
