/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre" } */

int bar (int);
void baz (int);

void
foo (int y)
{
  int a;
  if (y)
    baz (bar (7) + 6);
  else
    baz (bar (7) + 6);
}

/* { dg-final { scan-tree-dump-times "bar \\(" 1 "pre"} } */
/* { dg-final { scan-tree-dump-times "baz \\(" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
