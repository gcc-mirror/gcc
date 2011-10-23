/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-pre" } */

int bar (int i);

void
foo (int c, int d)
{
  if (bar (c))
    bar (c);
  d = 33;
  while (c == d);
}

/* { dg-final { scan-tree-dump-times "== 33" 1 "pre"} } */
/* { dg-final { cleanup-tree-dump "pre" } } */
