/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fno-tree-dominator-opts" } */

int bar (int i);

void
foo (int c, int d)
{
  if (bar (c))
    bar (c);
  d = 33;
  while (c == d);
}
