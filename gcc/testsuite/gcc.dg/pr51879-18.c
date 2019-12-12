/* { dg-do compile } */
/* { dg-options "-O2 -ftree-tail-merge -fdump-tree-pre -fno-tree-copy-prop -fno-tree-dominator-opts" } */

extern int foo (void);

void bar (int c, int *p)
{
  int *q = p;

  if (c)
    *p = foo ();
  else
    *q = foo ();
}

/* { dg-final { scan-tree-dump-times "foo \\(" 1 "pre"} } */
