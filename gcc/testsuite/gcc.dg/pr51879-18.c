/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-pre -fno-tree-copy-prop -fno-tree-dominator-opts -fno-tree-copyrename" } */

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
/* { dg-final { cleanup-tree-dump "pre" } } */
