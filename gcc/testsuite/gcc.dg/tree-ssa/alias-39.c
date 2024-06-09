/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop3" } */

static int a, b;
int foo (int n, int which)
{
  void *p = __builtin_malloc (n);
  void *q = which ? &a : &b;
  return p == q;
}

/* { dg-final { scan-tree-dump "return 0;" "forwprop3" } } */
