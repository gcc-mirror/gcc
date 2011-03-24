/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

static const int a[4] = {};
int foo(void)
{
  int i = 1;
  const int *p = &a[i];
  return *p;
}

/* { dg-final { scan-tree-dump "return 0;" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
