/* { dg-do compile } */
/* { dg-options "-O -fno-tree-ccp -fno-tree-forwprop -fdump-tree-fre1" } */

static const int a[4] = {};
int foo(void)
{
  int i = 1;
  const int *p = &a[i];
  return *p;
}

/* { dg-final { scan-tree-dump "return 0;" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
