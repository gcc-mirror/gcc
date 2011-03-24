/* { dg-do compile } */
/* { dg-options "-O -fno-tree-ccp -fno-tree-forwprop -fdump-tree-fre" } */

static const int a[4] = {};
int foo(void)
{
  int i = 1;
  const int *p = &a[i];
  return *p;
}

/* { dg-final { scan-tree-dump "= 0;" "fre" } } */
/* { dg-final { cleanup-tree-dump "fre" } } */
