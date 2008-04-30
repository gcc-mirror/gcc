/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

struct f { int i; };
int g()
{
  struct f a, *a1;
  int *i;
  a.i = 1;
  a1 = &a;
  i = &a1->i;
  return *i;  /* This should be turned into a.i */
}

/* { dg-final { scan-tree-dump "= a.i;" "ccp1" } } */
/* { dg-final { cleanup-tree-dump "ccp1" } } */
