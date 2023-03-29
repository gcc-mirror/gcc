/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=zero -fdump-tree-ccp1" } */

int foo (int flag)
{
  int i;
  if (flag)
    i = 1;
  return i;
}

/* { dg-final { scan-tree-dump "return 1;" "ccp1" } } */
