/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1 -fdump-tree-ccp1" } */

int foo (int x)
{
  int tem = x / 3;
  return tem / 5;
}
int bar (int x)
{
  int tem = x / 3;
  return tem / (__INT_MAX__ / 2);
}

/* { dg-final { scan-tree-dump "x_.\\(D\\) / 15" "forwprop1" } } */
/* { dg-final { scan-tree-dump "return 0;" "ccp1" } } */
