/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

int foo (int i)
{
  int j = i;
  int k = 0;
  int l = j + k;
  int m = l - j;
  return m;
}

/* { dg-final { scan-tree-dump "return 0;" "ccp1" } } */
