/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-ccp1" } */

int foo (int i, int j)
{
  int x = 1;
  int y = i + x;
  int z = y - i;
  if (z == 1)
    return 1;
  return 2;
}

/* { dg-final { scan-tree-dump "return 1;" "ccp1" } } */
