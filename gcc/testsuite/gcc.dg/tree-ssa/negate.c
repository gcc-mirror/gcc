/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-reassoc1" } */

int f (int a, int b)
{
  int x = -a;
  int y = b - x;
  return y;
}

int g (int a, int b)
{
  int x = -a;
  int y = x - b;
  return y;
}

/* There should be two additions now.  */
/* { dg-final { scan-tree-dump-times "\\+" 2 "reassoc1"} } */
/* { dg-final { cleanup-tree-dump "reassoc1" } } */
