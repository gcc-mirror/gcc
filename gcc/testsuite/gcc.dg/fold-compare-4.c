/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int test1 (int a, int b)
{
  return a - b == a;
}
int test2 (int a, int b)
{
  return a + b == a;
}
int test3 (int a)
{
  return a + 5 == a;
}
int test4 (int a)
{
  return a - 5 == a;
}

/* { dg-final { scan-tree-dump-times "b == 0" 2 "original" } } */
/* { dg-final { scan-tree-dump-times "return 0" 2 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
