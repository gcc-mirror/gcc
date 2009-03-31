/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int test1 (int a)
{
  return 2 - a == a;
}
int test2 (int a)
{
  return 1 - a == a;
}
int test3 (int a)
{
  return 1 - a != a;
}

/* { dg-final { scan-tree-dump-times "return 2 - a == a" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return 0" 1 "original" } } */
/* { dg-final { scan-tree-dump-times "return 1" 1 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
