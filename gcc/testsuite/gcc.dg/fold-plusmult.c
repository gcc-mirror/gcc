/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

int test1 (int a)
{
  return 2*a + 2*a;
}

int test2 (int a)
{
  return (a + a)*2;
}

/* { dg-final { scan-tree-dump-times "a \\\* 4" 2 "original" } } */
