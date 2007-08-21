/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

char *foo(char *p, __SIZE_TYPE__ i)
{
  return (char *)i + (__SIZE_TYPE__)p;
}

/* { dg-final { scan-tree-dump "p +" "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
