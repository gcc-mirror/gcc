/* { dg-do compile } */
/* { dg-options "-fdump-tree-original" } */

typedef __INTPTR_TYPE__ ssize_t;
ssize_t foo (ssize_t x)
{
  return (ssize_t)(char *)x;
}

char *bar (char *x)
{
  return (char *)(ssize_t)x;
}

/* { dg-final { scan-tree-dump-times "return x;" 2 "original" } } */
/* { dg-final { cleanup-tree-dump "original" } } */
