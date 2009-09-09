/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-forwprop1" } */

int f(int *p, int n)
{
  int (*a)[n] = (int (*)[n])p;
  int *q = &(*a)[0];
  return q[1];
}

int g(int *p, int n)
{
  int (*a)[n] = (int (*)[n])p;
  int *q = &(*a)[2];
  return q[-1];
}

/* { dg-final { scan-tree-dump-times "= \\\(\\\*a_..\\\)\\\[1\\\];" 2 "forwprop1" { xfail *-*-* } } } */
/* { dg-final { cleanup-tree-dump "forwprop1" } } */
