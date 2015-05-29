/* { dg-do compile } */
/* { dg-require-effective-target int32plus } */
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

/* { dg-final { scan-tree-dump-times "= MEM\\\[\\\(int \\\*\\\)\[ap\]_\[0-9\]+(?:\\(D\\))? \\\+ 4B\\\];" 2 "forwprop1" } } */
