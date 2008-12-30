/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

int foo()
{
  volatile int a[1];
  int i, *p = (int*)a;

  a[0] = 1;
  for (i = 0; i < 1; ++i)
    if (p[i])
      return -1;
  return 0;
}

/* { dg-final { scan-tree-dump "a.0. ={v} 1;" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
