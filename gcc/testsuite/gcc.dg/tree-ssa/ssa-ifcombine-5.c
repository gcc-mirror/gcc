/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

/* Testcase from PR15353.  */

int g(void);
int h(void);
int f(int *i, int *j)
{
  while (1)
    {
      if (*i > *j || *i == *j)
        break;
      return g();
    }
  return h();
}

/* { dg-final { scan-tree-dump ">=" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
