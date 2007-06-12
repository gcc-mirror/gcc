/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

/* Testcase extracted from PR15353.  */

int foo (int x, int a)
{
  /* if ((x > a) || (x == a)) */
  if (x > a)
    goto doit;
  if (x == a)
    goto doit;

  /* else */
  return 0;

  /* then - returing 1 causes phiopt to trigger */
doit:
  return 2;
}

/* { dg-final { scan-tree-dump ">=" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
