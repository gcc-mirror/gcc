/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized" } */

/* Testcase for PR31657.  */

int foo (int x, int a, int b)
{
  /* if ((x & a) || (x & b)) */
  if (x & a)
    goto doit;
  if (x & b)
    goto doit;

  /* else */
  return 0;

  /* then - returing 1 causes phiopt to trigger */
doit:
  return 2;
}

/* { dg-final { scan-tree-dump "\\|" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
