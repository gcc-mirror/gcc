/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-details-blocks" } */

/* Testcase for PR31657.  */

int foo (int x, int a, int b)
{
  int c = 1 << a;
  if (x & c)
    if (x & (1 << b))
      /* returning 1 causes phiopt to trigger in */
      return 2;
  return 0;
}

/* { dg-final { scan-tree-dump "\\|" "optimized" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
