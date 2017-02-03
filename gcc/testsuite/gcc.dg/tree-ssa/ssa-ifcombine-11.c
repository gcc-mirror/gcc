/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-optimized-details-blocks" } */

/* Testcase for PR31657.  */
int g(void);
int f(int x, int a, int b)
{
  int t = 0;
  int c = 1 << a;
  if (!(x & 1))
    t = 0;
  else
    if (x & (1 << 2))
      t = g();
    else
      t = 0;
  return t;
}

/* { dg-final { scan-tree-dump "& 5" "optimized" } } */
/* { dg-final { scan-tree-dump-not "Invalid sum" "optimized" } } */
