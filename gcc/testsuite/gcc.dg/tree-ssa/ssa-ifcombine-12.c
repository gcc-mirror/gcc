/* { dg-do compile } */
/* { dg-options "-O2 -fno-tree-vrp -fdump-tree-optimized" } */

/* Testcase for PR31657.  */

int f(int x, int a, int b)
{
  int t = 0;
  int c = 1 << a;
  if (!(x & 1))
    t = 0;
  else
    if (x & (1 << 2))
      t = 3;
    else
      t = 0;
  return t;
}
/* { dg-final { scan-tree-dump "& 5" "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
