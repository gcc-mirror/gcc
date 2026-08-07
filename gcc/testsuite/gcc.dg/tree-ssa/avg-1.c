/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* ((x >> 1) + (y >> 1)) + (x & y & 1) is the average of x and y without
   overflow.  It must fold to (x & y) + ((x ^ y) >> 1), which needs three
   operations instead of five.  */

int
f (int a, int b)
{
  return ((a >> 1) + (b >> 1)) + (a & b & 1);
}

unsigned
g (unsigned a, unsigned b)
{
  return (a & b & 1) + ((a >> 1) + (b >> 1));
}

long
h (long a, long b)
{
  return ((a >> 1) + (b >> 1)) + (1 & b & a);
}

/* { dg-final { scan-tree-dump-times " \\^ " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-not " & 1;" "optimized" } } */
