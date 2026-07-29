/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* The sum of the minimum and the maximum is the sum of the operands.  */

int f1 (int a, int b) { int mn = a < b ? a : b; return (a + b) - mn; }
int f2 (int a, int b) { int mx = a < b ? b : a; return (a + b) - mx; }
unsigned int f3 (unsigned int a, unsigned int b)
{
  unsigned int mn = a < b ? a : b;
  return (a + b) - mn;
}
long f4 (long a, long b) { long mx = a < b ? b : a; return (b + a) - mx; }

/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-not " \\+ " "optimized" } } */
/* { dg-final { scan-tree-dump-not " - " "optimized" } } */
