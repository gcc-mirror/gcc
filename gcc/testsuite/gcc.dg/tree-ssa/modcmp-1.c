/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */

/* A remainder of non-negative operands equals its dividend exactly when the
   dividend is smaller than the divisor, so the division can go away.  */

int f1 (unsigned int x, unsigned int y) { return x % y == x; }
int f2 (unsigned int x, unsigned int y) { return x % y != x; }
int f3 (unsigned long x, unsigned long y) { return x == x % y; }
int f4 (int x, int y)
{
  x &= __INT_MAX__;
  y &= __INT_MAX__;
  return x % y == x;
}

/* Signed operands that may be negative do not have that property.  */
int f5 (int x, int y) { return x % y == x; }

/* { dg-final { scan-tree-dump-times " % " 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times " < " 3 "optimized" } } */
/* { dg-final { scan-tree-dump-times " >= " 1 "optimized" } } */
