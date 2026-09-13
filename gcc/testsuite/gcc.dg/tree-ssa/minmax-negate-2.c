/* { dg-do compile } */
/* { dg-options "-O2 -fwrapv -fdump-tree-optimized-raw" } */

/* With a wrapping type the negation is no longer order reversing: at
   x = INT_MIN, -x is INT_MIN again and MAX (-x, -5) picks the wrong arm.
   Neither subtraction may go.  */

int
sub_min_cst (int a, int x)
{
  int nx = -x;
  int m = nx < 5 ? nx : 5;
  return a - m;
}

int
sub_max_cst (int a, int x)
{
  int nx = -x;
  int m = nx > 5 ? nx : 5;
  return a - m;
}

/* { dg-final { scan-tree-dump-times "<minus_expr," 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "<min_expr," 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "<max_expr," 1 "optimized" } } */
