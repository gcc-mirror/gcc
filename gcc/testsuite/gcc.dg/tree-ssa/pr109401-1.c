/* PR tree-optimization/109878 */
/* { dg-do compile } */
/* { dg-options "-O1 -fdump-tree-optimized" } */

/* Returns max (a, b) */
int max(int a, int b) {
  if (b > a)
    return b;
  else
    return a;
}

/* Returns min (a, b) */
int min(int a, int b) {
  if (b < a)
    return b;
  else
    return a;
}

/* These functions should return a op b */
int f8(int a, int b)
{
  return min (max (a, b), min (a, b));
}

int f9(int a, int b)
{
  return max (min (a, b), max (a, b));
}

/* Function min and f8 should have MIN_EXPR */
/* Function max and f9 should have MAX_EXPR */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 2 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 2 "optimized" } } */
