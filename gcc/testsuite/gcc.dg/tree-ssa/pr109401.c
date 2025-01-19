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

/* All the functions here shouldn't evalute min or max of a and b
 * These functions should return a op b */
int f(int a, int b)
{
  return max (a, b) + min (a, b);
}

int f1(int a, int b)
{
  return max (a, b) * min (a, b);
}

int f2(int a, int b)
{
  return max (a, b) | min (a, b);
}

int f3(int a, int b)
{
  return max (a, b) & min (a, b);
}

int f5(int a, int b)
{
  return min (a, b) ^ max (a, b);
}

int f6(int a, int b)
{
  return min (a, b) == max (a, b);
}

int f7(int a, int b)
{
  return min (a, b) != max (a, b);
}

/* Function min should have MIN_EXPR */
/* Function max should have MAX_EXPR */
/* { dg-final { scan-tree-dump-times "MIN_EXPR" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "MAX_EXPR" 1 "optimized" } } */
