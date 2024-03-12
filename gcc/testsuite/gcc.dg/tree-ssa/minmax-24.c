/* { dg-do compile } */
/* { dg-options "-O2 -fdump-tree-optimized" } */
/* PR tree-optimization/96694 */

static inline int min(int a, int b)
{
  return a < b ? a : b;
}

static inline int max(int a, int b)
{
  return a > b ? a : b;
}

int max_not(int x, int y)
{
  return ~max(~x, y); // min (x, ~y)
}
/* { dg-final { scan-tree-dump "~y_\[0-9\]+.D.;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "~x_\[0-9\]+.D.;" "optimized" } } */
/* { dg-final { scan-tree-dump "MIN_EXPR <x_\[0-9\]+.D., _\[0-9\]+>|MIN_EXPR <_\[0-9\]+, x_\[0-9\]+.D.>" "optimized" } } */

int min_not(int c, int d)
{
  return ~min(~c, d); // max (c, ~d)
}
/* { dg-final { scan-tree-dump "~d_\[0-9\]+.D.;" "optimized" } } */
/* { dg-final { scan-tree-dump-not "~c_\[0-9\]+.D.;" "optimized" } } */
/* { dg-final { scan-tree-dump "MAX_EXPR <c_\[0-9\]+.D., _\[0-9\]+>|MIN_EXPR <_\[0-9\]+, c_\[0-9\]+.D.>" "optimized" } } */

/* { dg-final { scan-tree-dump-times "~" 2 "optimized" } } */
