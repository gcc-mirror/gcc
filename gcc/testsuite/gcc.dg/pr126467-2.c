/* PR tree-optimization/126467 */
/* { dg-do compile } */
/* { dg-options "-O2 -fno-signed-zeros -ffinite-math-only -fdump-tree-optimized" } */

double foo (double x)
{
  return 0.0 - x;
}

double bar (double y)
{
  return 0.0 - __builtin_fabs (y);
}

/* { dg-final { scan-tree-dump-not " \\- " "optimized" } } */
