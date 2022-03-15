/* { dg-do compile } */
/* { dg-options "-O2 -ffinite-math-only -frounding-math -fdump-tree-optimized" } */
double foo (double a)
{
  return a - a;
}

/* { dg-final { scan-tree-dump " = a_\[0-9\]\\(D\\) - a_\[0-9\]\\(D\\);" "optimized" } } */
