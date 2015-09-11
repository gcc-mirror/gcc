/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -fdump-tree-optimized" } */

double baz (double x, double y)
{
  return x * y * y * x * y * x * x * y;
}

/* { dg-final { scan-tree-dump-times " \\* " 3 "optimized" } } */
