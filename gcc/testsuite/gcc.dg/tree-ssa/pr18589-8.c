/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -fdump-tree-optimized" } */

long double baz (long double x, long double y)
{
  return x * x * x * x * y * y * y * y;
}

/* { dg-final { scan-tree-dump-times " \\* " 3 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
