/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -fdump-tree-optimized" } */

double baz (double x, double y, double z, double u)
{
  return x * x * x * y * y * y * z * z * z * z * u * u * u * u;
}

/* { dg-final { scan-tree-dump-times " \\* " 6 "optimized" } } */
