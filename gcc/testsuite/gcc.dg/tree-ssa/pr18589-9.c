/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -fdump-tree-optimized" } */

double baz (double x, double y, double z)
{
  return (__builtin_pow (x, 3.0) * __builtin_pow (y, 2.0)
	  * __builtin_pow (z, 5.0));
}

/* { dg-final { scan-tree-dump-times " \\* " 6 "optimized" } } */
/* { dg-final { cleanup-tree-dump "optimized" } } */
