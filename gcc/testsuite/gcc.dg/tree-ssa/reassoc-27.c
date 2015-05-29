/* { dg-do compile } */
/* { dg-options "-O -ffast-math -frounding-math -fdump-tree-optimized" } */

double baz (double foo, double bar)
{
  return foo * foo * foo * foo * bar * bar * bar * bar;
}

/* We should re-associate this as (foo * bar)**3.  */
/* { dg-final { scan-tree-dump-times " \\\* " 3 "optimized" } } */
