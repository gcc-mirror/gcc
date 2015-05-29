/* { dg-do compile } */
/* { dg-options "-O3 -ffast-math -fdump-tree-optimized" } */

float baz (float x, float y)
{
  return x * x * x * x * y * y * y * y;
}

/* { dg-final { scan-tree-dump-times " \\* " 3 "optimized" } } */
