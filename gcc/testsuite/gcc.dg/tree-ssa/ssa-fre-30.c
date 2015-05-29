/* { dg-do compile } */
/* { dg-options "-O -ffinite-math-only -fdump-tree-optimized" } */

float foo(float x, float y, float z)
{
  float a = __builtin_fmaf (x, y, z);
  float b = __builtin_fmaf (x, y, z);
  return a - b;
}

/* { dg-final { scan-tree-dump "return 0" "optimized" } } */
