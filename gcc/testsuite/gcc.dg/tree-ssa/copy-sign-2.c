/* { dg-options "-O2 -ffast-math -fdump-tree-optimized" } */
/* { dg-do compile } */
float f(float x)
{
  float t = __builtin_copysignf (1.0f, x);
  return x * t;
}
float f1(float x)
{
  float t = __builtin_copysignf (1.0f, -x);
  return x * t;
}
/* { dg-final { scan-tree-dump-times "ABS" 1 "optimized"} } */
/* { dg-final { scan-tree-dump-times ".COPYSIGN" 1 "optimized"} } */
