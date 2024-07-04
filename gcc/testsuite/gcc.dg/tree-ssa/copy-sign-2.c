/* { dg-options "-O2 -ffast-math -fdump-tree-optimized" } */
/* { dg-additional-options "-msse -mfpmath=sse" { target { { i?86-*-* x86_64-*-* } && ilp32 } } } */
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
/* { dg-final { scan-tree-dump-times "ABS" 1 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times ".COPYSIGN" 1 "optimized" { target ifn_copysign } } } */
/* { dg-final { scan-tree-dump-times "ABS" 2 "optimized" { target { ! ifn_copysign } } } } */
