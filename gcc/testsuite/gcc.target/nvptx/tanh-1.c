/* { dg-do compile } */
/* { dg-options "-O2 -ffast-math -misa=sm_75 -mptx=7.0" } */

float foo(float x)
{
  return __builtin_tanhf(x);
}

/* { dg-final { scan-assembler "tanh.approx.f32" } } */
