/* { dg-options "-O" } */

#pragma GCC target "+simd+fp16"

__Float16x4_t
f1 (__Float16x4_t x, __Float16x4_t y)
{
  return x * y[0];
}

__Float16x4_t
f2 (__Float16x4_t x, __Float16x4_t y)
{
  return x * y[3];
}

__Float16x4_t
f3 (__Float16x4_t x, __Float16x8_t y)
{
  return x * y[0];
}

__Float16x4_t
f4 (__Float16x4_t x, __Float16x8_t y)
{
  return x * y[7];
}

__Float16x8_t
f5 (__Float16x8_t x, __Float16x4_t y)
{
  return x * y[0];
}

__Float16x8_t
f6 (__Float16x8_t x, __Float16x4_t y)
{
  return x * y[3];
}

__Float16x8_t
f7 (__Float16x8_t x, __Float16x8_t y)
{
  return x * y[0];
}

__Float16x8_t
f8 (__Float16x8_t x, __Float16x8_t y)
{
  return x * y[7];
}

/* { dg-final { scan-assembler-times {\tfmul\tv0.4h, v0.4h, v1.h\[0\]} 2 } } */
/* { dg-final { scan-assembler-times {\tfmul\tv0.4h, v0.4h, v1.h\[3\]} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tv0.4h, v0.4h, v1.h\[7\]} 1 } } */

/* { dg-final { scan-assembler-times {\tfmul\tv0.8h, v0.8h, v1.h\[0\]} 2 } } */
/* { dg-final { scan-assembler-times {\tfmul\tv0.8h, v0.8h, v1.h\[3\]} 1 } } */
/* { dg-final { scan-assembler-times {\tfmul\tv0.8h, v0.8h, v1.h\[7\]} 1 } } */
