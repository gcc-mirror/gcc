/* { dg-options "-O -msve-vector-bits=256 -fomit-frame-pointer" } */

#include <arm_sve.h>

typedef mfloat8_t mfloat8x32_t __attribute__((vector_size (32)));
typedef bfloat16_t bfloat16x16_t __attribute__((vector_size (32)));
typedef float16_t float16x16_t __attribute__((vector_size (32)));
typedef float32_t float32x8_t __attribute__((vector_size (32)));
typedef float64_t float64x4_t __attribute__((vector_size (32)));
typedef int8_t int8x32_t __attribute__((vector_size (32)));
typedef int16_t int16x16_t __attribute__((vector_size (32)));
typedef int32_t int32x8_t __attribute__((vector_size (32)));
typedef int64_t int64x4_t __attribute__((vector_size (32)));
typedef uint8_t uint8x32_t __attribute__((vector_size (32)));
typedef uint16_t uint16x16_t __attribute__((vector_size (32)));
typedef uint32_t uint32x8_t __attribute__((vector_size (32)));
typedef uint64_t uint64x4_t __attribute__((vector_size (32)));

void mfloat8_callee (mfloat8x32_t);
void bfloat16_callee (bfloat16x16_t);
void float16_callee (float16x16_t);
void float32_callee (float32x8_t);
void float64_callee (float64x4_t);
void int8_callee (int8x32_t);
void int16_callee (int16x16_t);
void int32_callee (int32x8_t);
void int64_callee (int64x4_t);
void uint8_callee (uint8x32_t);
void uint16_callee (uint16x16_t);
void uint32_callee (uint32x8_t);
void uint64_callee (uint64x4_t);

void
mfloat8_caller (mfloat8_t val)
{
  mfloat8_callee (svdup_mf8 (val));
}

void
bfloat16_caller (bfloat16_t val)
{
  bfloat16_callee (svdup_bf16 (val));
}

void
float16_caller (void)
{
  float16_callee (svdup_f16 (1.0));
}

void
float32_caller (void)
{
  float32_callee (svdup_f32 (2.0));
}

void
float64_caller (void)
{
  float64_callee (svdup_f64 (3.0));
}

void
int8_caller (void)
{
  int8_callee (svindex_s8 (0, 1));
}

void
int16_caller (void)
{
  int16_callee (svindex_s16 (0, 2));
}

void
int32_caller (void)
{
  int32_callee (svindex_s32 (0, 3));
}

void
int64_caller (void)
{
  int64_callee (svindex_s64 (0, 4));
}

void
uint8_caller (void)
{
  uint8_callee (svindex_u8 (1, 1));
}

void
uint16_caller (void)
{
  uint16_callee (svindex_u16 (1, 2));
}

void
uint32_caller (void)
{
  uint32_callee (svindex_u32 (1, 3));
}

void
uint64_caller (void)
{
  uint64_callee (svindex_u64 (1, 4));
}

/* { dg-final { scan-assembler-times {\tst1b\tz[0-9]+\.b, p[0-7], \[x0\]} 3 } } */
/* { dg-final { scan-assembler-times {\tst1h\tz[0-9]+\.h, p[0-7], \[x0\]} 4 } } */
/* { dg-final { scan-assembler-times {\tst1w\tz[0-9]+\.s, p[0-7], \[x0\]} 3 } } */
/* { dg-final { scan-assembler-times {\tst1d\tz[0-9]+\.d, p[0-7], \[x0\]} 3 } } */
/* { dg-final { scan-assembler-times {\tadd\tx0, sp, #?16\n} 13 } } */
