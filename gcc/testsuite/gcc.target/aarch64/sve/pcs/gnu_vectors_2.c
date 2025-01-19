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

void mfloat8_callee (svmfloat8_t);
void bfloat16_callee (svbfloat16_t);
void float16_callee (svfloat16_t);
void float32_callee (svfloat32_t);
void float64_callee (svfloat64_t);
void int8_callee (svint8_t);
void int16_callee (svint16_t);
void int32_callee (svint32_t);
void int64_callee (svint64_t);
void uint8_callee (svuint8_t);
void uint16_callee (svuint16_t);
void uint32_callee (svuint32_t);
void uint64_callee (svuint64_t);

void
mfloat8_caller (mfloat8x32_t arg)
{
  mfloat8_callee (arg);
}

void
bfloat16_caller (bfloat16x16_t arg)
{
  bfloat16_callee (arg);
}

void
float16_caller (float16x16_t arg)
{
  float16_callee (arg);
}

void
float32_caller (float32x8_t arg)
{
  float32_callee (arg);
}

void
float64_caller (float64x4_t arg)
{
  float64_callee (arg);
}

void
int8_caller (int8x32_t arg)
{
  int8_callee (arg);
}

void
int16_caller (int16x16_t arg)
{
  int16_callee (arg);
}

void
int32_caller (int32x8_t arg)
{
  int32_callee (arg);
}

void
int64_caller (int64x4_t arg)
{
  int64_callee (arg);
}

void
uint8_caller (uint8x32_t arg)
{
  uint8_callee (arg);
}

void
uint16_caller (uint16x16_t arg)
{
  uint16_callee (arg);
}

void
uint32_caller (uint32x8_t arg)
{
  uint32_callee (arg);
}

void
uint64_caller (uint64x4_t arg)
{
  uint64_callee (arg);
}

/* { dg-final { scan-assembler-times {\tld1b\tz0\.b, p[0-7]/z, \[x0\]} 3 } } */
/* { dg-final { scan-assembler-times {\tld1h\tz0\.h, p[0-7]/z, \[x0\]} 4 } } */
/* { dg-final { scan-assembler-times {\tld1w\tz0\.s, p[0-7]/z, \[x0\]} 3 } } */
/* { dg-final { scan-assembler-times {\tld1d\tz0\.d, p[0-7]/z, \[x0\]} 3 } } */
/* { dg-final { scan-assembler-not {\tst1[bhwd]\t} } } */
