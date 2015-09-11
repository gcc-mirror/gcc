/* Test vdup_lane intrinsics work correctly.  */
/* { dg-do run } */
/* { dg-options "--save-temps -O1" } */

#include <arm_neon.h>

extern void abort (void);

float32x2_t __attribute__ ((noinline))
wrap_vdup_lane_f32_0 (float32x2_t a)
{
  return vdup_lane_f32 (a, 0);
}

float32x2_t __attribute__ ((noinline))
wrap_vdup_lane_f32_1 (float32x2_t a)
{
  return vdup_lane_f32 (a, 1);
}

int __attribute__ ((noinline))
test_vdup_lane_f32 ()
{
  float32x2_t a;
  float32x2_t b;
  int i;
  float32_t c[2] = { 0.0 , 3.14 };
  float32_t d[2];

  a = vld1_f32 (c);
  b = wrap_vdup_lane_f32_0 (a);
  vst1_f32 (d, b);
  for (i = 0; i < 2; i++)
    if (c[0] != d[i])
      return 1;

  b = wrap_vdup_lane_f32_1 (a);
  vst1_f32 (d, b);
  for (i = 0; i < 2; i++)
    if (c[1] != d[i])
      return 1;
  return 0;
}

float32x4_t __attribute__ ((noinline))
wrap_vdupq_lane_f32_0 (float32x2_t a)
{
  return vdupq_lane_f32 (a, 0);
}

float32x4_t __attribute__ ((noinline))
wrap_vdupq_lane_f32_1 (float32x2_t a)
{
  return vdupq_lane_f32 (a, 1);
}

int __attribute__ ((noinline))
test_vdupq_lane_f32 ()
{
  float32x2_t a;
  float32x4_t b;
  int i;
  float32_t c[2] = { 0.0 , 3.14 };
  float32_t d[4];

  a = vld1_f32 (c);
  b = wrap_vdupq_lane_f32_0 (a);
  vst1q_f32 (d, b);
  for (i = 0; i < 4; i++)
    if (c[0] != d[i])
      return 1;

  b = wrap_vdupq_lane_f32_1 (a);
  vst1q_f32 (d, b);
  for (i = 0; i < 4; i++)
    if (c[1] != d[i])
      return 1;
  return 0;
}

int8x8_t __attribute__ ((noinline))
wrap_vdup_lane_s8_0 (int8x8_t a)
{
  return vdup_lane_s8 (a, 0);
}

int8x8_t __attribute__ ((noinline))
wrap_vdup_lane_s8_1 (int8x8_t a)
{
  return vdup_lane_s8 (a, 1);
}

int __attribute__ ((noinline))
test_vdup_lane_s8 ()
{
  int8x8_t a;
  int8x8_t b;
  int i;
  /* Only two first cases are interesting.  */
  int8_t c[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  int8_t d[8];

  a = vld1_s8 (c);
  b = wrap_vdup_lane_s8_0 (a);
  vst1_s8 (d, b);
  for (i = 0; i < 8; i++)
    if (c[0] != d[i])
      return 1;

  b = wrap_vdup_lane_s8_1 (a);
  vst1_s8 (d, b);
  for (i = 0; i < 8; i++)
    if (c[1] != d[i])
      return 1;
  return 0;
}

int8x16_t __attribute__ ((noinline))
wrap_vdupq_lane_s8_0 (int8x8_t a)
{
  return vdupq_lane_s8 (a, 0);
}

int8x16_t __attribute__ ((noinline))
wrap_vdupq_lane_s8_1 (int8x8_t a)
{
  return vdupq_lane_s8 (a, 1);
}

int __attribute__ ((noinline))
test_vdupq_lane_s8 ()
{
  int8x8_t a;
  int8x16_t b;
  int i;
  /* Only two first cases are interesting.  */
  int8_t c[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };
  int8_t d[16];

  a = vld1_s8 (c);
  b = wrap_vdupq_lane_s8_0 (a);
  vst1q_s8 (d, b);
  for (i = 0; i < 16; i++)
    if (c[0] != d[i])
      return 1;

  b = wrap_vdupq_lane_s8_1 (a);
  vst1q_s8 (d, b);
  for (i = 0; i < 16; i++)
    if (c[1] != d[i])
      return 1;
  return 0;
}

int16x4_t __attribute__ ((noinline))
wrap_vdup_lane_s16_0 (int16x4_t a)
{
  return vdup_lane_s16 (a, 0);
}

int16x4_t __attribute__ ((noinline))
wrap_vdup_lane_s16_1 (int16x4_t a)
{
  return vdup_lane_s16 (a, 1);
}

int __attribute__ ((noinline))
test_vdup_lane_s16 ()
{
  int16x4_t a;
  int16x4_t b;
  int i;
  /* Only two first cases are interesting.  */
  int16_t c[4] = { 0, 1, 2, 3 };
  int16_t d[4];

  a = vld1_s16 (c);
  b = wrap_vdup_lane_s16_0 (a);
  vst1_s16 (d, b);
  for (i = 0; i < 4; i++)
    if (c[0] != d[i])
      return 1;

  b = wrap_vdup_lane_s16_1 (a);
  vst1_s16 (d, b);
  for (i = 0; i < 4; i++)
    if (c[1] != d[i])
      return 1;
  return 0;
}

int16x8_t __attribute__ ((noinline))
wrap_vdupq_lane_s16_0 (int16x4_t a)
{
  return vdupq_lane_s16 (a, 0);
}

int16x8_t __attribute__ ((noinline))
wrap_vdupq_lane_s16_1 (int16x4_t a)
{
  return vdupq_lane_s16 (a, 1);
}

int __attribute__ ((noinline))
test_vdupq_lane_s16 ()
{
  int16x4_t a;
  int16x8_t b;
  int i;
  /* Only two first cases are interesting.  */
  int16_t c[4] = { 0, 1, 2, 3 };
  int16_t d[8];

  a = vld1_s16 (c);
  b = wrap_vdupq_lane_s16_0 (a);
  vst1q_s16 (d, b);
  for (i = 0; i < 8; i++)
    if (c[0] != d[i])
      return 1;

  b = wrap_vdupq_lane_s16_1 (a);
  vst1q_s16 (d, b);
  for (i = 0; i < 8; i++)
    if (c[1] != d[i])
      return 1;
  return 0;
}

int32x2_t __attribute__ ((noinline))
wrap_vdup_lane_s32_0 (int32x2_t a)
{
  return vdup_lane_s32 (a, 0);
}

int32x2_t __attribute__ ((noinline))
wrap_vdup_lane_s32_1 (int32x2_t a)
{
  return vdup_lane_s32 (a, 1);
}

int __attribute__ ((noinline))
test_vdup_lane_s32 ()
{
  int32x2_t a;
  int32x2_t b;
  int i;
  int32_t c[2] = { 0, 1 };
  int32_t d[2];

  a = vld1_s32 (c);
  b = wrap_vdup_lane_s32_0 (a);
  vst1_s32 (d, b);
  for (i = 0; i < 2; i++)
    if (c[0] != d[i])
      return 1;

  b = wrap_vdup_lane_s32_1 (a);
  vst1_s32 (d, b);
  for (i = 0; i < 2; i++)
    if (c[1] != d[i])
      return 1;
  return 0;
}

int32x4_t __attribute__ ((noinline))
wrap_vdupq_lane_s32_0 (int32x2_t a)
{
  return vdupq_lane_s32 (a, 0);
}

int32x4_t __attribute__ ((noinline))
wrap_vdupq_lane_s32_1 (int32x2_t a)
{
  return vdupq_lane_s32 (a, 1);
}

int __attribute__ ((noinline))
test_vdupq_lane_s32 ()
{
  int32x2_t a;
  int32x4_t b;
  int i;
  int32_t c[2] = { 0, 1 };
  int32_t d[4];

  a = vld1_s32 (c);
  b = wrap_vdupq_lane_s32_0 (a);
  vst1q_s32 (d, b);
  for (i = 0; i < 4; i++)
    if (c[0] != d[i])
      return 1;

  b = wrap_vdupq_lane_s32_1 (a);
  vst1q_s32 (d, b);
  for (i = 0; i < 4; i++)
    if (c[1] != d[i])
      return 1;
  return 0;
}

int64x1_t __attribute__ ((noinline))
wrap_vdup_lane_s64_0 (int64x1_t a)
{
  return vdup_lane_s64 (a, 0);
}

int __attribute__ ((noinline))
test_vdup_lane_s64 ()
{
  int64x1_t a;
  int64x1_t b;
  int64_t c[1];
  int64_t d[1];

  c[0] = 0;
  a = vld1_s64 (c);
  b = wrap_vdup_lane_s64_0 (a);
  vst1_s64 (d, b);
  if (c[0] != d[0])
    return 1;

  return 0;
}

int64x2_t __attribute__ ((noinline))
wrap_vdupq_lane_s64_0 (int64x1_t a)
{
  return vdupq_lane_s64 (a, 0);
}

int __attribute__ ((noinline))
test_vdupq_lane_s64 ()
{
  int64x1_t a;
  int64x2_t b;
  int i;
  int64_t c[1];
  int64_t d[2];

  c[0] = 0;
  a = vld1_s64 (c);
  b = wrap_vdupq_lane_s64_0 (a);
  vst1q_s64 (d, b);
  for (i = 0; i < 2; i++)
    if (c[0] != d[i])
      return 1;
  return 0;
}

int
main ()
{

  if (test_vdup_lane_f32 ())
    abort ();
  if (test_vdup_lane_s8 ())
    abort ();
  if (test_vdup_lane_s16 ())
    abort ();
  if (test_vdup_lane_s32 ())
    abort ();
  if (test_vdup_lane_s64 ())
    abort ();
  if (test_vdupq_lane_f32 ())
    abort ();
  if (test_vdupq_lane_s8 ())
    abort ();
  if (test_vdupq_lane_s16 ())
    abort ();
  if (test_vdupq_lane_s32 ())
    abort ();
  if (test_vdupq_lane_s64 ())
    abort ();

  return 0;
}

/* Asm check for test_vdup_lane_s8.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8b, v\[0-9\]+\.b\\\[0\\\]" 1 } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8b, v\[0-9\]+\.b\\\[1\\\]" 1 } } */

/* Asm check for test_vdupq_lane_s8.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.16b, v\[0-9\]+\.b\\\[0\\\]" 1 } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.16b, v\[0-9\]+\.b\\\[1\\\]" 1 } } */

/* Asm check for test_vdup_lane_s16.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4h, v\[0-9\]+\.h\\\[0\\\]" 1 } } */
/* Asm check for test_vdup_lane_s16.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4h, v\[0-9\]+\.h\\\[1\\\]" 1 } } */

/* Asm check for test_vdupq_lane_s16.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8h, v\[0-9\]+\.h\\\[0\\\]" 1 } } */
/* Asm check for test_vdupq_lane_s16.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8h, v\[0-9\]+\.h\\\[1\\\]" 1 } } */

/* Asm check for test_vdup_lane_f32 and test_vdup_lane_s32.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.2s, v\[0-9\]+\.s\\\[0\\\]" 2 } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.2s, v\[0-9\]+\.s\\\[1\\\]" 2 } } */

/* Asm check for test_vdupq_lane_f32 and test_vdupq_lane_s32.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4s, v\[0-9\]+\.s\\\[0\\\]" 2 } } */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4s, v\[0-9\]+\.s\\\[1\\\]" 2 } } */

