/* Test vdup_lane intrinsics work correctly.  */
/* { dg-do run } */
/* { dg-options "-O1 --save-temps" } */

#include <arm_neon.h>

#define force_simd(V1) asm volatile ("mov %d0, %1.d[0]" \
         : "=w"(V1)                                     \
         : "w"(V1)                                      \
         : /* No clobbers */)

extern void abort (void);

float32_t __attribute__ ((noinline))
wrap_vdups_lane_f32_0 (float32x2_t dummy, float32x2_t a)
{
  return vdups_lane_f32 (a, 0);
}

float32_t __attribute__ ((noinline))
wrap_vdups_lane_f32_1 (float32x2_t a)
{
  return vdups_lane_f32 (a, 1);
}

int __attribute__ ((noinline))
test_vdups_lane_f32 ()
{
  float32x2_t a;
  float32_t b;
  float32_t c[2] = { 0.0, 1.0 };

  a = vld1_f32 (c);
  b = wrap_vdups_lane_f32_0 (a, a);
  if (c[0] != b)
    return 1;
  b = wrap_vdups_lane_f32_1 (a);
  if (c[1] != b)
    return 1;
  return 0;
}

float64_t __attribute__ ((noinline))
wrap_vdupd_lane_f64_0 (float64x1_t dummy, float64x1_t a)
{
  return vdupd_lane_f64 (a, 0);
}

int __attribute__ ((noinline))
test_vdupd_lane_f64 ()
{
  float64x1_t a;
  float64_t b;
  float64_t c[1] = { 0.0 };
  a = vld1_f64 (c);
  b = wrap_vdupd_lane_f64_0 (a, a);
  if (c[0] != b)
    return 1;
  return 0;
}

int8_t __attribute__ ((noinline))
wrap_vdupb_lane_s8_0 (int8x8_t dummy, int8x8_t a)
{
  int8_t result = vdupb_lane_s8 (a, 0);
  force_simd (result);
  return result;
}

int8_t __attribute__ ((noinline))
wrap_vdupb_lane_s8_1 (int8x8_t a)
{
  int8_t result = vdupb_lane_s8 (a, 1);
  force_simd (result);
  return result;
}

int __attribute__ ((noinline))
test_vdupb_lane_s8 ()
{
  int8x8_t a;
  int8_t b;
  int8_t c[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };

  a = vld1_s8 (c);
  b = wrap_vdupb_lane_s8_0 (a, a);
  if (c[0] != b)
    return 1;
  b = wrap_vdupb_lane_s8_1 (a);
  if (c[1] != b)
    return 1;

  return 0;
}

uint8_t __attribute__ ((noinline))
wrap_vdupb_lane_u8_0 (uint8x8_t dummy, uint8x8_t a)
{
  uint8_t result = vdupb_lane_u8 (a, 0);
  force_simd (result);
  return result;
}

uint8_t __attribute__ ((noinline))
wrap_vdupb_lane_u8_1 (uint8x8_t a)
{
  uint8_t result = vdupb_lane_u8 (a, 1);
  force_simd (result);
  return result;
}

int __attribute__ ((noinline))
test_vdupb_lane_u8 ()
{
  uint8x8_t a;
  uint8_t b;
  uint8_t c[8] = { 0, 1, 2, 3, 4, 5, 6, 7 };

  a = vld1_u8 (c);
  b = wrap_vdupb_lane_u8_0 (a, a);
  if (c[0] != b)
    return 1;
  b = wrap_vdupb_lane_u8_1 (a);
  if (c[1] != b)
    return 1;
  return 0;
}

int16_t __attribute__ ((noinline))
wrap_vduph_lane_s16_0 (int16x4_t dummy, int16x4_t a)
{
  int16_t result = vduph_lane_s16 (a, 0);
  force_simd (result);
  return result;
}

int16_t __attribute__ ((noinline))
wrap_vduph_lane_s16_1 (int16x4_t a)
{
  int16_t result = vduph_lane_s16 (a, 1);
  force_simd (result);
  return result;
}

int __attribute__ ((noinline))
test_vduph_lane_s16 ()
{
  int16x4_t a;
  int16_t b;
  int16_t c[4] = { 0, 1, 2, 3 };

  a = vld1_s16 (c);
  b = wrap_vduph_lane_s16_0 (a, a);
  if (c[0] != b)
    return 1;
  b = wrap_vduph_lane_s16_1 (a);
  if (c[1] != b)
    return 1;
  return 0;
}

uint16_t __attribute__ ((noinline))
wrap_vduph_lane_u16_0 (uint16x4_t dummy, uint16x4_t a)
{
  uint16_t result = vduph_lane_u16 (a, 0);
  force_simd (result);
  return result;
}

uint16_t __attribute__ ((noinline))
wrap_vduph_lane_u16_1 (uint16x4_t a)
{
  uint16_t result = vduph_lane_u16 (a, 1);
  force_simd (result);
  return result;
}

int __attribute__ ((noinline))
test_vduph_lane_u16 ()
{
  uint16x4_t a;
  uint16_t b;
  uint16_t c[4] = { 0, 1, 2, 3 };

  a = vld1_u16 (c);
  b = wrap_vduph_lane_u16_0 (a, a);
  if (c[0] != b)
    return 1;
  b = wrap_vduph_lane_u16_1 (a);
  if (c[1] != b)
    return 1;
  return 0;
}

int32_t __attribute__ ((noinline))
wrap_vdups_lane_s32_0 (int32x2_t dummy, int32x2_t a)
{
  int32_t result = vdups_lane_s32 (a, 0);
  force_simd (result);
  return result;
}

int32_t __attribute__ ((noinline))
wrap_vdups_lane_s32_1 (int32x2_t a)
{
  int32_t result = vdups_lane_s32 (a, 1);
  force_simd (result);
  return result;
}

int __attribute__ ((noinline))
test_vdups_lane_s32 ()
{
  int32x2_t a;
  int32_t b;
  int32_t c[2] = { 0, 1 };

  a = vld1_s32 (c);
  b = wrap_vdups_lane_s32_0 (vcreate_s32 (0), a);
  if (c[0] != b)
    return 1;
  b = wrap_vdups_lane_s32_1 (a);
  if (c[1] != b)
    return 1;
  return 0;
}

uint32_t __attribute__ ((noinline))
wrap_vdups_lane_u32_0 (uint32x2_t dummy, uint32x2_t a)
{
  uint32_t result = vdups_lane_u32 (a, 0);
  force_simd (result);
  return result;
}

uint32_t __attribute__ ((noinline))
wrap_vdups_lane_u32_1 (uint32x2_t a)
{
  uint32_t result = vdups_lane_u32 (a, 1);
  force_simd (result);
  return result;
}

int __attribute__ ((noinline))
test_vdups_lane_u32 ()
{
  uint32x2_t a;
  uint32_t b;
  uint32_t c[2] = { 0, 1 };
  a = vld1_u32 (c);
  b = wrap_vdups_lane_u32_0 (a, a);
  if (c[0] != b)
    return 1;
  b = wrap_vdups_lane_u32_1 (a);
  if (c[1] != b)
    return 1;
  return 0;
}

uint64_t __attribute__ ((noinline))
wrap_vdupd_lane_u64_0 (uint64x1_t dummy, uint64x1_t a)
{
  return vdupd_lane_u64 (a, 0);;
}

int __attribute__ ((noinline))
test_vdupd_lane_u64 ()
{
  uint64x1_t a;
  uint64_t b;
  uint64_t c[1] = { 0 };

  a = vld1_u64 (c);
  b = wrap_vdupd_lane_u64_0 (a, a);
  if (c[0] != b)
    return 1;
  return 0;
}

int64_t __attribute__ ((noinline))
wrap_vdupd_lane_s64_0 (int64x1_t dummy, int64x1_t a)
{
  return vdupd_lane_s64 (a, 0);
}

int __attribute__ ((noinline))
test_vdupd_lane_s64 ()
{
  int64x1_t a;
  int64_t b;
  int64_t c[1] = { 0 };

  a = vld1_s64 (c);
  b = wrap_vdupd_lane_s64_0 (a, a);
  if (c[0] != b)
    return 1;
  return 0;
}

int
main ()
{
  if (test_vdups_lane_f32 ())
    abort ();
  if (test_vdupd_lane_f64 ())
    abort ();
  if (test_vdupb_lane_s8 ())
    abort ();
  if (test_vdupb_lane_u8 ())
    abort ();
  if (test_vduph_lane_s16 ())
    abort ();
  if (test_vduph_lane_u16 ())
    abort ();
  if (test_vdups_lane_s32 ())
    abort ();
  if (test_vdups_lane_u32 ())
    abort ();
  if (test_vdupd_lane_s64 ())
    abort ();
  if (test_vdupd_lane_u64 ())
    abort ();
  return 0;
}

/* Asm check for vdupb_lane_s8, vdupb_lane_u8.  */
/* { dg-final { scan-assembler-not "dup\\tb\[0-9\]+, v\[0-9\]+\.b\\\[0\\\]" } } */
/* { dg-final { scan-assembler-times "dup\\tb\[0-9\]+, v\[0-9\]+\.b\\\[1\\\]" 2 } } */

/* Asm check for vduph_lane_h16, vduph_lane_h16.  */
/* { dg-final { scan-assembler-not "dup\\th\[0-9\]+, v\[0-9\]+\.h\\\[0\\\]" } } */
/* { dg-final { scan-assembler-times "dup\\th\[0-9\]+, v\[0-9\]+\.h\\\[1\\\]" 2 } } */

/* Asm check for vdups_lane_f32, vdups_lane_s32, vdups_lane_u32.  */
/* Can't generate "dup s<n>, v<m>[0]" for vdups_lane_s32 and vdups_lane_u32.  */
/* { dg-final { scan-assembler-times "dup\\ts\[0-9\]+, v\[0-9\]+\.s\\\[0\\\]" 1} } */
/* { dg-final { scan-assembler-times "dup\\ts\[0-9\]+, v\[0-9\]+\.s\\\[1\\\]" 3 } } */

/* Asm check for vdupd_lane_f64, vdupd_lane_s64, vdupd_lane_u64.  */
/* Attempts to make the compiler generate vdupd are not practical.  */
/* { dg-final { scan-assembler-not "dup\\td\[0-9\]+, v\[0-9\]+\.d\\\[0\\\]" } } */

