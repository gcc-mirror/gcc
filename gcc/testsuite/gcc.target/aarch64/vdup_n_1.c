/* Test vdup_lane intrinsics work correctly.  */
/* { dg-do run } */
/* { dg-options "-O1 --save-temps" } */

#include <arm_neon.h>

extern void abort (void);

float32x2_t __attribute__ ((noinline))
wrap_vdup_n_f32 (float32_t a)
{
  return vdup_n_f32 (a);
}

int __attribute__ ((noinline))
test_vdup_n_f32 ()
{
  float32_t a = 1.0;
  float32x2_t b;
  float32_t c[2];
  int i;

  b = wrap_vdup_n_f32 (a);
  vst1_f32 (c, b);
  for (i = 0; i < 2; i++)
    if (a != c[i])
      return 1;
  return 0;
}

float32x4_t __attribute__ ((noinline))
wrap_vdupq_n_f32 (float32_t a)
{
  return vdupq_n_f32 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_f32 ()
{
  float32_t a = 1.0;
  float32x4_t b;
  float32_t c[4];
  int i;

  b = wrap_vdupq_n_f32 (a);
  vst1q_f32 (c, b);
  for (i = 0; i < 4; i++)
    if (a != c[i])
      return 1;
  return 0;
}

float64x1_t __attribute__ ((noinline))
wrap_vdup_n_f64 (float64_t a)
{
  return vdup_n_f64 (a);
}

int __attribute__ ((noinline))
test_vdup_n_f64 ()
{
  float64_t a = 1.0;
  float64x1_t b;
  float64_t c[1];
  int i;

  b = wrap_vdup_n_f64 (a);
  vst1_f64 (c, b);
  for (i = 0; i < 1; i++)
    if (a != c[i])
      return 1;
  return 0;
}

float64x2_t __attribute__ ((noinline))
wrap_vdupq_n_f64 (float64_t a)
{
  return vdupq_n_f64 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_f64 ()
{
  float64_t a = 1.0;
  float64x2_t b;
  float64_t c[2];
  int i;

  b = wrap_vdupq_n_f64 (a);
  vst1q_f64 (c, b);
  for (i = 0; i < 2; i++)
    if (a != c[i])
      return 1;
  return 0;
}

poly8x8_t __attribute__ ((noinline))
wrap_vdup_n_p8 (poly8_t a)
{
  return vdup_n_p8 (a);
}

int __attribute__ ((noinline))
test_vdup_n_p8 ()
{
  poly8_t a = 1;
  poly8x8_t b;
  poly8_t c[8];
  int i;

  b = wrap_vdup_n_p8 (a);
  vst1_p8 (c, b);
  for (i = 0; i < 8; i++)
    if (a != c[i])
      return 1;
  return 0;
}

poly8x16_t __attribute__ ((noinline))
wrap_vdupq_n_p8 (poly8_t a)
{
  return vdupq_n_p8 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_p8 ()
{
  poly8_t a = 1;
  poly8x16_t b;
  poly8_t c[16];
  int i;

  b = wrap_vdupq_n_p8 (a);
  vst1q_p8 (c, b);
  for (i = 0; i < 16; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int8x8_t __attribute__ ((noinline))
wrap_vdup_n_s8 (int8_t a)
{
  return vdup_n_s8 (a);
}

int __attribute__ ((noinline))
test_vdup_n_s8 ()
{
  int8_t a = 1;
  int8x8_t b;
  int8_t c[8];
  int i;

  b = wrap_vdup_n_s8 (a);
  vst1_s8 (c, b);
  for (i = 0; i < 8; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int8x16_t __attribute__ ((noinline))
wrap_vdupq_n_s8 (int8_t a)
{
  return vdupq_n_s8 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_s8 ()
{
  int8_t a = 1;
  int8x16_t b;
  int8_t c[16];
  int i;

  b = wrap_vdupq_n_s8 (a);
  vst1q_s8 (c, b);
  for (i = 0; i < 16; i++)
    if (a != c[i])
      return 1;
  return 0;
}

uint8x8_t __attribute__ ((noinline))
wrap_vdup_n_u8 (uint8_t a)
{
  return vdup_n_u8 (a);
}

int __attribute__ ((noinline))
test_vdup_n_u8 ()
{
  uint8_t a = 1;
  uint8x8_t b;
  uint8_t c[8];
  int i;

  b = wrap_vdup_n_u8 (a);
  vst1_u8 (c, b);
  for (i = 0; i < 8; i++)
    if (a != c[i])
      return 1;
  return 0;
}

uint8x16_t __attribute__ ((noinline))
wrap_vdupq_n_u8 (uint8_t a)
{
  return vdupq_n_u8 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_u8 ()
{
  uint8_t a = 1;
  uint8x16_t b;
  uint8_t c[16];
  int i;

  b = wrap_vdupq_n_u8 (a);
  vst1q_u8 (c, b);
  for (i = 0; i < 16; i++)
    if (a != c[i])
      return 1;
  return 0;
}

poly16x4_t __attribute__ ((noinline))
wrap_vdup_n_p16 (poly16_t a)
{
  return vdup_n_p16 (a);
}

int __attribute__ ((noinline))
test_vdup_n_p16 ()
{
  poly16_t a = 1;
  poly16x4_t b;
  poly16_t c[4];
  int i;

  b = wrap_vdup_n_p16 (a);
  vst1_p16 (c, b);
  for (i = 0; i < 4; i++)
    if (a != c[i])
      return 1;
  return 0;
}

poly16x8_t __attribute__ ((noinline))
wrap_vdupq_n_p16 (poly16_t a)
{
  return vdupq_n_p16 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_p16 ()
{
  poly16_t a = 1;
  poly16x8_t b;
  poly16_t c[8];
  int i;

  b = wrap_vdupq_n_p16 (a);
  vst1q_p16 (c, b);
  for (i = 0; i < 8; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int16x4_t __attribute__ ((noinline))
wrap_vdup_n_s16 (int16_t a)
{
  return vdup_n_s16 (a);
}

int __attribute__ ((noinline))
test_vdup_n_s16 ()
{
  int16_t a = 1;
  int16x4_t b;
  int16_t c[4];
  int i;

  b = wrap_vdup_n_s16 (a);
  vst1_s16 (c, b);
  for (i = 0; i < 4; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int16x8_t __attribute__ ((noinline))
wrap_vdupq_n_s16 (int16_t a)
{
  return vdupq_n_s16 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_s16 ()
{
  int16_t a = 1;
  int16x8_t b;
  int16_t c[8];
  int i;

  b = wrap_vdupq_n_s16 (a);
  vst1q_s16 (c, b);
  for (i = 0; i < 8; i++)
    if (a != c[i])
      return 1;
  return 0;
}

uint16x4_t __attribute__ ((noinline))
wrap_vdup_n_u16 (uint16_t a)
{
  return vdup_n_u16 (a);
}

int __attribute__ ((noinline))
test_vdup_n_u16 ()
{
  uint16_t a = 1;
  uint16x4_t b;
  uint16_t c[4];
  int i;

  b = wrap_vdup_n_u16 (a);
  vst1_u16 (c, b);
  for (i = 0; i < 4; i++)
    if (a != c[i])
      return 1;
  return 0;
}

uint16x8_t __attribute__ ((noinline))
wrap_vdupq_n_u16 (uint16_t a)
{
  return vdupq_n_u16 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_u16 ()
{
  uint16_t a = 1;
  uint16x8_t b;
  uint16_t c[8];
  int i;

  b = wrap_vdupq_n_u16 (a);
  vst1q_u16 (c, b);
  for (i = 0; i < 8; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int32x2_t __attribute__ ((noinline))
wrap_vdup_n_s32 (int32_t a)
{
  return vdup_n_s32 (a);
}

int __attribute__ ((noinline))
test_vdup_n_s32 ()
{
  int32_t a = 1;
  int32x2_t b;
  int32_t c[2];
  int i;

  b = wrap_vdup_n_s32 (a);
  vst1_s32 (c, b);
  for (i = 0; i < 2; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int32x4_t __attribute__ ((noinline))
wrap_vdupq_n_s32 (int32_t a)
{
  return vdupq_n_s32 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_s32 ()
{
  int32_t a = 1;
  int32x4_t b;
  int32_t c[4];
  int i;

  b = wrap_vdupq_n_s32 (a);
  vst1q_s32 (c, b);
  for (i = 0; i < 4; i++)
    if (a != c[i])
      return 1;
  return 0;
}

uint32x2_t __attribute__ ((noinline))
wrap_vdup_n_u32 (uint32_t a)
{
  return vdup_n_u32 (a);
}

int __attribute__ ((noinline))
test_vdup_n_u32 ()
{
  uint32_t a = 1;
  uint32x2_t b;
  uint32_t c[2];
  int i;

  b = wrap_vdup_n_u32 (a);
  vst1_u32 (c, b);
  for (i = 0; i < 2; i++)
    if (a != c[i])
      return 1;
  return 0;
}

uint32x4_t __attribute__ ((noinline))
wrap_vdupq_n_u32 (uint32_t a)
{
  return vdupq_n_u32 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_u32 ()
{
  uint32_t a = 1;
  uint32x4_t b;
  uint32_t c[4];
  int i;

  b = wrap_vdupq_n_u32 (a);
  vst1q_u32 (c, b);
  for (i = 0; i < 4; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int64x1_t __attribute__ ((noinline))
wrap_vdup_n_s64 (int64_t a)
{
  return vdup_n_s64 (a);
}

int __attribute__ ((noinline))
test_vdup_n_s64 ()
{
  int64_t a = 1;
  int64x1_t b;
  int64_t c[1];
  int i;

  b = wrap_vdup_n_s64 (a);
  vst1_s64 (c, b);
  for (i = 0; i < 1; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int64x2_t __attribute__ ((noinline))
wrap_vdupq_n_s64 (int64_t a)
{
  return vdupq_n_s64 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_s64 ()
{
  int64_t a = 1;
  int64x2_t b;
  int64_t c[2];
  int i;

  b = wrap_vdupq_n_s64 (a);
  vst1q_s64 (c, b);
  for (i = 0; i < 2; i++)
    if (a != c[i])
      return 1;
  return 0;
}

uint64x1_t __attribute__ ((noinline))
wrap_vdup_n_u64 (uint64_t a)
{
  return vdup_n_u64 (a);
}

int __attribute__ ((noinline))
test_vdup_n_u64 ()
{
  uint64_t a = 1;
  uint64x1_t b;
  uint64_t c[1];
  int i;

  b = wrap_vdup_n_u64 (a);
  vst1_u64 (c, b);
  for (i = 0; i < 1; i++)
    if (a != c[i])
      return 1;
  return 0;
}

uint64x2_t __attribute__ ((noinline))
wrap_vdupq_n_u64 (uint64_t a)
{
  return vdupq_n_u64 (a);
}

int __attribute__ ((noinline))
test_vdupq_n_u64 ()
{
  uint64_t a = 1;
  uint64x2_t b;
  uint64_t c[2];
  int i;

  b = wrap_vdupq_n_u64 (a);
  vst1q_u64 (c, b);
  for (i = 0; i < 2; i++)
    if (a != c[i])
      return 1;
  return 0;
}

int
main ()
{
  if (test_vdup_n_f32 ())
    abort ();
  if (test_vdup_n_f64 ())
    abort ();
  if (test_vdup_n_p8 ())
    abort ();
  if (test_vdup_n_u8 ())
    abort ();
  if (test_vdup_n_s8 ())
    abort ();
  if (test_vdup_n_p16 ())
    abort ();
  if (test_vdup_n_s16 ())
    abort ();
  if (test_vdup_n_u16 ())
    abort ();
  if (test_vdup_n_s32 ())
    abort ();
  if (test_vdup_n_u32 ())
    abort ();
  if (test_vdup_n_s64 ())
    abort ();
  if (test_vdup_n_u64 ())
    abort ();
  if (test_vdupq_n_f32 ())
    abort ();
  if (test_vdupq_n_f64 ())
    abort ();
  if (test_vdupq_n_p8 ())
    abort ();
  if (test_vdupq_n_u8 ())
    abort ();
  if (test_vdupq_n_s8 ())
    abort ();
  if (test_vdupq_n_p16 ())
    abort ();
  if (test_vdupq_n_s16 ())
    abort ();
  if (test_vdupq_n_u16 ())
    abort ();
  if (test_vdupq_n_s32 ())
    abort ();
  if (test_vdupq_n_u32 ())
    abort ();
  if (test_vdupq_n_s64 ())
    abort ();
  if (test_vdupq_n_u64 ())
    abort ();
  return 0;
}

/* No asm checks for vdup_n_f32, vdupq_n_f32, vdup_n_f64 and vdupq_n_f64.
   Cannot force floating point value in general purpose regester.  */

/* Asm check for test_vdup_n_p8, test_vdup_n_s8, test_vdup_n_u8.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8b, w\[0-9\]+" 3 } } */

/* Asm check for test_vdupq_n_p8, test_vdupq_n_s8, test_vdupq_n_u8.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.16b, w\[0-9\]+" 3 } } */

/* Asm check for test_vdup_n_p16, test_vdup_n_s16, test_vdup_n_u16.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4h, w\[0-9\]+" 3 } } */

/* Asm check for test_vdupq_n_p16, test_vdupq_n_s16, test_vdupq_n_u16.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.8h, w\[0-9\]+" 3 } } */

/* Asm check for test_vdup_n_s32, test_vdup_n_u32.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.2s, w\[0-9\]+" 2 } } */

/* Asm check for test_vdupq_n_s32, test_vdupq_n_u32.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.4s, w\[0-9\]+" 2 } } */

/* Asm check for test_vdup_n_s64, test_vdup_n_u64 are left out.
   Attempts to make the compiler generate "dup\\td\[0-9\]+, x\[0-9\]+"
   are not practical.  */

/* Asm check for test_vdupq_n_s64, test_vdupq_n_u64.  */
/* { dg-final { scan-assembler-times "dup\\tv\[0-9\]+\.2d, x\[0-9\]+" 2 } } */

/* { dg-final { cleanup-saved-temps } } */
