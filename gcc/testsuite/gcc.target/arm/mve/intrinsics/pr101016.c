/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */

#include "arm_mve.h"

void
foo (void)
{
  mve_pred16_t p;
  int8x16_t a;
  int8_t a1[10];
  int16x8_t b;
  int16_t b1[10];
  int32x4_t c;
  int32_t c1[10];
  uint8x16_t ua;
  uint8_t ua1[10];
  uint16x8_t ub;
  uint16_t ub1[10];
  uint32x4_t uc;
  uint32_t uc1[10];
  float16x8_t fb;
  float16_t fb1[10];
  float32x4_t fc;
  float32_t fc1[10];

  fb = vld1q (fb1);
  fc = vld1q (fc1);
  b = vld1q (b1);
  c = vld1q (c1);
  a = vld1q (a1);
  ub = vld1q (ub1);
  uc = vld1q (uc1);
  ua = vld1q (ua1);
  fb = vld1q_z (fb1, p);
  fc = vld1q_z (fc1, p);
  b = vld1q_z (b1, p);
  c = vld1q_z (c1, p);
  a = vld1q_z (a1, p);
  ub = vld1q_z (ub1, p);
  uc = vld1q_z (uc1, p);
  ua = vld1q_z (ua1, p);
}

void
foo1 (void)
{
  mve_pred16_t p;
  int8x16x2_t a;
  int8_t a1[10];
  int16x8x2_t b;
  int16_t b1[10];
  int32x4x2_t c;
  int32_t c1[10];
  uint8x16x2_t ua;
  uint8_t ua1[10];
  uint16x8x2_t ub;
  uint16_t ub1[10];
  uint32x4x2_t uc;
  uint32_t uc1[10];
  float16x8x2_t fb;
  float16_t fb1[10];
  float32x4x2_t fc;
  float32_t fc1[10];

  fb = vld2q (fb1);
  fc = vld2q (fc1);
  b = vld2q (b1);
  c = vld2q (c1);
  a = vld2q (a1);
  ub = vld2q (ub1);
  uc = vld2q (uc1);
  ua = vld2q (ua1);
}

void
foo2 (void)
{
  mve_pred16_t p;
  int8x16x4_t a;
  int8_t a1[10];
  int16x8x4_t b;
  int16_t b1[10];
  int32x4x4_t c;
  int32_t c1[10];
  uint8x16x4_t ua;
  uint8_t ua1[10];
  uint16x8x4_t ub;
  uint16_t ub1[10];
  uint32x4x4_t uc;
  uint32_t uc1[10];
  float16x8x4_t fb;
  float16_t fb1[10];
  float32x4x4_t fc;
  float32_t fc1[10];

  fb = vld4q (fb1);
  fc = vld4q (fc1);
  b = vld4q (b1);
  c = vld4q (c1);
  a = vld4q (a1);
  ub = vld4q (ub1);
  uc = vld4q (uc1);
  ua = vld4q (ua1);
}

void
foo3 (void)
{
  mve_pred16_t p;
  int16x8_t a;
  uint16x8_t ua;
  int8_t a1[10];
  uint8_t ua1[10];
  uint16x8_t offset_a;
  int8x16_t b;
  uint8x16_t ub;
  uint8x16_t offset_b;
  int32x4_t c;
  uint32x4_t uc;
  uint32x4_t offset_c;

  a = vldrbq_gather_offset (a1, offset_a);
  ua = vldrbq_gather_offset (ua1, offset_a);
  b = vldrbq_gather_offset (a1, offset_b);
  ub = vldrbq_gather_offset (ua1, offset_b);
  c = vldrbq_gather_offset (a1, offset_c);
  uc = vldrbq_gather_offset (ua1, offset_c);
  a = vldrbq_gather_offset_z (a1, offset_a, p);
  ua = vldrbq_gather_offset_z (ua1, offset_a, p);
  b = vldrbq_gather_offset_z (a1, offset_b, p);
  ub = vldrbq_gather_offset_z (ua1, offset_b, p);
  c = vldrbq_gather_offset_z (a1, offset_c, p);
  uc = vldrbq_gather_offset_z (ua1, offset_c, p);
}
/* { dg-final { scan-assembler-not "__ARM_undef" } } */
