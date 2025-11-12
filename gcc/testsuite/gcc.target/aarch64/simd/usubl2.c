/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

void foo(int16_t *dst, const uint8_t *src0, const uint8_t *src1)
{
  uint8x16_t s0 = vld1q_u8 (src0);
  uint8x16_t s1 = vld1q_u8 (src1);

  uint16x8_t d0_lo = vsubl_u8 (vget_low_u8 (s0), vget_low_u8 (s1));
  uint16x8_t d0_hi = vsubl_u8 (vget_high_u8 (s0), vget_high_u8 (s1));
    
  vst1q_s16 (dst, vreinterpretq_s16_u16 (d0_lo));
  vst1q_s16 (dst + 8, vreinterpretq_s16_u16 (d0_hi));
}

/* { dg-final { scan-assembler "usubl\tv\[0-9\]+\.8h,\ v\[0-9\]+\.8b,\ v\[0-9\]+\.8b" } } */
/* { dg-final { scan-assembler "usubl2\tv\[0-9\]+\.8h,\ v\[0-9\]+\.16b,\ v\[0-9\]+\.16b" } } */
