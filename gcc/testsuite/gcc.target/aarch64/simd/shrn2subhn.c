/* This test case checks that replacing a not+shift by a sub -1 works. */
/* { dg-do compile } */
/* { dg-additional-options "-O1" } */
/* { dg-final { scan-assembler-times "\\tsubhn\\t" 6 } } */

#include<arm_neon.h>

uint8x8_t neg_narrow_v8hi(uint16x8_t a) {
  uint16x8_t b = vmvnq_u16(a);
  return vshrn_n_u16(b, 8);
}

uint8x8_t neg_narrow_vsubhn_v8hi(uint16x8_t a) {
  uint16x8_t ones = vdupq_n_u16(0xffff);
  return vsubhn_u16(ones, a);
}

uint16x4_t neg_narrow_v4si(uint32x4_t a) {
  uint32x4_t b = vmvnq_u32(a);
  return vshrn_n_u32(b, 16);
}

uint16x4_t neg_narrow_vsubhn_v4si(uint32x4_t a) {
  uint32x4_t ones = vdupq_n_u32(0xffffffff);
  return vsubhn_u32(ones, a);
}

uint32x2_t neg_narrow_v2di(uint64x2_t a) {
  uint64x2_t b = ~a;
  return vshrn_n_u64(b, 32);
}

uint32x2_t neg_narrow_vsubhn_v2di(uint64x2_t a) {
  uint64x2_t ones = vdupq_n_u64(0xffffffffffffffff);
  return vsubhn_u64(ones, a);
}
