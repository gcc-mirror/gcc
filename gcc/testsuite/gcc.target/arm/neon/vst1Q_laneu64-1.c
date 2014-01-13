/* Test the `vst1Q_laneu64' ARM Neon intrinsic.  */

/* Detect ICE in the case of unaligned memory address.  */

/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-add-options arm_neon } */

#include "arm_neon.h"

unsigned char dummy_store[1000];

void
foo (char* addr)
{
  uint8x16_t vdata = vld1q_u8 (addr);
  vst1q_lane_u64 ((uint64_t*) &dummy_store, vreinterpretq_u64_u8 (vdata), 0);
}

uint64_t
bar (uint64x2_t vdata)
{
  vdata = vld1q_lane_u64 ((uint64_t*) &dummy_store, vdata, 0);
  return vgetq_lane_u64 (vdata, 0);
}
