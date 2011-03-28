/* { dg-do compile } */
/* { dg-require-effective-target arm_neon_ok } */
/* { dg-options "-O1" } */
/* { dg-add-options arm_neon } */

#include <arm_neon.h>

uint8x16_t
foo (uint8_t *a, uint8x16_t b)
{
  vst1q_lane_u8 (a, b, 14);
  return vld1q_lane_u8 (a + 0x100, b, 15);
}
