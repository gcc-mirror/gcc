/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_i8mm_ok } */
/* { dg-add-options arm_v8_2a_i8mm }  */
/* { dg-additional-options "--save-temps" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

int32x2_t sfoo_lane (int32x2_t r, int8x8_t x, uint8x8_t y)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" { target *-*-* } 0 } */
  return vsudot_lane_s32 (r, x, y, -1);
}

int32x2_t sfoo_laneq (int32x2_t r, int8x8_t x, uint8x16_t y)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vsudot_laneq_s32 (r, x, y, -1);
}

int32x4_t sfooq_lane (int32x4_t r, int8x16_t x, uint8x8_t y)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" { target *-*-* } 0 } */
  return vsudotq_lane_s32 (r, x, y, 2);
}

int32x4_t sfooq_laneq (int32x4_t r, int8x16_t x, uint8x16_t y)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vsudotq_laneq_s32 (r, x, y, 4);
}
