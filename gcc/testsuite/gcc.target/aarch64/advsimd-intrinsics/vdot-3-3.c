/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_i8mm_ok } */
/* { dg-add-options arm_v8_2a_i8mm }  */
/* { dg-additional-options "--save-temps" } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

int32x2_t ufoo_lane (int32x2_t r, uint8x8_t x, int8x8_t y)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" { target *-*-* } 0 } */
  return vusdot_lane_s32 (r, x, y, -1);
}

int32x2_t ufoo_laneq (int32x2_t r, uint8x8_t x, int8x16_t y)
{
  /* { dg-error "lane -1 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vusdot_laneq_s32 (r, x, y, -1);
}

int32x4_t ufooq_lane (int32x4_t r, uint8x16_t x, int8x8_t y)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" { target *-*-* } 0 } */
  return vusdotq_lane_s32 (r, x, y, 2);
}

int32x4_t ufooq_laneq (int32x4_t r, uint8x16_t x, int8x16_t y)
{
  /* { dg-error "lane 4 out of range 0 - 3" "" { target *-*-* } 0 } */
  return vusdotq_laneq_s32 (r, x, y, 4);
}
