/* { dg-do assemble { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_v8_2a_i8mm_ok } */
/* { dg-add-options arm_v8_2a_i8mm }  */
/* { dg-additional-options "--save-temps" } */

#include <arm_neon.h>

/* Unsigned-Signed Dot Product instructions.  */

int32x2_t usfoo_lane (int32x2_t r, uint8x8_t x, int8x8_t y)
{
  /* { dg-error "lane -1 out of range 0 - 1" "" { target *-*-* } 0 } */
  return vusdot_lane_s32 (r, x, y, -1);
}


int32x4_t usfooq_lane (int32x4_t r, uint8x16_t x, int8x8_t y)
{
  /* { dg-error "lane 2 out of range 0 - 1" "" { target *-*-* } 0 } */
  return vusdotq_lane_s32 (r, x, y, 2);
}
