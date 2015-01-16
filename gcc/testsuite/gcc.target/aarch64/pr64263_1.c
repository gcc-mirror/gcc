/* { dg-do compile } */
/* { dg-options "-O1" } */

#include "arm_neon.h"

extern long int vget_lane_s64_1 (int64x1_t, const int);

void
foo ()
{
  int8x8_t val14;
  int8x8_t val15;
  uint8x8_t val16;
  uint32x4_t val40;
  val14 = vcreate_s8 (0xff0080f6807f807fUL);
  val15 = vcreate_s8 (0x10807fff7f808080UL);
  val16 = vcgt_s8 (val14, val15);
  val40 = vreinterpretq_u32_u64 (
    vdupq_n_u64 (
         vget_lane_s64_1 (
         vreinterpret_s64_u8 (val16), 0)
    ));
}
