/* { dg-do compile } */
/* { dg-additional-options "-fdump-rtl-expand -w" } */

#include <arm_neon.h>

void foo (uint8x8x2_t cols_01_23, uint8x8x2_t cols_45_67, uint16_t*
outptr0) {
  uint16x4x4_t cols_01_23_45_67 = { {
    vreinterpret_u16_u8(cols_01_23.val[0]),
    vreinterpret_u16_u8(cols_01_23.val[1]),
    vreinterpret_u16_u8(cols_45_67.val[0]),
    vreinterpret_u16_u8(cols_45_67.val[1])
  } };

  vst4_lane_u16(outptr0, cols_01_23_45_67, 0); }

/* Check that we expand to v0 and v2 from the function arguments.  */
/* { dg-final { scan-rtl-dump {\(reg:V2x8QI \d+ v0 \[ cols_01_23
\]\)} expand } } */
/* { dg-final { scan-rtl-dump {\(reg:V2x8QI \d+ v2 \[ cols_45_67
\]\)} expand } } */

