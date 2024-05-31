/* { dg-do compile } */

#include <arm_sve.h>

#pragma GCC target ("arch=armv9.2-a+sve2+lut")

void
test (svfloat16_t f16, svfloat16x2_t f16x2,
      svuint8_t u8, svuint16_t u16, svuint16x2_t u16x2,
      svint8_t s8, svint16_t s16, svint16x2_t s16x2,
      svbfloat16_t bf16, svbfloat16x2_t bf16x2)
{
  svluti2_lane (f16, u8, 0);
  svluti2_lane (bf16, u8, 0);

  svluti2_lane (u8, u8, 0);
  svluti2_lane (u16, u8, 0);

  svluti2_lane (s8, u8, 0);
  svluti2_lane (s16, u8, 0);

  svluti4_lane (f16, u8, 0);
  svluti4_lane (bf16, u8, 0);
  svluti4_lane (f16x2, u8, 0);
  svluti4_lane (bf16x2, u8, 0);

  svluti4_lane (u8, u8, 0);
  svluti4_lane (u16, u8, 0);
  svluti4_lane (u16x2, u8, 0);

  svluti4_lane (s8, u8, 0);
  svluti4_lane (s16, u8, 0);
  svluti4_lane (s16x2, u8, 0);
}
