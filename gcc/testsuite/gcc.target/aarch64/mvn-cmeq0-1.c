/* { dg-do assemble } */
/* { dg-options "-O --save-temps" } */

#include <arm_neon.h>

uint8x8_t bar(int16x8_t abs_row0, int16x8_t row0) {
  uint16x8_t row0_diff =
    vreinterpretq_u16_s16(veorq_s16(abs_row0, vshrq_n_s16(row0, 15)));
  uint8x8_t abs_row0_gt0 =
    vmovn_u16(vcgtq_u16(vreinterpretq_u16_s16(abs_row0), vdupq_n_u16(0)));
  return abs_row0_gt0;
}


/* { dg-final { scan-assembler-times {\tcmtst\t} 1 } } */
/* { dg-final { scan-assembler-not {\tcmeq\t} } } */
/* { dg-final { scan-assembler-not {\tnot\t} } } */
