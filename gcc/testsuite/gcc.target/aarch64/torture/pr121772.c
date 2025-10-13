/* { dg-do compile } */
/* { dg-options "-fchecking" } */
#include <arm_neon.h>
int16_t f(int16x4_t b) {
  return vaddvq_s16(vcombine_s16(b, vdup_n_s16 (0)));
}
