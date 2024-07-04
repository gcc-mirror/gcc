/* { dg-options "-Ofast" } */

#include <arm_neon.h>

int16x8x3_t bsl(const uint16x8x3_t *check, const int16x8x3_t *in1,
                              const int16x8x3_t *in2) {
  int16x8x3_t out;
  for (uint32_t j = 0; j < 3; j++) {
    out.val[j] = vbslq_s16(check->val[j], in1->val[j], in2->val[j]);
  }
  return out;
}

/* { dg-final { scan-assembler-not {\tmov\t} } } */
