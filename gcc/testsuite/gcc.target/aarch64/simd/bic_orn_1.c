/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

int64x2_t bic_16b (int32x4_t a, int32x4_t b) {
  return vandq_s64 (vreinterpretq_s64_s32 (vmvnq_s32 (a)),
		    vreinterpretq_s64_s32 (b));
}

int16x4_t orn_8b (int32x2_t a, int32x2_t b) {
  return vorr_s16 (vreinterpret_s16_s32 (a),
		   vreinterpret_s16_s32 (vmvn_s32 (b)));
}

/* { dg-final { scan-assembler {\tbic\tv[0-9]+\.16b} } } */
/* { dg-final { scan-assembler {\torn\tv[0-9]+\.8b} } } */
