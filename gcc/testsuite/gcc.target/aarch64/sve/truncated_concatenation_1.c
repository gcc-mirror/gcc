/* { dg-do compile } */
/* { dg-options "-O3 -Wall -march=armv8.2-a+sve" } */

#include <arm_neon.h>
#include <arm_sve.h>

int8x8_t f1 (int16x4_t a, int16x4_t b) {
    int8x8_t ab = vdup_n_s8 (0);
    int16x8_t ab_concat = vcombine_s16 (a, b);
    ab = vmovn_s16 (ab_concat);
    return ab;
}

int16x4_t f2 (int32x2_t a, int32x2_t b) {
    int16x4_t ab = vdup_n_s16 (0);
    int32x4_t ab_concat = vcombine_s32 (a, b);
    ab = vmovn_s32 (ab_concat);
    return ab;
}

int32x2_t f3 (svint64_t a, svint64_t b) {
    int32x2_t ab = vdup_n_s32 (0);
    ab = vset_lane_s32 ((int)svaddv_s64 (svptrue_b64 (), a), ab, 0);
    ab = vset_lane_s32 ((int)svaddv_s64 (svptrue_b64 (), b), ab, 1);
    return ab;
}

/* { dg-final { scan-assembler-not {\txtn\t} } }*/
/* { dg-final { scan-assembler-not {\tfcvtn\t} } }*/
/* { dg-final { scan-assembler-times {\tuzp1\tv[0-9]+\.8b, v[0-9]+\.8b, v[0-9]+\.8b} 1 } }*/
/* { dg-final { scan-assembler-times {\tuzp1\tv[0-9]+\.4h, v[0-9]+\.4h, v[0-9]+\.4h} 1 } }*/
/* { dg-final { scan-assembler-times {\tuzp1\tv[0-9]+\.2s, v[0-9]+\.2s, v[0-9]+\.2s} 1 } }*/