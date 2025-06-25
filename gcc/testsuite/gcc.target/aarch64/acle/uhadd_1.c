/* Test if SIMD fused unsigned halving adds are generated */
/* { dg-do compile } */
/* { dg-options "-O2" } */

#include <arm_neon.h>

#define FUSED_SIMD_UHADD(vectype, q, ts, mask) \
  vectype simd_uhadd ## q ## _ ## ts ## _1 (vectype a) \
  { \
    vectype v1 = vand ## q ## _ ## ts (a, vdup ## q ## _n_ ## ts (mask)); \
    vectype v2 = vdup ## q ## _n_ ## ts (mask); \
    return vshr ## q ## _n_ ## ts (vadd ## q ## _ ## ts (v1, v2), 1); \
  } \
  \
  vectype simd_uhadd ## q ## _ ## ts ## _2 (vectype a, vectype b) \
  { \
    vectype v1 = vand ## q ## _ ## ts (a, vdup ## q ## _n_ ## ts (mask)); \
    vectype v2 = vand ## q ## _ ## ts (b, vdup ## q ## _n_ ## ts (mask)); \
    return vshr ## q ## _n_ ## ts (vadd ## q ## _ ## ts (v1, v2), 1); \
  }

FUSED_SIMD_UHADD (uint8x8_t, , u8, 0x7f)
FUSED_SIMD_UHADD (uint8x16_t, q, u8, 0x7f)
FUSED_SIMD_UHADD (uint16x4_t, , u16, 0x7fff)
FUSED_SIMD_UHADD (uint16x8_t, q, u16, 0x7fff)
FUSED_SIMD_UHADD (uint32x2_t, , u32, 0x7fffffff)
FUSED_SIMD_UHADD (uint32x4_t, q, u32, 0x7fffffff)

/* { dg-final { scan-assembler-times {\tuhadd\tv[0-9]+\.8b,} 2 } } */
/* { dg-final { scan-assembler-times {\tuhadd\tv[0-9]+\.16b,} 2 } } */
/* { dg-final { scan-assembler-times {\tuhadd\tv[0-9]+\.4h,} 2 } } */
/* { dg-final { scan-assembler-times {\tuhadd\tv[0-9]+\.8h,} 2 } } */
/* { dg-final { scan-assembler-times {\tuhadd\tv[0-9]+\.2s,} 2 } } */
/* { dg-final { scan-assembler-times {\tuhadd\tv[0-9]+\.4s,} 2 } } */
