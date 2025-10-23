/* { dg-require-effective-target arm_v8_1m_mve_fp_ok } */
/* { dg-add-options arm_v8_1m_mve_fp } */
/* { dg-additional-options "-O2" } */

/* PR target/122223.  */

#include <arm_mve.h>

#ifdef __cplusplus
extern "C" {
#endif

float32x4_t foo() {
  float32x4_t a = vdupq_n_f32(1.0f); /* 0x3f800000 */
  float32x4_t b = vbicq_f32(a, a);   /* 0x3f800000 & ~0x3f800000 => 0x00000000 */
  float32x4_t c = vbicq_f32(a, b);   /* 0x3f800000 & ~0x00000000 => 0x3f800000 */
  return c;
}

#ifdef __cplusplus
}
#endif

/* { dg-final { scan-assembler-not "vmov.f32\tq\[0-9\]+, #0.0" } } */
/* { dg-final { scan-assembler "vmov.f32\tq\[0-9\]+, #1.0" } } */
