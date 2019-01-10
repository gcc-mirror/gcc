/* { dg-skip-if "" { arm-*-* } } */
/* { dg-do assemble } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-add-options arm_v8_3a_complex_neon }  */
/* { dg-additional-options "-O2 -save-temps" } */

#include <arm_neon.h>

float32x2_t
test_vcadd_rot90_f32 (float32x2_t __a, float32x2_t __b)
{
  return vcadd_rot90_f32 (__a, __b);
}

float32x4_t
test_vcaddq_rot90_f32 (float32x4_t __a, float32x4_t __b)
{
  return vcaddq_rot90_f32 (__a, __b);
}

#ifdef __ARM_ARCH_ISA_A64
float64x2_t
test_vcaddq_rot90_f64 (float64x2_t __a, float64x2_t __b)
{
  return vcaddq_rot90_f64 (__a, __b);
}
#endif

float32x2_t
test_vcadd_rot270_f32 (float32x2_t __a, float32x2_t __b)
{
  return vcadd_rot270_f32 (__a, __b);
}

float32x4_t
test_vcaddq_rot270_f32 (float32x4_t __a, float32x4_t __b)
{
  return vcaddq_rot270_f32 (__a, __b);
}

#ifdef __ARM_ARCH_ISA_A64
float64x2_t
test_vcaddq_rot270_f64 (float64x2_t __a, float64x2_t __b)
{
  return vcaddq_rot270_f64 (__a, __b);
}
#endif

float32x2_t
test_vcmla_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return vcmla_f32 (__r, __a, __b);
}

float32x4_t
test_vcmlaq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return vcmlaq_f32 (__r, __a, __b);
}

#ifdef __ARM_ARCH_ISA_A64
float64x2_t
test_vcmlaq_f64 (float64x2_t __r, float64x2_t __a, float64x2_t __b)
{
  return vcmlaq_f64 (__r, __a, __b);
}
#endif

float32x2_t
test_vcmla_lane_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return vcmla_lane_f32 (__r, __a, __b, 0);
}

float32x2_t
test_vcmla_laneq_f32 (float32x2_t __r, float32x2_t __a, float32x4_t __b)
{
  return vcmla_laneq_f32 (__r, __a, __b, 1);
}

float32x4_t
test_vcmlaq_lane_f32 (float32x4_t __r, float32x4_t __a, float32x2_t __b)
{
  return vcmlaq_lane_f32 (__r, __a, __b, 0);
}

float32x4_t
test_vcmlaq_laneq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return vcmlaq_laneq_f32 (__r, __a, __b, 1);
}

float32x2_t
test_vcmla_rot90_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return vcmla_rot90_f32 (__r, __a, __b);
}

float32x4_t
test_vcmlaq_rot90_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return vcmlaq_rot90_f32 (__r, __a, __b);
}

#ifdef __ARM_ARCH_ISA_A64
float64x2_t
test_vcmlaq_rot90_f64 (float64x2_t __r, float64x2_t __a, float64x2_t __b)
{
  return vcmlaq_rot90_f64 (__r, __a, __b);
}
#endif

float32x2_t
test_vcmla_rot90_lane_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return vcmla_rot90_lane_f32 (__r, __a, __b, 0);
}

float32x2_t
test_vcmla_rot90_laneq_f32 (float32x2_t __r, float32x2_t __a, float32x4_t __b)
{
  return vcmla_rot90_laneq_f32 (__r, __a, __b, 1);
}

float32x4_t
test_vcmlaq_rot90_lane_f32 (float32x4_t __r, float32x4_t __a, float32x2_t __b)
{
  return vcmlaq_rot90_lane_f32 (__r, __a, __b, 0);
}

float32x4_t
test_vcmlaq_rot90_laneq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return vcmlaq_rot90_laneq_f32 (__r, __a, __b, 1);
}

float32x2_t
test_vcmla_rot180_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return vcmla_rot180_f32 (__r, __a, __b);
}

float32x4_t
test_vcmlaq_rot180_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return vcmlaq_rot180_f32 (__r, __a, __b);
}

#ifdef __ARM_ARCH_ISA_A64
float64x2_t
test_vcmlaq_rot180_f64 (float64x2_t __r, float64x2_t __a, float64x2_t __b)
{
  return vcmlaq_rot180_f64 (__r, __a, __b);
}
#endif

float32x2_t
test_vcmla_rot180_lane_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return vcmla_rot180_lane_f32 (__r, __a, __b, 0);
}

float32x2_t
test_vcmla_rot180_laneq_f32 (float32x2_t __r, float32x2_t __a, float32x4_t __b)
{
  return vcmla_rot180_laneq_f32 (__r, __a, __b, 1);
}

float32x4_t
test_vcmlaq_rot180_lane_f32 (float32x4_t __r, float32x4_t __a, float32x2_t __b)
{
  return vcmlaq_rot180_lane_f32 (__r, __a, __b, 0);
}

float32x4_t
test_vcmlaq_rot180_laneq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return vcmlaq_rot180_laneq_f32 (__r, __a, __b, 1);
}

float32x2_t
test_vcmla_rot270_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return vcmla_rot270_f32 (__r, __a, __b);
}

float32x4_t
test_vcmlaq_rot270_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return vcmlaq_rot270_f32 (__r, __a, __b);
}

#ifdef __ARM_ARCH_ISA_A64
float64x2_t
test_vcmlaq_rot270_f64 (float64x2_t __r, float64x2_t __a, float64x2_t __b)
{
  return vcmlaq_rot270_f64 (__r, __a, __b);
}
#endif

float32x2_t
test_vcmla_rot270_lane_f32 (float32x2_t __r, float32x2_t __a, float32x2_t __b)
{
  return vcmla_rot270_lane_f32 (__r, __a, __b, 0);
}

float32x2_t
test_vcmla_rot270_laneq_f32 (float32x2_t __r, float32x2_t __a, float32x4_t __b)
{
  return vcmla_rot270_laneq_f32 (__r, __a, __b, 1);
}

float32x4_t
test_vcmlaq_rot270_lane_f32 (float32x4_t __r, float32x4_t __a, float32x2_t __b)
{
  return vcmlaq_rot270_lane_f32 (__r, __a, __b, 0);
}

float32x4_t
test_vcmlaq_rot270_laneq_f32 (float32x4_t __r, float32x4_t __a, float32x4_t __b)
{
  return vcmlaq_rot270_laneq_f32 (__r, __a, __b, 1);
}

/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.2s, v[0-9]+.2s, v[0-9]+.2s, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.2s, v[0-9]+.2s, v[0-9]+.2s, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d, #0} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d, #180} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.2d, v[0-9]+.2d, v[0-9]+.2d, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.2s, v[0-9]+.2s, v[0-9]+.2s, #0} 3 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.2s, v[0-9]+.2s, v[0-9]+.2s, #180} 3 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.2s, v[0-9]+.2s, v[0-9]+.2s, #270} 3 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.2s, v[0-9]+.2s, v[0-9]+.2s, #90} 3 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s, #0} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s, #180} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.4s, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.s\[0\], #0} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.s\[0\], #180} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.s\[0\], #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.s\[0\], #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.s\[1\], #0} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.s\[1\], #180} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.s\[1\], #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4s, v[0-9]+.4s, v[0-9]+.s\[1\], #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {dup\td[0-9]+, v[0-9]+.d\[1\]} 4 { target { aarch64*-*-* } } } } */
