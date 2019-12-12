/* { dg-do compile } */
/* { dg-require-effective-target arm_v8_3a_complex_neon_ok } */
/* { dg-require-effective-target arm_v8_2a_fp16_neon_ok } */
/* { dg-add-options arm_v8_3a_complex_neon } */
/* { dg-add-options arm_v8_2a_fp16_neon } */
/* { dg-additional-options "-O2 -march=armv8.3-a+fp16 -save-temps" } */

#include <arm_neon.h>

float16x4_t
test_vcadd_rot90_f16 (float16x4_t __a, float16x4_t __b)
{
  return vcadd_rot90_f16 (__a, __b);
}

float16x8_t
test_vcaddq_rot90_f16 (float16x8_t __a, float16x8_t __b)
{
  return vcaddq_rot90_f16 (__a, __b);
}

float16x4_t
test_vcadd_rot270_f16 (float16x4_t __a, float16x4_t __b)
{
  return vcadd_rot270_f16 (__a, __b);
}

float16x8_t
test_vcaddq_rot270_f16 (float16x8_t __a, float16x8_t __b)
{
  return vcaddq_rot270_f16 (__a, __b);
}

float16x4_t
test_vcmla_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_f16 (__r, __a, __b);
}

float16x8_t
test_vcmlaq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_f16 (__r, __a, __b);
}

float16x4_t
test_vcmla_lane_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_lane_f16 (__r, __a, __b, 0);
}

float16x4_t
test_vcmla_laneq_f16 (float16x4_t __r, float16x4_t __a, float16x8_t __b)
{
  return vcmla_laneq_f16 (__r, __a, __b, 0);
}

float16x8_t
test_vcmlaq_lane_f16 (float16x8_t __r, float16x8_t __a, float16x4_t __b)
{
  return vcmlaq_lane_f16 (__r, __a, __b, 0);
}

float16x8_t
test_vcmlaq_laneq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_laneq_f16 (__r, __a, __b, 0);
}

float16x4_t
test_vcmla_lane_f16_2 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_lane_f16 (__r, __a, __b, 1);
}

float16x4_t
test_vcmla_laneq_f16_2 (float16x4_t __r, float16x4_t __a, float16x8_t __b)
{
  return vcmla_laneq_f16 (__r, __a, __b, 3);
}

float16x8_t
test_vcmlaq_lane_f16_2 (float16x8_t __r, float16x8_t __a, float16x4_t __b)
{
  return vcmlaq_lane_f16 (__r, __a, __b, 1);
}

float16x8_t
test_vcmlaq_laneq_f16_2 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_laneq_f16 (__r, __a, __b, 3);
}

float16x4_t
test_vcmla_rot90_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot90_f16 (__r, __a, __b);
}

float16x8_t
test_vcmlaq_rot90_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot90_f16 (__r, __a, __b);
}

float16x4_t
test_vcmla_rot90_lane_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot90_lane_f16 (__r, __a, __b, 0);
}

float16x4_t
test_vcmla_rot90_laneq_f16 (float16x4_t __r, float16x4_t __a, float16x8_t __b)
{
  return vcmla_rot90_laneq_f16 (__r, __a, __b, 0);
}

float16x8_t
test_vcmlaq_rot90_lane_f16 (float16x8_t __r, float16x8_t __a, float16x4_t __b)
{
  return vcmlaq_rot90_lane_f16 (__r, __a, __b, 0);
}

float16x8_t
test_vcmlaq_rot90_laneq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot90_laneq_f16 (__r, __a, __b, 0);
}

float16x4_t
test_vcmla_rot90_lane_f16_2 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot90_lane_f16 (__r, __a, __b, 1);
}

float16x4_t
test_vcmla_rot90_laneq_f16_2 (float16x4_t __r, float16x4_t __a, float16x8_t __b)
{
  return vcmla_rot90_laneq_f16 (__r, __a, __b, 3);
}

float16x8_t
test_vcmlaq_rot90_lane_f16_2 (float16x8_t __r, float16x8_t __a, float16x4_t __b)
{
  return vcmlaq_rot90_lane_f16 (__r, __a, __b, 1);
}

float16x8_t
test_vcmlaq_rot90_laneq_f16_2 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot90_laneq_f16 (__r, __a, __b, 3);
}

float16x4_t
test_vcmla_rot180_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot180_f16 (__r, __a, __b);
}

float16x8_t
test_vcmlaq_rot180_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot180_f16 (__r, __a, __b);
}

float16x4_t
test_vcmla_rot180_lane_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot180_lane_f16 (__r, __a, __b, 0);
}

float16x4_t
test_vcmla_rot180_laneq_f16 (float16x4_t __r, float16x4_t __a, float16x8_t __b)
{
  return vcmla_rot180_laneq_f16 (__r, __a, __b, 0);
}

float16x8_t
test_vcmlaq_rot180_lane_f16 (float16x8_t __r, float16x8_t __a, float16x4_t __b)
{
  return vcmlaq_rot180_lane_f16 (__r, __a, __b, 0);
}

float16x8_t
test_vcmlaq_rot180_laneq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot180_laneq_f16 (__r, __a, __b, 0);
}

float16x4_t
test_vcmla_rot180_lane_f16_2 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot180_lane_f16 (__r, __a, __b, 1);
}

float16x4_t
test_vcmla_rot180_laneq_f16_2 (float16x4_t __r, float16x4_t __a, float16x8_t __b)
{
  return vcmla_rot180_laneq_f16 (__r, __a, __b, 3);
}

float16x8_t
test_vcmlaq_rot180_lane_f16_2 (float16x8_t __r, float16x8_t __a, float16x4_t __b)
{
  return vcmlaq_rot180_lane_f16 (__r, __a, __b, 1);
}

float16x8_t
test_vcmlaq_rot180_laneq_f16_2 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot180_laneq_f16 (__r, __a, __b, 3);
}

float16x4_t
test_vcmla_rot270_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot270_f16 (__r, __a, __b);
}

float16x8_t
test_vcmlaq_rot270_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot270_f16 (__r, __a, __b);
}

float16x4_t
test_vcmla_rot270_lane_f16 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot270_lane_f16 (__r, __a, __b, 0);
}

float16x4_t
test_vcmla_rot270_laneq_f16 (float16x4_t __r, float16x4_t __a, float16x8_t __b)
{
  return vcmla_rot270_laneq_f16 (__r, __a, __b, 0);
}

float16x8_t
test_vcmlaq_rot270_lane_f16 (float16x8_t __r, float16x8_t __a, float16x4_t __b)
{
  return vcmlaq_rot270_lane_f16 (__r, __a, __b, 0);
}

float16x8_t
test_vcmlaq_rot270_laneq_f16 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot270_laneq_f16 (__r, __a, __b, 0);
}

float16x4_t
test_vcmla_rot270_lane_f16_2 (float16x4_t __r, float16x4_t __a, float16x4_t __b)
{
  return vcmla_rot270_lane_f16 (__r, __a, __b, 1);
}

float16x4_t
test_vcmla_rot270_laneq_f16_2 (float16x4_t __r, float16x4_t __a, float16x8_t __b)
{
  return vcmla_rot270_laneq_f16 (__r, __a, __b, 3);
}

float16x8_t
test_vcmlaq_rot270_lane_f16_2 (float16x8_t __r, float16x8_t __a, float16x4_t __b)
{
  return vcmlaq_rot270_lane_f16 (__r, __a, __b, 1);
}

float16x8_t
test_vcmlaq_rot270_laneq_f16_2 (float16x8_t __r, float16x8_t __a, float16x8_t __b)
{
  return vcmlaq_rot270_laneq_f16 (__r, __a, __b, 3);
}

/* { dg-final { scan-assembler-times {dup\td[0-9]+, v[0-9]+.d\[1\]} 4 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.4h, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.4h, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcadd\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.4h, #0} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.4h, #180} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.4h, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.4h, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.h\[0\], #0} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.h\[0\], #180} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.h\[0\], #270} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.h\[0\], #90} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.h\[1\], #0} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.h\[1\], #180} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.h\[1\], #270} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.4h, v[0-9]+.4h, v[0-9]+.h\[1\], #90} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h, #0} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h, #180} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h, #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.8h, #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[0\], #0} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[0\], #180} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[0\], #270} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[0\], #90} 2 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[1\], #0} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[1\], #180} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[1\], #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[1\], #90} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[3\], #0} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[3\], #180} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[3\], #270} 1 { target { aarch64*-*-* } } } } */
/* { dg-final { scan-assembler-times {fcmla\tv[0-9]+.8h, v[0-9]+.8h, v[0-9]+.h\[3\], #90} 1 { target { aarch64*-*-* } } } } */

/* { dg-final { scan-assembler-times {vcadd.f16\td[0-9]+, d[0-9]+, d[0-9]+, #90} 2 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcadd.f16\tq[0-9]+, q[0-9]+, q[0-9]+, #90} 2 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+, #0} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+\[0\], #0} 3 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+\[0\], #180} 3 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+\[0\], #270} 3 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+\[0\], #90} 3 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+\[1\], #0} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+\[1\], #180} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+\[1\], #270} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+, #180} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+\[1\], #90} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+, #270} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\td[0-9]+, d[0-9]+, d[0-9]+, #90} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[0\], #0} 3 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[0\], #180} 3 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[0\], #270} 3 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[0\], #90} 3 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[1\], #0} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[1\], #180} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[1\], #270} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, d[0-9]+\[1\], #90} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, q[0-9]+, #0} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, q[0-9]+, #180} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, q[0-9]+, #270} 1 { target { arm*-*-* } } } } */
/* { dg-final { scan-assembler-times {vcmla.f16\tq[0-9]+, q[0-9]+, q[0-9]+, #90} 1 { target { arm*-*-* } } } } */
