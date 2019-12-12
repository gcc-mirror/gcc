/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16fml_neon_ok } */
/* { dg-add-options arm_fp16fml_neon }  */

#include "arm_neon.h"

float32x2_t
test_vfmlal_lane_high_f16 (float32x2_t r, float16x4_t a, float16x4_t b)
{
  return vfmlal_lane_high_f16 (r, a, b, 0);
}

float32x2_t
tets_vfmlsl_lane_high_f16  (float32x2_t r, float16x4_t a, float16x4_t b)
{
  return vfmlsl_lane_high_f16 (r, a, b, 0);
}

float32x2_t
test_vfmlal_laneq_high_f16 (float32x2_t r, float16x4_t a, float16x8_t b)
{
  return vfmlal_laneq_high_f16 (r, a, b, 6);
}

float32x2_t
test_vfmlsl_laneq_high_f16 (float32x2_t r, float16x4_t a, float16x8_t b)
{
  return vfmlsl_laneq_high_f16 (r, a, b, 6);
}

float32x4_t
test_vfmlalq_lane_high_f16 (float32x4_t r, float16x8_t a, float16x4_t b)
{
  return vfmlalq_lane_high_f16 (r, a, b, 1);
}

float32x4_t
test_vfmlslq_lane_high_f16 (float32x4_t r, float16x8_t a, float16x4_t b)
{
  return vfmlslq_lane_high_f16 (r, a, b, 1);
}

float32x4_t
test_vfmlalq_laneq_high_f16  (float32x4_t r, float16x8_t a, float16x8_t b)
{
  return vfmlalq_laneq_high_f16 (r, a, b, 7);
}

float32x4_t
test_vfmlslq_laneq_high_f16 (float32x4_t r, float16x8_t a, float16x8_t b)
{
  return vfmlslq_laneq_high_f16 (r, a, b, 7);
}

/* { dg-final { scan-assembler-times {vfmal.f16\td[0-9]+, s[123]?[13579], s[123]?[02468]\[0\]} 1 } } */
/* { dg-final { scan-assembler-times {vfmal.f16\td[0-9]+, s[123]?[13579], s[123]?[13579]\[0\]} 1 } } */
/* { dg-final { scan-assembler-times {vfmal.f16\tq[0-9]+, d[123]?[13579], d[0-9]+\[1\]} 1 } } */
/* { dg-final { scan-assembler-times {vfmal.f16\tq[0-9]+, d[123]?[13579], d[123]?[13579]\[3\]} 1 } } */

/* { dg-final { scan-assembler-times {vfmsl.f16\td[0-9]+, s[123]?[13579], s[123]?[02468]\[0\]} 1 } } */
/* { dg-final { scan-assembler-times {vfmsl.f16\td[0-9]+, s[123]?[13579], s[123]?[13579]\[0\]} 1 } } */
/* { dg-final { scan-assembler-times {vfmsl.f16\tq[0-9]+, d[123]?[13579], d[0-9]+\[1\]} 1 } } */
/* { dg-final { scan-assembler-times {vfmsl.f16\tq[0-9]+, d[123]?[13579], d[123]?[13579]\[3\]} 1 } } */
