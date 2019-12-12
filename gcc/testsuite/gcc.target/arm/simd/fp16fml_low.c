/* { dg-do compile } */
/* { dg-require-effective-target arm_fp16fml_neon_ok } */
/* { dg-add-options arm_fp16fml_neon }  */

#include "arm_neon.h"

float32x2_t
test_vfmlal_low_f16 (float32x2_t r, float16x4_t a, float16x4_t b)
{
  return vfmlal_low_f16 (r, a, b);
}

float32x4_t
test_vfmlalq_low_f16 (float32x4_t r, float16x8_t a, float16x8_t b)
{
  return vfmlalq_low_f16 (r, a, b);
}

float32x2_t
test_vfmlsl_low_f16 (float32x2_t r, float16x4_t a, float16x4_t b)
{
  return vfmlsl_low_f16 (r, a, b);
}

float32x4_t
test_vfmlslq_low_f16 (float32x4_t r, float16x8_t a, float16x8_t b)
{
  return vfmlslq_low_f16 (r, a, b);
}

/* { dg-final { scan-assembler-times {vfmal.f16\td[0-9]+, s[123]?[02468], s[123]?[02468]} 1 } } */
/* { dg-final { scan-assembler-times {vfmal.f16\tq[0-9]+, d[123]?[02468], d[123]?[02468]} 1 } } */
/* { dg-final { scan-assembler-times {vfmsl.f16\td[0-9]+, s[123]?[02468], s[123]?[02468]} 1 } } */
/* { dg-final { scan-assembler-times {vfmsl.f16\tq[0-9]+, d[123]?[02468], d[123]?[02468]} 1 } } */
