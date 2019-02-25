#include "arm_neon.h"

float32x2_t
test_vfmlal_high_f16 (float32x2_t r, float16x4_t a, float16x4_t b)
{
  return vfmlal_high_f16 (r, a, b);
}

float32x4_t
test_vfmlalq_high_f16 (float32x4_t r, float16x8_t a, float16x8_t b)
{
  return vfmlalq_high_f16 (r, a, b);
}

float32x2_t
test_vfmlsl_high_f16 (float32x2_t r, float16x4_t a, float16x4_t b)
{
  return vfmlsl_high_f16 (r, a, b);
}

float32x4_t
test_vfmlslq_high_f16 (float32x4_t r, float16x8_t a, float16x8_t b)
{
  return vfmlslq_high_f16 (r, a, b);
}
