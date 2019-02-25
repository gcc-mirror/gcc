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
