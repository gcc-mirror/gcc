#include <arm_neon.h>

float32x4_t
foo(float32x4_t x, bfloat16x8_t a)
{
  register bfloat16x4_t b asm ("v16");
  asm volatile ("" : "=w"(b));
  return vbfmlalbq_lane_f32 (x, a, b, 0);
}
