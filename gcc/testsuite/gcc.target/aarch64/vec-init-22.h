#include <arm_neon.h>

int16x8_t f_s16 (int16_t x0, int16_t x1, int16_t x2, int16_t x3,
                 int16_t x4, int16_t x5, int16_t x6, int16_t x7)
{
  return (int16x8_t) { x0, x1, x2, x3, x4, x5, x6, x7 };
}
