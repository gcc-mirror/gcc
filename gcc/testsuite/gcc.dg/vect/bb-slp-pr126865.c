/* { dg-do compile } */

#include <stdint.h>

uint16_t g2, g24, g11;
_Bool g28_c11;
uint16_t g28()
{
  g24 = 10690 / (int16_t)g2;
  if (g28_c11)
    g11 = (int16_t)(uintptr_t)g28 / 9 + g24;
  __builtin_abort();
}
