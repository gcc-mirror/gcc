/* { dg-options "-O2 -fno-inline" } */
/* { dg-add-options arm_neon } */
#include <stdint.h>
#include "arm_neon.h"

volatile uint8_t v40 = 255;

volatile uint8x8_t result = {
#if __BYTE_ORDER__ == __ORDER_LITTLE_ENDIAN__
  0, 0, 0, 0, 0, 0, 0, 255
#else
  255, 0, 0, 0, 0, 0, 0, 0
#endif
};

void check (uint8x8_t v)
{
  int i;
  for (i = 0; i < 8; i++)
    if (v[i] != result[i])
      __builtin_abort ();
}

int main ()
{
  uint8_t v116[16] = {0};
  uint8x8_t v117 = vld1_dup_u8(v116); // 0, ..., 0
  uint8x8_t v119 = vset_lane_u8(v40, v117, 7); // 0, ..., 0, 0xff
  check (v119);

  return 0;
}
