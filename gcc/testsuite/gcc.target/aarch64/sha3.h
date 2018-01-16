#include "arm_neon.h"

uint16x8_t
test_veor3q_u16 (uint16x8_t a, uint16x8_t b, uint16x8_t c)
{
  return veor3q_u16 (a, b, c);
}

uint64x2_t
test_vrax1q_u64 (uint64x2_t a, uint64x2_t b)
{
  return vrax1q_u64 (a, b);
}

uint64x2_t
test_vxarq_u64 (uint64x2_t a, uint64x2_t b)
{
  return vxarq_u64 (a, b, 15);
}

uint16x8_t
test_vbcaxq_u16 (uint16x8_t a, uint16x8_t b, uint16x8_t c)
{
  return vbcaxq_u16 (a, b, c);
}
