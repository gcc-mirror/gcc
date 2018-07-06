#include "arm_neon.h"

uint64x2_t
test_vsha512hq_u64 (uint64x2_t a, uint64x2_t b, uint64x2_t c)
{
  return vsha512hq_u64 (a, b, c);
}

uint64x2_t
test_vsha512h2q_u64 (uint64x2_t a, uint64x2_t b, uint64x2_t c)
{
  return vsha512h2q_u64 (a, b, c);
}

uint64x2_t
test_vsha512su0q_u64 (uint64x2_t a, uint64x2_t b)
{
  return vsha512su0q_u64 (a, b);
}

uint64x2_t
test_vsha512su1q_u64 (uint64x2_t a, uint64x2_t b, uint64x2_t c)
{
  return vsha512su1q_u64 (a, b, c);
}
