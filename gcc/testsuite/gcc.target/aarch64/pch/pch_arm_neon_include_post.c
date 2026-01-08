#include "pch_arm_neon_include_post.h"
#include "arm_neon.h"

uint32x4_t add_vectors(uint32x4_t a, uint32x4_t b)
{
  return vaddq_u32(a, b);
}
