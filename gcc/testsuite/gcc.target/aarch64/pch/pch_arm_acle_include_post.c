#include "pch_arm_acle_include_post.h"
#include <arm_acle.h>

uint32_t ror(uint32_t a, uint32_t b)
{
  return __ror(a, b);
}
