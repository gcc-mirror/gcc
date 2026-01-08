#include "pch_arm_neon_sve_bridge_include_post.h"
#include <arm_neon_sve_bridge.h>

svint8_t set(svint8_t a, int8x16_t b)
{
  return svset_neonq(a, b);
}
