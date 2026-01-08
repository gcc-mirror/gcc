#include "pch_arm_multiple_include_post.h"
#include <arm_sme.h>
#include <arm_sve.h>
#include <arm_neon_sve_bridge.h>
#include <arm_neon.h>
#include <arm_acle.h>

svint32_t add_vectors(svint32_t a, svint32_t b)
{
  return svadd_s32_z(svptrue_b32(), a, b);
}
