#include "pch_arm_sve_include_post.h"
#include "arm_sve.h"

svint32_t add_vectors(svint32_t a, svint32_t b)
{
  return svadd_s32_z(svptrue_b32(), a, b);
}
