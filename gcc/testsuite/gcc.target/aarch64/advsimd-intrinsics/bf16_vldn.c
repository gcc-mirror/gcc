/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */
/* { dg-require-effective-target arm_v8_2a_bf16_neon_ok } */
/* { dg-add-options arm_v8_2a_bf16_neon }  */

#include <arm_neon.h>

bfloat16x4_t
test_vld1_dup_bf16 (bfloat16_t * ptr)
{
  return vld1_dup_bf16 (ptr);
}

bfloat16x8_t
test_vld1q_dup_bf16 (bfloat16_t * ptr)
{
  return vld1q_dup_bf16 (ptr);
}

bfloat16x4_t
test_vld1_lane_bf16 (bfloat16_t * ptr, bfloat16x4_t src)
{
  return vld1_lane_bf16 (ptr, src, 3);
}

bfloat16x8_t
test_vld1q_lane_bf16 (bfloat16_t * ptr, bfloat16x8_t src)
{
  return vld1q_lane_bf16 (ptr, src, 7);
}

bfloat16x4_t
test_vld1_bf16 (bfloat16_t * ptr)
{
  return vld1_bf16 (ptr);
}

bfloat16x8_t
test_vld1q_bf16 (bfloat16_t * ptr)
{
  return vld1q_bf16 (ptr);
}

bfloat16x4x2_t
test_vld1_bf16_x2 (bfloat16_t * ptr)
{
  return vld1_bf16_x2 (ptr);
}

bfloat16x8x2_t
test_vld1q_bf16_x2 (bfloat16_t * ptr)
{
  return vld1q_bf16_x2 (ptr);
}

bfloat16x4x3_t
test_vld1_bf16_x3 (bfloat16_t * ptr)
{
  return vld1_bf16_x3 (ptr);
}

bfloat16x8x3_t
test_vld1q_bf16_x3 (bfloat16_t * ptr)
{
  return vld1q_bf16_x3 (ptr);
}

bfloat16x4x4_t
test_vld1_bf16_x4 (bfloat16_t * ptr)
{
  return vld1_bf16_x4 (ptr);
}

bfloat16x8x4_t
test_vld1q_bf16_x4 (bfloat16_t * ptr)
{
  return vld1q_bf16_x4 (ptr);
}

bfloat16x4x2_t
test_vld2_bf16 (bfloat16_t * ptr)
{
  return vld2_bf16 (ptr);
}

bfloat16x8x2_t
test_vld2q_bf16 (bfloat16_t * ptr)
{
  return vld2q_bf16 (ptr);
}

bfloat16x4x2_t
test_vld2_dup_bf16 (bfloat16_t * ptr)
{
  return vld2_dup_bf16 (ptr);
}

bfloat16x8x2_t
test_vld2q_dup_bf16 (bfloat16_t * ptr)
{
  return vld2q_dup_bf16 (ptr);
}

bfloat16x4x3_t
test_vld3_bf16 (bfloat16_t * ptr)
{
  return vld3_bf16 (ptr);
}

bfloat16x8x3_t
test_vld3q_bf16 (bfloat16_t * ptr)
{
  return vld3q_bf16 (ptr);
}

bfloat16x4x3_t
test_vld3_dup_bf16 (bfloat16_t * ptr)
{
  return vld3_dup_bf16 (ptr);
}

bfloat16x8x3_t
test_vld3q_dup_bf16 (bfloat16_t * ptr)
{
  return vld3q_dup_bf16 (ptr);
}

bfloat16x4x4_t
test_vld4_bf16 (bfloat16_t * ptr)
{
 return vld4_bf16 (ptr);
}

bfloat16x8x4_t
test_vld4q_bf16 (bfloat16_t * ptr)
{
 return vld4q_bf16 (ptr);
}

bfloat16x4x4_t
test_vld4_dup_bf16 (bfloat16_t * ptr)
{
  return vld4_dup_bf16 (ptr);
}

bfloat16x8x4_t
test_vld4q_dup_bf16 (bfloat16_t * ptr)
{
  return vld4q_dup_bf16 (ptr);
}
