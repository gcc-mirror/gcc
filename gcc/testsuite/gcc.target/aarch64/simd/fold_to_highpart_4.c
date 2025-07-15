/* { dg-do compile } */
/* { dg-require-effective-target aarch64_little_endian } */
/* { dg-options "-O -fdump-tree-optimized" } */

#include "arm_neon.h"

#define VEC_CST_u8  0x0102030405060708
#define VEC_CST_u16 0x0001000200030004
#define VEC_CST_u32 0x0000000100000002

/* Extend the 64b VECTOR_CST to the type required by the hi builtin.  */

uint16x8_t
test_u8 (uint8x16_t a)
{
  const uint8x8_t b = vcreate_u8 (VEC_CST_u8);
  return vmull_u8 (vget_high_u8 (a), b);
}

/* { dg-final { scan-tree-dump-times "\{ 8, 7, 6, 5, 4, 3, 2, 1, 8, 7, 6, 5, 4, 3, 2, 1 \}" 1 "optimized" } } */

uint32x4_t
test_u16 (uint16x8_t a)
{
  const uint16x4_t b = vcreate_u16 (VEC_CST_u16);
  return vmull_u16 (vget_high_u16 (a), b);
}

/* { dg-final { scan-tree-dump-times "\{ 4, 3, 2, 1, 4, 3, 2, 1 \}" 1 "optimized" } } */

uint64x2_t
test_u32 (uint32x4_t a)
{
  const uint32x2_t b = vcreate_u32 (VEC_CST_u32);
  return vmull_u32 (vget_high_u32 (a), b);
}

/* { dg-final { scan-tree-dump-times "\{ 2, 1, 2, 1 \}" 1 "optimized" } } */
