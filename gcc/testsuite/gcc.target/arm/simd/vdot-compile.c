/* { dg-do compile } */
/* { dg-additional-options "-O3" } */
/* { dg-require-effective-target arm_v8_2a_dotprod_neon_ok } */
/* { dg-add-options arm_v8_2a_dotprod_neon }  */

#include <arm_neon.h>

/* Unsigned Dot Product instructions.  */

uint32x2_t ufoo (uint32x2_t r, uint8x8_t x, uint8x8_t y)
{
  return vdot_u32 (r, x, y);
}

uint32x4_t ufooq (uint32x4_t r, uint8x16_t x, uint8x16_t y)
{
  return vdotq_u32 (r, x, y);
}

uint32x2_t ufoo_lane (uint32x2_t r, uint8x8_t x, uint8x8_t y)
{
  return vdot_lane_u32 (r, x, y, 0);
}

uint32x4_t ufooq_lane (uint32x4_t r, uint8x16_t x, uint8x8_t y)
{
  return vdotq_lane_u32 (r, x, y, 0);
}

/* Signed Dot Product instructions.  */

int32x2_t sfoo (int32x2_t r, int8x8_t x, int8x8_t y)
{
  return vdot_s32 (r, x, y);
}

int32x4_t sfooq (int32x4_t r, int8x16_t x, int8x16_t y)
{
  return vdotq_s32 (r, x, y);
}

int32x2_t sfoo_lane (int32x2_t r, int8x8_t x, int8x8_t y)
{
  return vdot_lane_s32 (r, x, y, 0);
}

int32x4_t sfooq_lane (int32x4_t r, int8x16_t x, int8x8_t y)
{
  return vdotq_lane_s32 (r, x, y, 0);
}

/* { dg-final { scan-assembler-times {v[us]dot\.[us]8\td[0-9]+, d[0-9]+, d[0-9]+} 4 } } */
/* { dg-final { scan-assembler-times {v[us]dot\.[us]8\tq[0-9]+, q[0-9]+, q[0-9]+} 2 } } */
/* { dg-final { scan-assembler-times {v[us]dot\.[us]8\td[0-9]+, d[0-9]+, d[0-9]+\[#?[0-9]\]} 2 } } */
/* { dg-final { scan-assembler-times {v[us]dot\.[us]8\tq[0-9]+, q[0-9]+, d[0-9]+\[#?[0-9]\]} 2 } } */

