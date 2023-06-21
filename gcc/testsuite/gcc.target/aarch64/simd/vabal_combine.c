/* { dg-do compile } */
/* { dg-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" } } */

#include <arm_neon.h>

/*
** test_vabal_s8:
**      sabal2	v0.8h, v2.16b, v1.16b
**      ret
*/
int16x8_t
test_vabal_s8 (int16x8_t sadv, int8x16_t pv, int8x16_t sv)
{
  return vabal_s8 (sadv, vget_high_s8 (pv), vget_high_s8 (sv));
}

/*
** test_vabal_u8:
**      uabal2	v0.8h, v2.16b, v1.16b
**      ret
*/
uint16x8_t
test_vabal_u8 (uint16x8_t sadv, uint8x16_t pv, uint8x16_t sv)
{
  return vabal_u8 (sadv, vget_high_u8 (pv), vget_high_u8 (sv));
}

/*
** test_vabal_s16:
**      sabal2	v0.4s, v2.8h, v1.8h
**      ret
*/
int32x4_t
test_vabal_s16 (int32x4_t sadv, int16x8_t pv, int16x8_t sv)
{
  return vabal_s16 (sadv, vget_high_s16 (pv), vget_high_s16 (sv));
}

/*
** test_vabal_u16:
**      uabal2	v0.4s, v2.8h, v1.8h
**      ret
*/
uint32x4_t
test_vabal_u16 (uint32x4_t sadv, uint16x8_t pv, uint16x8_t sv)
{
  return vabal_u16 (sadv, vget_high_u16 (pv), vget_high_u16 (sv));
}

/*
** test_vabal_s32:
**      sabal2	v0.2d, v2.4s, v1.4s
**      ret
*/
int64x2_t
test_vabal_s32 (int64x2_t sadv, int32x4_t pv, int32x4_t sv)
{
  return vabal_s32 (sadv, vget_high_s32 (pv), vget_high_s32 (sv));
}

/*
** test_vabal_u32:
**      uabal2	v0.2d, v2.4s, v1.4s
**      ret
*/
uint64x2_t
test_vabal_u32 (uint64x2_t sadv, uint32x4_t pv, uint32x4_t sv)
{
  return vabal_u32 (sadv, vget_high_u32 (pv), vget_high_u32 (sv));
}

