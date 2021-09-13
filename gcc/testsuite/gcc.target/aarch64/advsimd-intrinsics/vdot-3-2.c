/* { dg-do assemble { target { aarch64*-*-* } } } */
/* { dg-require-effective-target stdint_types_mbig_endian } */
/* { dg-require-effective-target arm_v8_2a_i8mm_ok } */
/* { dg-add-options arm_v8_2a_i8mm }  */
/* { dg-additional-options "-mbig-endian -save-temps" } */
/* { dg-final { check-function-bodies "**" "" {-O[^0]} } } */
/* { dg-skip-if "" { *-*-* } { "-fno-fat-lto-objects" } } */

#include <arm_neon.h>

/* Unsigned-Signed Dot Product instructions.  */

/*
**ufoo:
**	usdot	v0\.2s, v1\.8b, v2\.8b
**	ret
*/
int32x2_t ufoo (int32x2_t r, uint8x8_t x, int8x8_t y)
{
  return vusdot_s32 (r, x, y);
}

/*
**ufooq:
**	usdot	v0\.4s, v1\.16b, v2\.16b
**	ret
*/
int32x4_t ufooq (int32x4_t r, uint8x16_t x, int8x16_t y)
{
  return vusdotq_s32 (r, x, y);
}

/*
**ufoo_lane:
**	usdot	v0\.2s, v1\.8b, v2\.4b\[0\]
**	ret
*/
int32x2_t ufoo_lane (int32x2_t r, uint8x8_t x, int8x8_t y)
{
  return vusdot_lane_s32 (r, x, y, 0);
}

/*
**ufoo_laneq:
**	usdot	v0\.2s, v1\.8b, v2\.4b\[2\]
**	ret
*/
int32x2_t ufoo_laneq (int32x2_t r, uint8x8_t x, int8x16_t y)
{
  return vusdot_laneq_s32 (r, x, y, 2);
}

/*
**ufooq_lane:
**	usdot	v0\.4s, v1\.16b, v2\.4b\[1\]
**	ret
*/
int32x4_t ufooq_lane (int32x4_t r, uint8x16_t x, int8x8_t y)
{
  return vusdotq_lane_s32 (r, x, y, 1);
}

/*
**ufooq_laneq:
**	usdot	v0\.4s, v1\.16b, v2\.4b\[3\]
**	ret
*/
int32x4_t ufooq_laneq (int32x4_t r, uint8x16_t x, int8x16_t y)
{
  return vusdotq_laneq_s32 (r, x, y, 3);
}


/* Signed-Unsigned Dot Product instructions.  */

/*
**sfoo_lane:
**	sudot	v0\.2s, v1\.8b, v2\.4b\[0\]
**	ret
*/
int32x2_t sfoo_lane (int32x2_t r, int8x8_t x, uint8x8_t y)
{
  return vsudot_lane_s32 (r, x, y, 0);
}

/*
**sfoo_laneq:
**	sudot	v0\.2s, v1\.8b, v2\.4b\[2\]
**	ret
*/
int32x2_t sfoo_laneq (int32x2_t r, int8x8_t x, uint8x16_t y)
{
  return vsudot_laneq_s32 (r, x, y, 2);
}

/*
**sfooq_lane:
**	sudot	v0\.4s, v1\.16b, v2\.4b\[1\]
**	ret
*/
int32x4_t sfooq_lane (int32x4_t r, int8x16_t x, uint8x8_t y)
{
  return vsudotq_lane_s32 (r, x, y, 1);
}

/*
**sfooq_laneq:
**	sudot	v0\.4s, v1\.16b, v2\.4b\[3\]
**	ret
*/
int32x4_t sfooq_laneq (int32x4_t r, int8x16_t x, uint8x16_t y)
{
  return vsudotq_laneq_s32 (r, x, y, 3);
}

/*
**ufoo_untied:
**	mov	v0\.8b, v1\.8b
**	usdot	v0\.2s, v2\.8b, v3\.8b
**	ret
*/
int32x2_t ufoo_untied (int32x2_t unused, int32x2_t r, uint8x8_t x, int8x8_t y)
{
  return vusdot_s32 (r, x, y);
}

/*
**ufooq_laneq_untied:
**	mov	v0\.16b, v1\.16b
**	usdot	v0\.4s, v2\.16b, v3\.4b\[3\]
**	ret
*/
int32x4_t ufooq_laneq_untied (int32x2_t unused, int32x4_t r, uint8x16_t x, int8x16_t y)
{
  return vusdotq_laneq_s32 (r, x, y, 3);
}


