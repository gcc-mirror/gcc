/* { dg-do assemble { target { arm*-*-* } } } */
/* { dg-require-effective-target arm_hard_ok } */
/* { dg-require-effective-target arm_v8_2a_i8mm_ok } */
/* { dg-add-options arm_v8_2a_i8mm }  */
/* { dg-additional-options "-O -save-temps -mfloat-abi=hard" } */
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon.h>

/* Unsigned-Signed Dot Product instructions.  */

/*
**usfoo:
**	...
**	vusdot\.s8	d0, d1, d2
**	bx	lr
*/
int32x2_t usfoo (int32x2_t r, uint8x8_t x, int8x8_t y)
{
  return vusdot_s32 (r, x, y);
}

/*
**usfoo_lane:
**	...
**	vusdot\.s8	d0, d1, d2\[0\]
**	bx	lr
*/
int32x2_t usfoo_lane (int32x2_t r, uint8x8_t x, int8x8_t y)
{
  return vusdot_lane_s32 (r, x, y, 0);
}

/*
**usfooq_lane:
**	...
**	vusdot\.s8	q0, q1, d4\[1\]
**	bx	lr
*/
int32x4_t usfooq_lane (int32x4_t r, uint8x16_t x, int8x8_t y)
{
  return vusdotq_lane_s32 (r, x, y, 1);
}

/* Signed-Unsigned Dot Product instructions.  */

/*
**sfoo_lane:
**	...
**	vsudot\.u8	d0, d1, d2\[0\]
**	bx	lr
*/
int32x2_t sfoo_lane (int32x2_t r, int8x8_t x, uint8x8_t y)
{
  return vsudot_lane_s32 (r, x, y, 0);
}

/*
**sfooq_lane:
**	...
**	vsudot\.u8	q0, q1, d4\[1\]
**	bx	lr
*/
int32x4_t sfooq_lane (int32x4_t r, int8x16_t x, uint8x8_t y)
{
  return vsudotq_lane_s32 (r, x, y, 1);
}

/*
**usfoo_untied:
**	...
**	vusdot\.s8	d1, d2, d3
**	vmov	d0, d1  @ v2si
**	bx	lr
*/
int32x2_t usfoo_untied (int32x2_t unused, int32x2_t r, uint8x8_t x, int8x8_t y)
{
  return vusdot_s32 (r, x, y);
}

/*
**usfoo_lane_untied:
**	...
**	vusdot.s8	d1, d2, d3\[0\]
**	vmov	d0, d1  @ v2si
**	bx	lr
*/
int32x2_t usfoo_lane_untied (int32x2_t unused, int32x2_t r, uint8x8_t x, int8x8_t y)
{
  return vusdot_lane_s32 (r, x, y, 0);
}

