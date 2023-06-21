/* { dg-do compile } */
/* { dg-additional-options "-O" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
#include <arm_neon.h>

/*
** foo_1:
**	uaddlv	s([0-9]+), v0.8h
**	fmov	x0, d\1
**	ret
*/

uint64_t
foo_1 (uint16x8_t b)
{
  return vaddlvq_u32 (vpadalq_u16 (vdupq_n_u32 (0), b));
}

/*
** foo_2:
**	uaddlv	s([0-9]+), v0.8h
**	fmov	w0, s\1
**	ret
*/

uint32_t
foo_2 (uint16x8_t b)
{
  return vaddvq_u32 (vpadalq_u16 (vdupq_n_u32 (0), b));
}

/*
** foo_3:
**	saddlv	s([0-9]+), v0.8h
**	fmov	w0, s\1
**	ret
*/

int32_t
foo_3 (int16x8_t b)
{
  return vaddvq_s32 (vpadalq_s16 (vdupq_n_s32 (0), b));
}
