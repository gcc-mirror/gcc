/* { dg-do compile } */
/* { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" { target { be } } } } */

#include <arm_neon.h>

/*
** f_s8:
**	dup	v0.16b, w0
**	movi	(v[0-9]+)\.8b, 0x1
**	ins	v0.b\[0\], \1\.b\[0\]
**	ret
*/

int8x16_t f_s8(int8_t x)
{
  return (int8x16_t) { x, x, x, x, x, x, x, x,
                       x, x, x, x, x, x, x, 1 };
}

/*
** f_s16:
**	dup	v0.8h, w0
**	movi	(v[0-9]+)\.4h, 0x1
**	ins	v0.h\[0\], \1\.h\[0\]
**	ret
*/

int16x8_t f_s16(int16_t x)
{
  return (int16x8_t) { x, x, x, x, x, x, x, 1 };
}

/*
** f_s32:
**	dup	v0.4s, w0
**	movi	(v[0-9]+)\.2s, 0x1
**	ins	v0.s\[0\], \1\.s\[0\]
**	ret
*/

int32x4_t f_s32(int32_t x)
{
  return (int32x4_t) { x, x, x, 1 };
}

/*
** f_s64:
**	adrp	x[0-9]+, .LC[0-9]+
**	ldr	q0, \[x[0-9]+, #:lo12:.LC[0-9]+\]
**	ins	v0\.d\[1\], x0
**	ret
*/

int64x2_t f_s64(int64_t x)
{
  return (int64x2_t) { x, 1 };
}
