/* { dg-do compile } */
/* { dg-additional-options "-O2 -fno-schedule-insns -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" {} } } */
#include <arm_neon.h>

#pragma GCC target ("+sha3")

#define EOR_SCALAR_FN(type)		    \
type eor3_##type (type a, type b, type c) { \
    return a ^ b ^ c;			    \
}


EOR_SCALAR_FN(uint64x1_t)
/*
** eor3_uint64x1_t:
**	eor3	v0.16b, v0.16b, v1.16b, v2.16b
**	ret
*/
EOR_SCALAR_FN(int64x1_t)
/*
** eor3_int64x1_t:
**	eor3	v0.16b, v0.16b, v1.16b, v2.16b
**	ret
*/

#define EOR_VEC_FN(type)		  \
type eor3_##type (type a, type b, type c) \
{					  \
  type res = a;				  \
  res[0] = a[0] ^ b[0] ^ c[0];		  \
  return res;				  \
}

EOR_VEC_FN(int32x4_t)
/*
** eor3_int32x4_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.s\[0\], v\1.s\[0\]
**	ret
*/
EOR_VEC_FN(int32x2_t)
/*
** eor3_int32x2_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.s\[0\], v\1.s\[0\]
**	ret
*/
EOR_VEC_FN(uint32x4_t)
/*
** eor3_uint32x4_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.s\[0\], v\1.s\[0\]
**	ret
*/
EOR_VEC_FN(uint32x2_t)
/*
** eor3_uint32x2_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.s\[0\], v\1.s\[0\]
**	ret
*/
EOR_VEC_FN(int16x8_t)
/*
** eor3_int16x8_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.h\[0\], v\1.h\[0\]
**	ret
*/
EOR_VEC_FN(int16x4_t)
/*
** eor3_int16x4_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.h\[0\], v\1.h\[0\]
**	ret
*/
EOR_VEC_FN(uint16x8_t)
/*
** eor3_uint16x8_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.h\[0\], v\1.h\[0\]
**	ret
*/
EOR_VEC_FN(uint16x4_t)
/*
** eor3_uint16x4_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.h\[0\], v\1.h\[0\]
**	ret
*/
EOR_VEC_FN(int8x16_t)
/*
** eor3_int8x16_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.b\[0\], v\1.b\[0\]
**	ret
*/
EOR_VEC_FN(int8x8_t)
/*
** eor3_int8x8_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.b\[0\], v\1.b\[0\]
**	ret
*/
EOR_VEC_FN(uint8x16_t)
/*
** eor3_uint8x16_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.b\[0\], v\1.b\[0\]
**	ret
*/
EOR_VEC_FN(uint8x8_t)
/*
** eor3_uint8x8_t:
**	eor3	v([0-2]).16b, v[0-2].16b, v[0-2].16b, v[0-2].16b
**	ins	v0.b\[0\], v\1.b\[0\]
**	ret
*/

/* The following tests should not be optimized to EOR3 as that would involve
   unnecessary register file moves.  */

EOR_SCALAR_FN(uint64_t)
/*
** eor3_uint64_t:
**	eor	x1, x1, x2
**	eor	x0, x1, x0
**	ret
*/
EOR_SCALAR_FN(int64_t)
/*
** eor3_int64_t:
**	eor	x1, x1, x2
**	eor	x0, x1, x0
**	ret
*/
EOR_SCALAR_FN(uint32_t)
/*
** eor3_uint32_t:
**	eor	w1, w1, w2
**	eor	w0, w1, w0
**	ret
*/
EOR_SCALAR_FN(int32_t)
/*
** eor3_int32_t:
**	eor	w1, w1, w2
**	eor	w0, w1, w0
**	ret
*/
EOR_SCALAR_FN(uint16_t)
/*
** eor3_uint16_t:
**	eor	w1, w1, w2
**	eor	w0, w0, w1
**	ret
*/
EOR_SCALAR_FN(int16_t)
/*
** eor3_int16_t:
**	eor	w1, w1, w2
**	eor	w0, w0, w1
**	ret
*/
EOR_SCALAR_FN(uint8_t)
/*
** eor3_uint8_t:
**	eor	w1, w1, w2
**	eor	w0, w0, w1
**	ret
*/
EOR_SCALAR_FN(int8_t)
/*
** eor3_int8_t:
**	eor	w1, w1, w2
**	eor	w0, w0, w1
**	ret
*/

void not_eor3_long(long *p)
{
  p[6] = p[4] ^ p[0] ^ (p[2] << 2);
}
/*
** not_eor3_long:
**	ldr	x1, \[x0\]
**	ldr	x2, \[x0, 32\]
**	eor	x2, x2, x1
**	ldr	x1, \[x0, 16\]
**	eor	x1, x2, x1, lsl 2
**	str	x1, \[x0, 48\]
**	ret
*/

int64x2_t not_eor3_int64_t (int64x2_t a, int64_t b, int64_t c)
{
  int64x2_t res;
  res[0] = a[0] ^ b ^ c;
  return res;
}
/*
** not_eor3_int64_t:
**	eor	x0, x0, x1
**	fmov	d31, x0
**	eor	v0.8b, v31.8b, v0.8b
**	fmov	d0, d0
**	ret
*/

