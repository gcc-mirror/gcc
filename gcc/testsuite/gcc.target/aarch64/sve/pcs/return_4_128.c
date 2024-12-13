/* { dg-do compile } */
/* { dg-options "-O -msve-vector-bits=128 -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target { aarch64_little_endian && lp64 } } } } */

#define CALLEE(SUFFIX, TYPE)			\
  TYPE __attribute__((noipa))			\
  callee_##SUFFIX (TYPE *ptr)			\
  {						\
    return *ptr;				\
  }

/*
** callee_s8:
**	ptrue	(p[0-7])\.b, vl16
**	ld1b	z0\.b, \1/z, \[x0\]
**	ret
*/
CALLEE (s8, __SVInt8_t)

/*
** callee_u8:
**	ptrue	(p[0-7])\.b, vl16
**	ld1b	z0\.b, \1/z, \[x0\]
**	ret
*/
CALLEE (u8, __SVUint8_t)

/*
** callee_mf8:
**	ptrue	(p[0-7])\.b, vl16
**	ld1b	z0\.b, \1/z, \[x0\]
**	ret
*/
CALLEE (mf8, __SVMfloat8_t)

/*
** callee_s16:
**	ptrue	(p[0-7])\.b, vl16
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
CALLEE (s16, __SVInt16_t)

/*
** callee_u16:
**	ptrue	(p[0-7])\.b, vl16
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
CALLEE (u16, __SVUint16_t)

/*
** callee_f16:
**	ptrue	(p[0-7])\.b, vl16
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
CALLEE (f16, __SVFloat16_t)

/*
** callee_bf16:
**	ptrue	(p[0-7])\.b, vl16
**	ld1h	z0\.h, \1/z, \[x0\]
**	ret
*/
CALLEE (bf16, __SVBfloat16_t)

/*
** callee_s32:
**	ptrue	(p[0-7])\.b, vl16
**	ld1w	z0\.s, \1/z, \[x0\]
**	ret
*/
CALLEE (s32, __SVInt32_t)

/*
** callee_u32:
**	ptrue	(p[0-7])\.b, vl16
**	ld1w	z0\.s, \1/z, \[x0\]
**	ret
*/
CALLEE (u32, __SVUint32_t)

/*
** callee_f32:
**	ptrue	(p[0-7])\.b, vl16
**	ld1w	z0\.s, \1/z, \[x0\]
**	ret
*/
CALLEE (f32, __SVFloat32_t)

/*
** callee_s64:
**	ptrue	(p[0-7])\.b, vl16
**	ld1d	z0\.d, \1/z, \[x0\]
**	ret
*/
CALLEE (s64, __SVInt64_t)

/*
** callee_u64:
**	ptrue	(p[0-7])\.b, vl16
**	ld1d	z0\.d, \1/z, \[x0\]
**	ret
*/
CALLEE (u64, __SVUint64_t)

/*
** callee_f64:
**	ptrue	(p[0-7])\.b, vl16
**	ld1d	z0\.d, \1/z, \[x0\]
**	ret
*/
CALLEE (f64, __SVFloat64_t)

#include <arm_sve.h>

#define CALLER(SUFFIX, TYPE)					\
  typeof (svaddv (svptrue_b8 (), *(TYPE *) 0))			\
  __attribute__((noipa))					\
  caller_##SUFFIX (TYPE *ptr1)					\
  {								\
    return svaddv (svptrue_b8 (), callee_##SUFFIX (ptr1));	\
  }

#define CALLER_NON_NUMERIC(SUFFIX, TYPE)				\
  typeof (svlasta (svptrue_b8 (), *(TYPE *) 0))			\
  __attribute__((noipa))					\
  caller_##SUFFIX (TYPE *ptr1)					\
  {								\
    return svlasta (svptrue_b8 (), callee_##SUFFIX (ptr1));	\
  }

/*
** caller_s8:
**	...
**	bl	callee_s8
**	ptrue	(p[0-7])\.b, vl16
**	saddv	(d[0-9]+), \1, z0\.b
**	fmov	x0, \2
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (s8, __SVInt8_t)

/*
** caller_u8:
**	...
**	bl	callee_u8
**	ptrue	(p[0-7])\.b, vl16
**	uaddv	(d[0-9]+), \1, z0\.b
**	fmov	x0, \2
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (u8, __SVUint8_t)

/*
** caller_mf8:
**	...
**	bl	callee_mf8
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER_NON_NUMERIC (mf8, __SVMfloat8_t)

/*
** caller_s16:
**	...
**	bl	callee_s16
**	ptrue	(p[0-7])\.b, vl16
**	saddv	(d[0-9]+), \1, z0\.h
**	fmov	x0, \2
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (s16, __SVInt16_t)

/*
** caller_u16:
**	...
**	bl	callee_u16
**	ptrue	(p[0-7])\.b, vl16
**	uaddv	(d[0-9]+), \1, z0\.h
**	fmov	x0, \2
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (u16, __SVUint16_t)

/*
** caller_f16:
**	...
**	bl	callee_f16
**	ptrue	(p[0-7])\.b, vl16
**	faddv	h0, \1, z0\.h
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (f16, __SVFloat16_t)

/*
** caller_bf16:
**	...
**	bl	callee_bf16
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER_NON_NUMERIC (bf16, __SVBfloat16_t)

/*
** caller_s32:
**	...
**	bl	callee_s32
**	ptrue	(p[0-7])\.b, vl16
**	saddv	(d[0-9]+), \1, z0\.s
**	fmov	x0, \2
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (s32, __SVInt32_t)

/*
** caller_u32:
**	...
**	bl	callee_u32
**	ptrue	(p[0-7])\.b, vl16
**	uaddv	(d[0-9]+), \1, z0\.s
**	fmov	x0, \2
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (u32, __SVUint32_t)

/*
** caller_f32:
**	...
**	bl	callee_f32
**	ptrue	(p[0-7])\.b, vl16
**	faddv	s0, \1, z0\.s
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (f32, __SVFloat32_t)

/*
** caller_s64:
**	...
**	bl	callee_s64
**	ptrue	(p[0-7])\.b, vl16
**	uaddv	(d[0-9]+), \1, z0\.d
**	fmov	x0, \2
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (s64, __SVInt64_t)

/*
** caller_u64:
**	...
**	bl	callee_u64
**	ptrue	(p[0-7])\.b, vl16
**	uaddv	(d[0-9]+), \1, z0\.d
**	fmov	x0, \2
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (u64, __SVUint64_t)

/*
** caller_f64:
**	...
**	bl	callee_f64
**	ptrue	(p[0-7])\.b, vl16
**	faddv	d0, \1, z0\.d
**	ldp	x29, x30, \[sp\], 16
**	ret
*/
CALLER (f64, __SVFloat64_t)
