/* { dg-do compile } */
/* { dg-options "-O -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <stdint.h>

typedef int8_t svint8_t __attribute__ ((vector_size (32)));
typedef uint8_t svuint8_t __attribute__ ((vector_size (32)));
typedef __mfp8 svmfloat8_t __attribute__ ((vector_size (32)));

typedef int16_t svint16_t __attribute__ ((vector_size (32)));
typedef uint16_t svuint16_t __attribute__ ((vector_size (32)));
typedef __fp16 svfloat16_t __attribute__ ((vector_size (32)));
typedef __bf16 svbfloat16_t __attribute__ ((vector_size (32)));

typedef int32_t svint32_t __attribute__ ((vector_size (32)));
typedef uint32_t svuint32_t __attribute__ ((vector_size (32)));
typedef float svfloat32_t __attribute__ ((vector_size (32)));

typedef int64_t svint64_t __attribute__ ((vector_size (32)));
typedef uint64_t svuint64_t __attribute__ ((vector_size (32)));
typedef double svfloat64_t __attribute__ ((vector_size (32)));

#define CALLEE(SUFFIX, TYPE)			\
  TYPE __attribute__((noipa))			\
  callee_##SUFFIX (TYPE *ptr)			\
  {						\
    return *ptr;				\
  }

/*
** callee_s8:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (s8, svint8_t)

/*
** callee_u8:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (u8, svuint8_t)

/*
** callee_u8:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (mf8, svmfloat8_t)

/*
** callee_s16:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (s16, svint16_t)

/*
** callee_u16:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (u16, svuint16_t)

/* Currently we scalarize this.  */
CALLEE (f16, svfloat16_t)

/* Currently we scalarize this.  */
CALLEE (bf16, svbfloat16_t)

/*
** callee_s32:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (s32, svint32_t)

/*
** callee_u32:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (u32, svuint32_t)

/* Currently we scalarize this.  */
CALLEE (f32, svfloat32_t)

/*
** callee_s64:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (s64, svint64_t)

/*
** callee_u64:
** (
**	ld1	({v.*}), \[x0\]
**	st1	\1, \[x8\]
** |
**	ldp	(q[0-9]+, q[0-9]+), \[x0\]
**	stp	\2, \[x8\]
** )
**	ret
*/
CALLEE (u64, svuint64_t)

/* Currently we scalarize this.  */
CALLEE (f64, svfloat64_t)

#define CALLER(SUFFIX, TYPE)		\
  typeof ((*(TYPE *) 0)[0])		\
  __attribute__((noipa))		\
  caller_##SUFFIX (TYPE *ptr1)		\
  {					\
    return callee_##SUFFIX (ptr1)[0];	\
  }

/*
** caller_s8:
**	...
**	bl	callee_s8
**	ldrb	w0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (s8, svint8_t)

/*
** caller_u8:
**	...
**	bl	callee_u8
**	ldrb	w0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (u8, svuint8_t)

/*
** caller_mf8:
**	...
**	bl	callee_mf8
**	ldr	b0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (mf8, svmfloat8_t)

/*
** caller_s16:
**	...
**	bl	callee_s16
**	ldrh	w0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (s16, svint16_t)

/*
** caller_u16:
**	...
**	bl	callee_u16
**	ldrh	w0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (u16, svuint16_t)

/*
** caller_f16:
**	...
**	bl	callee_f16
**	ldr	h0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (f16, svfloat16_t)

/*
** caller_bf16:
**	...
**	bl	callee_bf16
**	ldr	h0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (bf16, svbfloat16_t)

/*
** caller_s32:
**	...
**	bl	callee_s32
**	ldr	w0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (s32, svint32_t)

/*
** caller_u32:
**	...
**	bl	callee_u32
**	ldr	w0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (u32, svuint32_t)

/*
** caller_f32:
**	...
**	bl	callee_f32
**	ldr	s0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (f32, svfloat32_t)

/*
** caller_s64:
**	...
**	bl	callee_s64
**	ldr	x0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (s64, svint64_t)

/*
** caller_u64:
**	...
**	bl	callee_u64
**	ldr	x0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (u64, svuint64_t)

/*
** caller_f64:
**	...
**	bl	callee_f64
**	ldr	d0, \[sp, 16\]
**	ldp	x29, x30, \[sp\], 48
**	ret
*/
CALLER (f64, svfloat64_t)
