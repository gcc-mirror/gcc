/* { dg-do compile } */
/* { dg-options "-O -msve-vector-bits=128 -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target { aarch64_little_endian && lp64 } } } } */

#include <stdint.h>

typedef int8_t svint8_t __attribute__ ((vector_size (16)));
typedef uint8_t svuint8_t __attribute__ ((vector_size (16)));
typedef __mfp8 svmfloat8_t __attribute__ ((vector_size (16)));

typedef int16_t svint16_t __attribute__ ((vector_size (16)));
typedef uint16_t svuint16_t __attribute__ ((vector_size (16)));
typedef __fp16 svfloat16_t __attribute__ ((vector_size (16)));
typedef __bf16 svbfloat16_t __attribute__ ((vector_size (16)));

typedef int32_t svint32_t __attribute__ ((vector_size (16)));
typedef uint32_t svuint32_t __attribute__ ((vector_size (16)));
typedef float svfloat32_t __attribute__ ((vector_size (16)));

typedef int64_t svint64_t __attribute__ ((vector_size (16)));
typedef uint64_t svuint64_t __attribute__ ((vector_size (16)));
typedef double svfloat64_t __attribute__ ((vector_size (16)));

#define CALLEE(SUFFIX, TYPE)			\
  TYPE __attribute__((noipa))			\
  callee_##SUFFIX (TYPE *ptr)			\
  {						\
    return *ptr;				\
  }

/*
** callee_s8:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (s8, svint8_t)

/*
** callee_u8:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (u8, svuint8_t)

/*
** callee_mf8:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (mf8, svmfloat8_t)

/*
** callee_s16:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (s16, svint16_t)

/*
** callee_u16:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (u16, svuint16_t)

/*
** callee_f16:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (f16, svfloat16_t)

/*
** callee_bf16:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (bf16, svbfloat16_t)

/*
** callee_s32:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (s32, svint32_t)

/*
** callee_u32:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (u32, svuint32_t)

/*
** callee_f32:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (f32, svfloat32_t)

/*
** callee_s64:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (s64, svint64_t)

/*
** callee_u64:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (u64, svuint64_t)

/*
** callee_f64:
**	ldr	q0, \[x0\]
**	ret
*/
CALLEE (f64, svfloat64_t)

#define CALLER(SUFFIX, TYPE)			\
  void __attribute__((noipa))			\
  caller_##SUFFIX (TYPE *ptr1, TYPE *ptr2)	\
  {						\
    *ptr2 = callee_##SUFFIX (ptr1);		\
  }

/*
** caller_s8:
**	...
**	bl	callee_s8
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (s8, svint8_t)

/*
** caller_u8:
**	...
**	bl	callee_u8
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (u8, svuint8_t)

/*
** caller_mf8:
**	...
**	bl	callee_mf8
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (mf8, svmfloat8_t)

/*
** caller_s16:
**	...
**	bl	callee_s16
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (s16, svint16_t)

/*
** caller_u16:
**	...
**	bl	callee_u16
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (u16, svuint16_t)

/*
** caller_f16:
**	...
**	bl	callee_f16
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (f16, svfloat16_t)

/*
** caller_bf16:
**	...
**	bl	callee_bf16
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (bf16, svbfloat16_t)

/*
** caller_s32:
**	...
**	bl	callee_s32
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (s32, svint32_t)

/*
** caller_u32:
**	...
**	bl	callee_u32
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (u32, svuint32_t)

/*
** caller_f32:
**	...
**	bl	callee_f32
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (f32, svfloat32_t)

/*
** caller_s64:
**	...
**	bl	callee_s64
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (s64, svint64_t)

/*
** caller_u64:
**	...
**	bl	callee_u64
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (u64, svuint64_t)

/*
** caller_f64:
**	...
**	bl	callee_f64
**	...
**	str	q0, \[[^]]*\]
**	...
**	ret
*/
CALLER (f64, svfloat64_t)
