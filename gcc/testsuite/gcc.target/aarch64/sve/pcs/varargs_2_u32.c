/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-clash-protection -fno-cprop-registers -fdisable-rtl-combine -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_sve.h>
#include <stdarg.h>

/*
** callee_0: {target aarch64_big_endian}
**	...
**	ld1w	(z[0-9]+\.s), (p[0-7])/z, \[x1\]
**	...
**	st1w	\1, \2, \[x0\]
**	...
**	ret
*/
/*
** callee_0: {target aarch64_little_endian}
**	...
**	ldr	(z[0-9]+), \[x1\]
**	...
**	str	\1, \[x0\]
**	...
**	ret
*/
void __attribute__((noipa))
callee_0 (int32_t *ptr, ...)
{
  va_list va;
  svint32_t vec;

  va_start (va, ptr);
  vec = va_arg (va, svint32_t);
  va_end (va);
  svst1 (svptrue_b8 (), ptr, vec);
}

/*
** caller_0: {target aarch64_big_endian}
**	...
**	mov	(z[0-9]+\.s), #42
**	...
**	st1w	\1, p[0-7], \[x1\]
**	...
**	ret
*/
/*
** caller_0: {target aarch64_little_endian}
**	...
**	mov	(z[0-9]+)\.s, #42
**	...
**	str	\1, \[x1\]
**	...
**	ret
*/
void __attribute__((noipa))
caller_0 (int32_t *ptr)
{
  callee_0 (ptr, svdup_u32 (42));
}

/*
** callee_1: {target aarch64_big_endian}
**	...
**	ld1w	(z[0-9]+\.s), (p[0-7])/z, \[x2\]
**	...
**	st1w	\1, p[0-7], \[x0\]
**	...
**	ret
*/
/*
** callee_1: {target aarch64_little_endian}
**	...
**	ldr	(z[0-9]+), \[x2\]
**	...
**	str	\1, \[x0\]
**	...
**	ret
*/
void __attribute__((noipa))
callee_1 (int32_t *ptr, ...)
{
  va_list va;
  svint32_t vec;

  va_start (va, ptr);
  va_arg (va, int);
  vec = va_arg (va, svint32_t);
  va_end (va);
  svst1 (svptrue_b8 (), ptr, vec);
}

/*
** caller_1: {target aarch64_big_endian}
**	...
**	mov	(z[0-9]+\.s), #42
**	...
**	st1w	\1, p[0-7], \[x2\]
**	...
**	ret
*/
/*
** caller_1: {target aarch64_little_endian}
**	...
**	mov	(z[0-9]+)\.s, #42
**	...
**	str	\1, \[x2\]
**	...
**	ret
*/
void __attribute__((noipa))
caller_1 (int32_t *ptr)
{
  callee_1 (ptr, 1, svdup_u32 (42));
}

/*
** callee_7: {target aarch64_big_endian}
**	...
**	ld1w	(z[0-9]+\.s), (p[0-7])/z, \[x7\]
**	...
**	st1w	\1, p[0-7], \[x0\]
**	...
**	ret
*/
/*
** callee_7: {target aarch64_little_endian}
**	...
**	ldr	(z[0-9]+), \[x7\]
**	...
**	str	\1, \[x0\]
**	...
**	ret
*/
void __attribute__((noipa))
callee_7 (int32_t *ptr, ...)
{
  va_list va;
  svint32_t vec;

  va_start (va, ptr);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  vec = va_arg (va, svint32_t);
  va_end (va);
  svst1 (svptrue_b8 (), ptr, vec);
}

/*
** caller_7: {target aarch64_big_endian}
**	...
**	mov	(z[0-9]+\.s), #42
**	...
**	st1w	\1, p[0-7], \[x7\]
**	...
**	ret
*/
/*
** caller_7: {target aarch64_little_endian}
**	...
**	mov	(z[0-9]+)\.s, #42
**	...
**	str	\1, \[x7\]
**	...
**	ret
*/
void __attribute__((noipa))
caller_7 (int32_t *ptr)
{
  callee_7 (ptr, 1, 2, 3, 4, 5, 6, svdup_u32 (42));
}

/* FIXME: We should be able to get rid of the va_list object.  */
/*
** callee_8: {target aarch64_big_endian}
**	sub	sp, sp, #([0-9]+)
**	...
**	ldr	(x[0-9]+), \[sp, \1\]
**	...
**	ld1w	(z[0-9]+\.s), (p[0-7])/z, \[\2\]
**	...
**	st1w	\3, \4, \[x0\]
**	...
**	ret
*/
/*
** callee_8: {target aarch64_little_endian}
**	sub	sp, sp, #([0-9]+)
**	...
**	ldr	(x[0-9]+), \[sp, \1\]
**	...
**	ldr	(z[0-9]+), \[\2\]
**	...
**	str	\3, \[x0\]
**	...
**	ret
*/
void __attribute__((noipa))
callee_8 (int32_t *ptr, ...)
{
  va_list va;
  svint32_t vec;

  va_start (va, ptr);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  vec = va_arg (va, svint32_t);
  va_end (va);
  svst1 (svptrue_b8 (), ptr, vec);
}

/*
** caller_8: {target aarch64_big_endian}
**	...
**	mov	(z[0-9]+\.s), #42
**	...
**	st1w	\1, p[0-7], \[(x[0-9]+)\]
**	...
**	str	\2, \[sp\]
**	...
**	ret
*/
/*
** caller_8: {target aarch64_little_endian}
**	...
**	mov	(z[0-9]+)\.s, #42
**	...
**	str	\1, \[(x[0-9]+)\]
**	...
**	str	\2, \[sp\]
**	...
**	ret
*/
void __attribute__((noipa))
caller_8 (int32_t *ptr)
{
  callee_8 (ptr, 1, 2, 3, 4, 5, 6, 7, svdup_u32 (42));
}
