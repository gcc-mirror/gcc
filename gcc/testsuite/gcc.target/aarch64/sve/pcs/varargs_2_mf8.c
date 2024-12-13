/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-clash-protection -fno-cprop-registers -fdisable-rtl-combine -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_sve.h>
#include <stdarg.h>

/*
** callee_0:
**	...
**	ld1b	(z[0-9]+\.b), (p[0-7])/z, \[x1\]
**	...
**	st1b	\1, \2, \[x0\]
**	...
**	ret
*/
void __attribute__((noipa))
callee_0 (mfloat8_t *ptr, ...)
{
  va_list va;
  svmfloat8_t vec;

  va_start (va, ptr);
  vec = va_arg (va, svmfloat8_t);
  va_end (va);
  svst1 (svptrue_b8 (), ptr, vec);
}

/* FIXME: optimize the umov and mov pair.  */
/*
** caller_0:
**	...
**	umov	(w[0-9]+), v0.b\[0\]
**	...
**	mov	(z[0-9]+\.b), \1
**	...
**	st1b	\2, p[0-7], \[x1\]
**	...
**	ret
*/
void __attribute__((noipa))
caller_0 (mfloat8_t *ptr, mfloat8_t in)
{
  callee_0 (ptr, svdup_mf8 (in));
}

/*
** callee_1:
**	...
**	ld1b	(z[0-9]+\.b), (p[0-7])/z, \[x2\]
**	...
**	st1b	\1, p[0-7], \[x0\]
**	...
**	ret
*/
void __attribute__((noipa))
callee_1 (mfloat8_t *ptr, ...)
{
  va_list va;
  svmfloat8_t vec;

  va_start (va, ptr);
  va_arg (va, int);
  vec = va_arg (va, svmfloat8_t);
  va_end (va);
  svst1 (svptrue_b8 (), ptr, vec);
}

/* FIXME: optimize the umov and mov pair.  */
/*
** caller_1:
**	...
**	umov	(w[0-9]+), v0.b\[0\]
**	...
**	mov	(z[0-9]+\.b), \1
**	...
**	st1b	\2, p[0-7], \[x2\]
**	...
**	ret
*/
void __attribute__((noipa))
caller_1 (mfloat8_t *ptr, mfloat8_t in)
{
  callee_1 (ptr, 1, svdup_mf8 (in));
}

/*
** callee_7:
**	...
**	ld1b	(z[0-9]+\.b), (p[0-7])/z, \[x7\]
**	...
**	st1b	\1, p[0-7], \[x0\]
**	...
**	ret
*/
void __attribute__((noipa))
callee_7 (mfloat8_t *ptr, ...)
{
  va_list va;
  svmfloat8_t vec;

  va_start (va, ptr);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  vec = va_arg (va, svmfloat8_t);
  va_end (va);
  svst1 (svptrue_b8 (), ptr, vec);
}

/* FIXME: optimize the umov and mov pair.  */
/*
** caller_7:
**	...
**	umov	(w[0-9]+), v0.b\[0\]
**	...
**	mov	(z[0-9]+\.b), \1
**	...
**	st1b	\2, p[0-7], \[x7\]
**	...
**	ret
*/
void __attribute__((noipa))
caller_7 (mfloat8_t *ptr, mfloat8_t in)
{
  callee_7 (ptr, 1, 2, 3, 4, 5, 6, svdup_mf8 (in));
}

/* FIXME: We should be able to get rid of the va_list object.  */
/*
** callee_8:
**	sub	sp, sp, #([0-9]+)
**	...
**	ldr	(x[0-9]+), \[sp, \1\]
**	...
**	ld1b	(z[0-9]+\.b), (p[0-7])/z, \[\2\]
**	...
**	st1b	\3, \4, \[x0\]
**	...
**	ret
*/
void __attribute__((noipa))
callee_8 (mfloat8_t *ptr, ...)
{
  va_list va;
  svmfloat8_t vec;

  va_start (va, ptr);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  vec = va_arg (va, svmfloat8_t);
  va_end (va);
  svst1 (svptrue_b8 (), ptr, vec);
}

/* FIXME: optimize the umov and mov pair.  */
/*
** caller_8:
**	...
**	umov	(w[0-9]+), v0.b\[0\]
**	...
**	mov	(z[0-9]+\.b), \1
**	...
**	st1b	\2, p[0-7], \[(x[0-9]+)\]
**	...
**	str	\3, \[sp\]
**	...
**	ret
*/
void __attribute__((noipa))
caller_8 (mfloat8_t *ptr, mfloat8_t in)
{
  callee_8 (ptr, 1, 2, 3, 4, 5, 6, 7, svdup_mf8 (in));
}
