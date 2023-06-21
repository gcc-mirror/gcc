/* { dg-do compile } */
/* { dg-options "-O2 -fno-stack-clash-protection -fno-cprop-registers -fdisable-rtl-combine -g" } */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } } } */

#include <arm_sve.h>
#include <stdarg.h>

/*
** callee_0:
**	...
**	ldr	(p[0-7]), \[x1\]
**	...
**	cntp	x0, \1, \1\.b
**	...
**	ret
*/
uint64_t __attribute__((noipa))
callee_0 (int64_t *ptr, ...)
{
  va_list va;
  svbool_t pg;

  va_start (va, ptr);
  pg = va_arg (va, svbool_t);
  va_end (va);
  return svcntp_b8 (pg, pg);
}

/*
** caller_0:
**	...
**	ptrue	(p[0-9]+)\.d, vl7
**	...
**	str	\1, \[x1\]
**	...
**	ret
*/
uint64_t __attribute__((noipa))
caller_0 (int64_t *ptr)
{
  return callee_0 (ptr, svptrue_pat_b64 (SV_VL7));
}

/*
** callee_1:
**	...
**	ldr	(p[0-7]), \[x2\]
**	...
**	cntp	x0, \1, \1\.b
**	...
**	ret
*/
uint64_t __attribute__((noipa))
callee_1 (int64_t *ptr, ...)
{
  va_list va;
  svbool_t pg;

  va_start (va, ptr);
  va_arg (va, int);
  pg = va_arg (va, svbool_t);
  va_end (va);
  return svcntp_b8 (pg, pg);
}

/*
** caller_1:
**	...
**	ptrue	(p[0-9]+)\.d, vl7
**	...
**	str	\1, \[x2\]
**	...
**	ret
*/
uint64_t __attribute__((noipa))
caller_1 (int64_t *ptr)
{
  return callee_1 (ptr, 1, svptrue_pat_b64 (SV_VL7));
}

/*
** callee_7:
**	...
**	ldr	(p[0-7]), \[x7\]
**	...
**	cntp	x0, \1, \1\.b
**	...
**	ret
*/
uint64_t __attribute__((noipa))
callee_7 (int64_t *ptr, ...)
{
  va_list va;
  svbool_t pg;

  va_start (va, ptr);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  pg = va_arg (va, svbool_t);
  va_end (va);
  return svcntp_b8 (pg, pg);
}

/*
** caller_7:
**	...
**	ptrue	(p[0-9]+)\.d, vl7
**	...
**	str	\1, \[x7\]
**	...
**	ret
*/
uint64_t __attribute__((noipa))
caller_7 (int64_t *ptr)
{
  return callee_7 (ptr, 1, 2, 3, 4, 5, 6, svptrue_pat_b64 (SV_VL7));
}

/* FIXME: We should be able to get rid of the va_list object.  */
/*
** callee_8:
**	sub	sp, sp, #([0-9]+)
**	...
**	ldr	(x[0-9]+), \[sp, \1\]
**	...
**	ldr	(p[0-7]), \[\2\]
**	...
**	cntp	x0, \3, \3\.b
**	...
**	ret
*/
uint64_t __attribute__((noipa))
callee_8 (int64_t *ptr, ...)
{
  va_list va;
  svbool_t pg;

  va_start (va, ptr);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  va_arg (va, int);
  pg = va_arg (va, svbool_t);
  va_end (va);
  return svcntp_b8 (pg, pg);
}

/*
** caller_8:
**	...
**	ptrue	(p[0-9]+)\.d, vl7
**	...
**	str	\1, \[(x[0-9]+)\]
**	...
**	str	\2, \[sp\]
**	...
**	ret
*/
uint64_t __attribute__((noipa))
caller_8 (int64_t *ptr)
{
  return callee_8 (ptr, 1, 2, 3, 4, 5, 6, 7, svptrue_pat_b64 (SV_VL7));
}
