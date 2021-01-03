/* { dg-do compile } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target lp64 } */

#include <stdint.h>

/*
**set128bits:
**	dup	v0.16b, w1
**	str	q0, \[x0\]
**	ret
*/
void __attribute__((__noinline__))
set128bits (int64_t *src, char c)
{
  __builtin_memset (src, c, 2*sizeof(int64_t));
}

/*
**set128bitszero:
**	stp	xzr, xzr, \[x0\]
**	ret
*/
void __attribute__((__noinline__))
set128bitszero (int64_t *src)
{
  __builtin_memset (src, 0, 2*sizeof(int64_t));
}

/*
** set128bitsplus:
**	dup	v0.16b, w1
**	str	q0, \[x0\]
**	str	q0, \[x0, 12\]
**	ret
*/
void __attribute__((__noinline__))
set128bitsplus (int64_t *src, char c)
{
  __builtin_memset (src, c, 7*sizeof(int32_t));
}

/*
** set256bits:
**	movi	v0.16b, 0x63
**	stp	q0, q0, \[x0\]
**	ret
*/
void __attribute__((__noinline__))
set256bits (int64_t *src)
{
  __builtin_memset (src, 'c', 4*sizeof(int64_t));
}

/*
**set256bitszero:
**	stp	xzr, xzr, \[x0\]
**	stp	xzr, xzr, \[x0, 16\]
**	ret
*/
void __attribute__((__noinline__))
set256bitszero (int64_t *src)
{
  __builtin_memset (src, 0, 4*sizeof(int64_t));
}

/*
** set256bitsplus:
**	movi	v0.16b, 0x63
**	stp	q0, q0, \[x0\]
**	str	q0, \[x0, 32\]
**	str	d0, \[x0, 48\]
**	ret
*/
void __attribute__((__noinline__))
set256bitsplus (int64_t *src)
{
  __builtin_memset (src, 'c', 7*sizeof(int64_t));
}

/* { dg-final { check-function-bodies "**" "" "" } } */
