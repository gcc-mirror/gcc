/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+aes" } */

typedef unsigned long long u64;
typedef unsigned int u32;

/* Both halves of a 64x64->128 product.  */

/*
** clmul64_widen:
**	fmov	d([0-9]+), x0
**	fmov	d([0-9]+), x1
**	pmull	v\2\.1q, v\1\.1d, v\2\.1d
**	fmov	x0, d\2
**	st1	\{v\2\.d\}\[1\], \[x2\]
**	ret
*/
u64
clmul64_widen (u64 a, u64 b, u64 *hi)
{
  return __builtin_clmulg (a, b, hi);
}

/* Only the high half is used.  Must not go through the stack.  */

/*
** clmul64_high:
**	fmov	d([0-9]+), x0
**	fmov	d([0-9]+), x1
**	pmull	v\2\.1q, v\1\.1d, v\2\.1d
**	umov	x0, v\2\.d\[1\]
**	ret
*/
u64
clmul64_high (u64 a, u64 b)
{
  u64 h;
  __builtin_clmulg (a, b, &h);
  return h;
}

/*
** clmul32_widen:
**	fmov	s([0-9]+), w0
**	fmov	s([0-9]+), w1
**	pmull	v\1\.1q, v\1\.1d, v\2\.1d
**	fmov	x0, d\1
**	lsr	x0, x0, 32
**	str	w0, \[x2\]
**	fmov	w0, s\1
**	ret
*/
u32
clmul32_widen (u32 a, u32 b, u32 *hi)
{
  return __builtin_clmulg (a, b, hi);
}

/* { dg-final { check-function-bodies "**" "" } } */
