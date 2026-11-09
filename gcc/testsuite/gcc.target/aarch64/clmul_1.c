/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+aes" } */

typedef unsigned long long u64;
typedef unsigned int u32;
typedef unsigned short u16;

/*
** clmul64:
**	fmov	d([0-9]+), x0
**	fmov	d([0-9]+), x1
**	pmull	v\2\.1q, v\1\.1d, v\2\.1d
**	fmov	x0, d\2
**	ret
*/
u64
clmul64 (u64 a, u64 b)
{
  return __builtin_clmulg (a, b);
}

/*
** clmul32:
**	fmov	s([0-9]+), w0
**	fmov	s([0-9]+), w1
**	pmull	v\1\.1q, v\1\.1d, v\2\.1d
**	fmov	w0, s\1
**	ret
*/
u32
clmul32 (u32 a, u32 b)
{
  return __builtin_clmulg (a, b);
}

/*
** clmul16:
**	and	x0, x0, 65535
**	and	x1, x1, 65535
**	fmov	d([0-9]+), x0
**	fmov	d([0-9]+), x1
**	pmull	v\2\.1q, v\1\.1d, v\2\.1d
**	umov	w0, v\2\.h\[0\]
**	ret
*/
u16
clmul16 (u16 a, u16 b)
{
  return __builtin_clmulg (a, b);
}

/* { dg-final { check-function-bodies "**" "" } } */
