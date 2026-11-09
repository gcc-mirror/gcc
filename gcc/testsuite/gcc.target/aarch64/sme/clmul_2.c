/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+sme+ssve-aes" } */

typedef unsigned long long u64;

/* With +ssve-aes the 64x64->128 product uses SVE2 PMULLB.  */

/*
** clmul64:
**	fmov	d([0-9]+), x0
**	fmov	d([0-9]+), x1
**	pmullb	z\2\.q, z\1\.d, z\2\.d
**	fmov	x0, d\2
**	ret
*/
u64
clmul64 (u64 a, u64 b) [[arm::streaming]]
{
  return __builtin_clmulg (a, b);
}

/*
** clmul64_widen:
**	fmov	d([0-9]+), x0
**	fmov	d([0-9]+), x1
**	pmullb	z\2\.q, z\1\.d, z\2\.d
**	uzp2	z([0-9]+)\.d, z\2\.d, z\2\.d
**	fmov	x0, d\2
**	str	d\3, \[x2\]
**	ret
*/
u64
clmul64_widen (u64 a, u64 b, u64 *hi) [[arm::streaming]]
{
  return __builtin_clmulg (a, b, hi);
}

/*
** clmul64_high:
**	fmov	d([0-9]+), x0
**	fmov	d([0-9]+), x1
**	pmullb	z\2\.q, z\1\.d, z\2\.d
**	uzp2	z\2\.d, z\2\.d, z\2\.d
**	fmov	x0, d\2
**	ret
*/
u64
clmul64_high (u64 a, u64 b) [[arm::streaming]]
{
  u64 h;
  __builtin_clmulg (a, b, &h);
  return h;
}

/* { dg-final { check-function-bodies "**" "" } } */
