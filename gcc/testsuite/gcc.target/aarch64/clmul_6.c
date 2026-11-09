/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a+aes -mbig-endian" } */

typedef unsigned long long u64;

/* The high half of the product is the second doubleword of the PMULL
   result regardless of endianness.  */

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

/* { dg-final { check-function-bodies "**" "" } } */
