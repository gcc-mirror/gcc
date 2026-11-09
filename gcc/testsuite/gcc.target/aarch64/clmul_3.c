/* { dg-do compile } */
/* { dg-options "-O2 -march=armv8-a" } */

typedef unsigned char u8;

/* 8x8->16 carry-less multiplication needs only base AdvSIMD.  */

/*
** clmul8:
**	dup	v([0-9]+)\.8b, w0
**	dup	v([0-9]+)\.8b, w1
**	pmull	v\2\.8h, v\1\.8b, v\2\.8b
**	umov	w0, v\2\.b\[0\]
**	ret
*/
u8
clmul8 (u8 a, u8 b)
{
  return __builtin_clmulg (a, b);
}

/*
** clmul8_widen:
**	dup	v([0-9]+)\.8b, w0
**	dup	v([0-9]+)\.8b, w1
**	pmull	v\2\.8h, v\1\.8b, v\2\.8b
**	ushr	v\1\.2s, v\2\.2s, 8
**	umov	w0, v\2\.b\[0\]
**	str	b\1, \[x2\]
**	ret
*/
u8
clmul8_widen (u8 a, u8 b, u8 *hi)
{
  return __builtin_clmulg (a, b, hi);
}

/* { dg-final { check-function-bodies "**" "" } } */
