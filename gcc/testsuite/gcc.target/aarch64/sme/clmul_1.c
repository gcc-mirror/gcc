/* { dg-do compile } */
/* { dg-options "-O2 -march=armv9-a+sme" } */

typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned char u8;

/* PMULL is an Advanced SIMD instruction and is not available in streaming
   mode.  8-bit and 32-bit carry-less multiplications use SVE2 PMULLB.  */

/*
** clmul8:
**	fmov	s([0-9]+), w0
**	fmov	s([0-9]+), w1
**	pmullb	z\1\.h, z\1\.b, z\2\.b
**	umov	w0, v\1\.h\[0\]
**	ret
*/
u8
clmul8 (u8 a, u8 b) [[arm::streaming]]
{
  return __builtin_clmulg (a, b);
}

/*
** clmul8_widen:
**	fmov	s([0-9]+), w1
**	fmov	s([0-9]+), w0
**	pmullb	z\2\.h, z\2\.b, z\1\.b
**	umov	w0, v\2\.h\[0\]
**	lsr	w1, w0, 8
**	strb	w1, \[x2\]
**	ret
*/
u8
clmul8_widen (u8 a, u8 b, u8 *hi) [[arm::streaming]]
{
  return __builtin_clmulg (a, b, hi);
}

/*
** clmul16:
**	and	w0, w0, 65535
**	and	w1, w1, 65535
**	fmov	s([0-9]+), w0
**	fmov	s([0-9]+), w1
**	pmullb	z\1\.d, z\1\.s, z\2\.s
**	fmov	x0, d\1
**	ret
*/
u16
clmul16 (u16 a, u16 b) [[arm::streaming]]
{
  return __builtin_clmulg (a, b);
}

/*
** clmul32_widen:
**	fmov	s([0-9]+), w1
**	fmov	s([0-9]+), w0
**	pmullb	z\2\.d, z\2\.s, z\1\.s
**	fmov	x0, d\2
**	lsr	x1, x0, 32
**	str	w1, \[x2\]
**	ret
*/
u32
clmul32_widen (u32 a, u32 b, u32 *hi) [[arm::streaming]]
{
  return __builtin_clmulg (a, b, hi);
}

/* { dg-final { check-function-bodies "**" "" } } */
