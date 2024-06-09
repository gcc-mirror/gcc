// { dg-options "-O -fomit-frame-pointer" }
// { dg-final { check-function-bodies "**" "" } }

#include <arm_neon.h>
#include <arm_sve.h>

/*
** test_d0:
**	...
**	fmov	x10, d0
**	smstart	sm
**	fmov	d0, x10
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_d0 (double d0)
{
  asm ("");
}

/*
** test_d7:
**	...
**	fmov	x10, d0
**	fmov	x11, d1
**	fmov	x12, d2
**	fmov	x13, d3
**	fmov	x14, d4
**	fmov	x15, d5
**	fmov	x16, d6
**	fmov	x17, d7
**	smstart	sm
**	fmov	d0, x10
**	fmov	d1, x11
**	fmov	d2, x12
**	fmov	d3, x13
**	fmov	d4, x14
**	fmov	d5, x15
**	fmov	d6, x16
**	fmov	d7, x17
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_d7 (double d0, double d1, double d2, double d3,
	 double d4, double d5, double d6, double d7)
{
  asm ("");
}

/*
** test_d0_vec:
**	...
** (
**	fmov	x10, d0
** |
**	umov	x10, v0.d\[0\]
** )
**	smstart	sm
**	fmov	d0, x10
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_d0_vec (int8x8_t d0)
{
  asm ("");
}

/*
** test_d7_vec:
**	...
** (
**	fmov	x10, d0
**	fmov	x11, d1
**	fmov	x12, d2
**	fmov	x13, d3
**	fmov	x14, d4
**	fmov	x15, d5
**	fmov	x16, d6
**	fmov	x17, d7
** |
**	umov	x10, v0.d\[0\]
**	umov	x11, v1.d\[0\]
**	umov	x12, v2.d\[0\]
**	umov	x13, v3.d\[0\]
**	umov	x14, v4.d\[0\]
**	umov	x15, v5.d\[0\]
**	umov	x16, v6.d\[0\]
**	umov	x17, v7.d\[0\]
** )
**	smstart	sm
**	fmov	d0, x10
**	fmov	d1, x11
**	fmov	d2, x12
**	fmov	d3, x13
**	fmov	d4, x14
**	fmov	d5, x15
**	fmov	d6, x16
**	fmov	d7, x17
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_d7_vec (int8x8_t d0, int8x8_t d1, int8x8_t d2, int8x8_t d3,
	     int8x8_t d4, int8x8_t d5, int8x8_t d6, int8x8_t d7)
{
  asm ("");
}

/*
** test_q0:
**	...
**	str	q0, \[sp, #?-16\]!
**	smstart	sm
**	ldr	q0, \[sp\], #?16
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_q0 (int8x16_t q0)
{
  asm ("");
}

/*
** test_q7:
**	...
**	stp	q0, q1, \[sp, #?-128\]!
**	stp	q2, q3, \[sp, #?32\]
**	stp	q4, q5, \[sp, #?64\]
**	stp	q6, q7, \[sp, #?96\]
**	smstart	sm
**	ldp	q2, q3, \[sp, #?32\]
**	ldp	q4, q5, \[sp, #?64\]
**	ldp	q6, q7, \[sp, #?96\]
**	ldp	q0, q1, \[sp\], #?128
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_q7 (int8x16x4_t q0, int8x16x4_t q4)
{
  asm ("");
}

/*
** test_z0:
**	...
**	addvl	sp, sp, #-1
**	str	z0, \[sp\]
**	smstart	sm
**	ldr	z0, \[sp\]
**	addvl	sp, sp, #1
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_z0 (svint8_t z0)
{
  asm ("");
}

/*
** test_z7:
**	...
**	addvl	sp, sp, #-8
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
**	str	z2, \[sp, #2, mul vl\]
**	str	z3, \[sp, #3, mul vl\]
**	str	z4, \[sp, #4, mul vl\]
**	str	z5, \[sp, #5, mul vl\]
**	str	z6, \[sp, #6, mul vl\]
**	str	z7, \[sp, #7, mul vl\]
**	smstart	sm
**	ldr	z0, \[sp\]
**	ldr	z1, \[sp, #1, mul vl\]
**	ldr	z2, \[sp, #2, mul vl\]
**	ldr	z3, \[sp, #3, mul vl\]
**	ldr	z4, \[sp, #4, mul vl\]
**	ldr	z5, \[sp, #5, mul vl\]
**	ldr	z6, \[sp, #6, mul vl\]
**	ldr	z7, \[sp, #7, mul vl\]
**	addvl	sp, sp, #8
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_z7 (svint8x4_t z0, svint8x4_t z4)
{
  asm ("");
}

/*
** test_p0:
**	...
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_p0 (svbool_t p0)
{
  asm ("");
}

/*
** test_p3:
**	...
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	str	p1, \[sp, #1, mul vl\]
**	str	p2, \[sp, #2, mul vl\]
**	str	p3, \[sp, #3, mul vl\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	ldr	p1, \[sp, #1, mul vl\]
**	ldr	p2, \[sp, #2, mul vl\]
**	ldr	p3, \[sp, #3, mul vl\]
**	addvl	sp, sp, #1
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_p3 (svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3)
{
  asm ("");
}

/*
** test_mixed:
**	...
**	addvl	sp, sp, #-3
**	str	p0, \[sp\]
**	str	p1, \[sp, #1, mul vl\]
**	str	p2, \[sp, #2, mul vl\]
**	str	p3, \[sp, #3, mul vl\]
**	str	z3, \[sp, #1, mul vl\]
**	str	z7, \[sp, #2, mul vl\]
**	stp	q2, q6, \[sp, #?-32\]!
**	fmov	w10, s0
**	fmov	x11, d1
**	fmov	w12, s4
**	fmov	x13, d5
**	smstart	sm
**	fmov	s0, w10
**	fmov	d1, x11
**	fmov	s4, w12
**	fmov	d5, x13
**	ldp	q2, q6, \[sp\], #?32
**	ldr	p0, \[sp\]
**	ldr	p1, \[sp, #1, mul vl\]
**	ldr	p2, \[sp, #2, mul vl\]
**	ldr	p3, \[sp, #3, mul vl\]
**	ldr	z3, \[sp, #1, mul vl\]
**	ldr	z7, \[sp, #2, mul vl\]
**	addvl	sp, sp, #3
**	smstop	sm
**	...
*/
[[arm::locally_streaming]] void
test_mixed (float s0, double d1, float32x4_t q2, svfloat32_t z3,
	    float s4, double d5, float64x2_t q6, svfloat64_t z7,
	    svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3)
{
  asm ("");
}
