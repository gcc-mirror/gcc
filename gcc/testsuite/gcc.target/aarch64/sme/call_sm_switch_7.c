// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls" }
// { dg-final { check-function-bodies "**" "" } }

#include <arm_neon.h>
#include <arm_sve.h>

double produce_d0 ();
void consume_d0 (double);

/*
** test_d0:
**	...
**	smstop	sm
**	bl	produce_d0
**	fmov	x10, d0
**	smstart	sm
**	fmov	d0, x10
**	fmov	x10, d0
**	smstop	sm
**	fmov	d0, x10
**	bl	consume_d0
**	...
*/
void
test_d0 () [[arm::streaming]]
{
  double res = produce_d0 ();
  asm volatile ("");
  consume_d0 (res);
}

int8x8_t produce_d0_vec ();
void consume_d0_vec (int8x8_t);

/*
** test_d0_vec:
**	...
**	smstop	sm
**	bl	produce_d0_vec
** (
**	fmov	x10, d0
** |
**	umov	x10, v0.d\[0\]
** )
**	smstart	sm
**	fmov	d0, x10
** (
**	fmov	x10, d0
** |
**	umov	x10, v0.d\[0\]
** )
**	smstop	sm
**	fmov	d0, x10
**	bl	consume_d0_vec
**	...
*/
void
test_d0_vec () [[arm::streaming]]
{
  int8x8_t res = produce_d0_vec ();
  asm volatile ("");
  consume_d0_vec (res);
}

int8x16_t produce_q0 ();
void consume_q0 (int8x16_t);

/*
** test_q0:
**	...
**	smstop	sm
**	bl	produce_q0
**	str	q0, \[sp, #?-16\]!
**	smstart	sm
**	ldr	q0, \[sp\], #?16
**	str	q0, \[sp, #?-16\]!
**	smstop	sm
**	ldr	q0, \[sp\], #?16
**	bl	consume_q0
**	...
*/
void
test_q0 () [[arm::streaming]]
{
  int8x16_t res = produce_q0 ();
  asm volatile ("");
  consume_q0 (res);
}

int8x16x2_t produce_q1 ();
void consume_q1 (int8x16x2_t);

/*
** test_q1:
**	...
**	smstop	sm
**	bl	produce_q1
**	stp	q0, q1, \[sp, #?-32\]!
**	smstart	sm
**	ldp	q0, q1, \[sp\], #?32
**	stp	q0, q1, \[sp, #?-32\]!
**	smstop	sm
**	ldp	q0, q1, \[sp\], #?32
**	bl	consume_q1
**	...
*/
void
test_q1 () [[arm::streaming]]
{
  int8x16x2_t res = produce_q1 ();
  asm volatile ("");
  consume_q1 (res);
}

int8x16x3_t produce_q2 ();
void consume_q2 (int8x16x3_t);

/*
** test_q2:
**	...
**	smstop	sm
**	bl	produce_q2
**	stp	q0, q1, \[sp, #?-48\]!
**	str	q2, \[sp, #?32\]
**	smstart	sm
**	ldr	q2, \[sp, #?32\]
**	ldp	q0, q1, \[sp\], #?48
**	stp	q0, q1, \[sp, #?-48\]!
**	str	q2, \[sp, #?32\]
**	smstop	sm
**	ldr	q2, \[sp, #?32\]
**	ldp	q0, q1, \[sp\], #?48
**	bl	consume_q2
**	...
*/
void
test_q2 () [[arm::streaming]]
{
  int8x16x3_t res = produce_q2 ();
  asm volatile ("");
  consume_q2 (res);
}

int8x16x4_t produce_q3 ();
void consume_q3 (int8x16x4_t);

/*
** test_q3:
**	...
**	smstop	sm
**	bl	produce_q3
**	stp	q0, q1, \[sp, #?-64\]!
**	stp	q2, q3, \[sp, #?32\]
**	smstart	sm
**	ldp	q2, q3, \[sp, #?32\]
**	ldp	q0, q1, \[sp\], #?64
**	stp	q0, q1, \[sp, #?-64\]!
**	stp	q2, q3, \[sp, #?32\]
**	smstop	sm
**	ldp	q2, q3, \[sp, #?32\]
**	ldp	q0, q1, \[sp\], #?64
**	bl	consume_q3
**	...
*/
void
test_q3 () [[arm::streaming]]
{
  int8x16x4_t res = produce_q3 ();
  asm volatile ("");
  consume_q3 (res);
}

svint8_t produce_z0 ();
void consume_z0 (svint8_t);

/*
** test_z0:
**	...
**	smstop	sm
**	bl	produce_z0
**	addvl	sp, sp, #-1
**	str	z0, \[sp\]
**	smstart	sm
**	ldr	z0, \[sp\]
**	addvl	sp, sp, #1
**	addvl	sp, sp, #-1
**	str	z0, \[sp\]
**	smstop	sm
**	ldr	z0, \[sp\]
**	addvl	sp, sp, #1
**	bl	consume_z0
**	...
*/
void
test_z0 () [[arm::streaming]]
{
  svint8_t res = produce_z0 ();
  asm volatile ("");
  consume_z0 (res);
}

svint8x4_t produce_z3 ();
void consume_z3 (svint8x4_t);

/*
** test_z3:
**	...
**	smstop	sm
**	bl	produce_z3
**	addvl	sp, sp, #-4
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
**	str	z2, \[sp, #2, mul vl\]
**	str	z3, \[sp, #3, mul vl\]
**	smstart	sm
**	ldr	z0, \[sp\]
**	ldr	z1, \[sp, #1, mul vl\]
**	ldr	z2, \[sp, #2, mul vl\]
**	ldr	z3, \[sp, #3, mul vl\]
**	addvl	sp, sp, #4
**	addvl	sp, sp, #-4
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
**	str	z2, \[sp, #2, mul vl\]
**	str	z3, \[sp, #3, mul vl\]
**	smstop	sm
**	ldr	z0, \[sp\]
**	ldr	z1, \[sp, #1, mul vl\]
**	ldr	z2, \[sp, #2, mul vl\]
**	ldr	z3, \[sp, #3, mul vl\]
**	addvl	sp, sp, #4
**	bl	consume_z3
**	...
*/
void
test_z3 () [[arm::streaming]]
{
  svint8x4_t res = produce_z3 ();
  asm volatile ("");
  consume_z3 (res);
}

svbool_t produce_p0 ();
void consume_p0 (svbool_t);

/*
** test_p0:
**	...
**	smstop	sm
**	bl	produce_p0
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstop	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	bl	consume_p0
**	...
*/
void
test_p0 () [[arm::streaming]]
{
  svbool_t res = produce_p0 ();
  asm volatile ("");
  consume_p0 (res);
}

void consume_d7 (double, double, double, double, double, double, double,
		 double);

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
**	smstop	sm
**	fmov	d0, x10
**	fmov	d1, x11
**	fmov	d2, x12
**	fmov	d3, x13
**	fmov	d4, x14
**	fmov	d5, x15
**	fmov	d6, x16
**	fmov	d7, x17
**	bl	consume_d7
**	...
*/
void
test_d7 () [[arm::streaming]]
{
  consume_d7 (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
}

void consume_d7_vec (int8x8_t, int8x8_t, int8x8_t, int8x8_t, int8x8_t,
		     int8x8_t, int8x8_t, int8x8_t);

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
**	smstop	sm
**	fmov	d0, x10
**	fmov	d1, x11
**	fmov	d2, x12
**	fmov	d3, x13
**	fmov	d4, x14
**	fmov	d5, x15
**	fmov	d6, x16
**	fmov	d7, x17
**	bl	consume_d7_vec
**	...
*/
void
test_d7_vec (int8x8_t *ptr) [[arm::streaming]]
{
  consume_d7_vec (*ptr, *ptr, *ptr, *ptr, *ptr, *ptr, *ptr, *ptr);
}

void consume_q7 (int8x16_t, int8x16_t, int8x16_t, int8x16_t, int8x16_t,
		 int8x16_t, int8x16_t, int8x16_t);

/*
** test_q7:
**	...
**	stp	q0, q1, \[sp, #?-128\]!
**	stp	q2, q3, \[sp, #?32\]
**	stp	q4, q5, \[sp, #?64\]
**	stp	q6, q7, \[sp, #?96\]
**	smstop	sm
**	ldp	q2, q3, \[sp, #?32\]
**	ldp	q4, q5, \[sp, #?64\]
**	ldp	q6, q7, \[sp, #?96\]
**	ldp	q0, q1, \[sp\], #?128
**	bl	consume_q7
**	...
*/
void
test_q7 (int8x16_t *ptr) [[arm::streaming]]
{
  consume_q7 (*ptr, *ptr, *ptr, *ptr, *ptr, *ptr, *ptr, *ptr);
}

void consume_z7 (svint8_t, svint8_t, svint8_t, svint8_t, svint8_t,
		 svint8_t, svint8_t, svint8_t);

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
**	smstop	sm
**	ldr	z0, \[sp\]
**	ldr	z1, \[sp, #1, mul vl\]
**	ldr	z2, \[sp, #2, mul vl\]
**	ldr	z3, \[sp, #3, mul vl\]
**	ldr	z4, \[sp, #4, mul vl\]
**	ldr	z5, \[sp, #5, mul vl\]
**	ldr	z6, \[sp, #6, mul vl\]
**	ldr	z7, \[sp, #7, mul vl\]
**	addvl	sp, sp, #8
**	bl	consume_z7
**	...
*/
void
test_z7 (svint8_t *ptr) [[arm::streaming]]
{
  consume_z7 (*ptr, *ptr, *ptr, *ptr, *ptr, *ptr, *ptr, *ptr);
}

void consume_p3 (svbool_t, svbool_t, svbool_t, svbool_t);

/*
** test_p3:
**	...
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	str	p1, \[sp, #1, mul vl\]
**	str	p2, \[sp, #2, mul vl\]
**	str	p3, \[sp, #3, mul vl\]
**	smstop	sm
**	ldr	p0, \[sp\]
**	ldr	p1, \[sp, #1, mul vl\]
**	ldr	p2, \[sp, #2, mul vl\]
**	ldr	p3, \[sp, #3, mul vl\]
**	addvl	sp, sp, #1
**	bl	consume_p3
**	...
*/
void
test_p3 (svbool_t *ptr) [[arm::streaming]]
{
  consume_p3 (*ptr, *ptr, *ptr, *ptr);
}

void consume_mixed (float, double, float32x4_t, svfloat32_t,
		    float, double, float64x2_t, svfloat64_t,
		    svbool_t, svbool_t, svbool_t, svbool_t);

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
**	smstop	sm
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
**	bl	consume_mixed
**	...
*/
void
test_mixed (float32x4_t *float32x4_ptr,
	    svfloat32_t *svfloat32_ptr,
	    float64x2_t *float64x2_ptr,
	    svfloat64_t *svfloat64_ptr,
	    svbool_t *svbool_ptr) [[arm::streaming]]
{
  consume_mixed (1.0f, 2.0, *float32x4_ptr, *svfloat32_ptr,
		 3.0f, 4.0, *float64x2_ptr, *svfloat64_ptr,
		 *svbool_ptr, *svbool_ptr, *svbool_ptr, *svbool_ptr);
}

void consume_varargs (float, ...);

/*
** test_varargs:
**	...
**	stp	q3, q7, \[sp, #?-32\]!
**	fmov	w10, s0
**	fmov	x11, d1
** (
**	fmov	x12, d2
** |
**	umov	x12, v2.d\[0\]
** )
**	fmov	x13, d4
**	fmov	x14, d5
** (
**	fmov	x15, d6
** |
**	umov	x15, v6.d\[0\]
** )
**	smstop	sm
**	fmov	s0, w10
**	fmov	d1, x11
**	fmov	d2, x12
**	fmov	d4, x13
**	fmov	d5, x14
**	fmov	d6, x15
**	ldp	q3, q7, \[sp\], #?32
**	bl	consume_varargs
**	...
*/
void
test_varargs (float32x2_t *float32x2_ptr,
	      float32x4_t *float32x4_ptr,
	      float64x1_t *float64x1_ptr,
	      float64x2_t *float64x2_ptr) [[arm::streaming]]
{
  consume_varargs (1.0f, 2.0, *float32x2_ptr, *float32x4_ptr,
		   3.0f, 4.0, *float64x1_ptr, *float64x2_ptr);
}
