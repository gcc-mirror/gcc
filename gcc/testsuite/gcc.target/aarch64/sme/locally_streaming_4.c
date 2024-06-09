// { dg-options "-O -fomit-frame-pointer" }
/* { dg-final { check-function-bodies "**" "" } } */

#include <arm_neon.h>
#include <arm_sve.h>

/*
** test_d0:
**	...
**	smstart	sm
**	...
**	fmov	x10, d0
**	smstop	sm
**	fmov	d0, x10
**	...
**	smstart	sm
**	...
**	smstop	sm
**	...
*/
void consume_d0 (double d0);

__arm_locally_streaming void
test_d0 ()
{
  asm ("");
  consume_d0 (1.0);
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
**	smstop	sm
**	fmov	d0, x10
**	fmov	d1, x11
**	fmov	d2, x12
**	fmov	d3, x13
**	fmov	d4, x14
**	fmov	d5, x15
**	fmov	d6, x16
**	fmov	d7, x17
**	...
*/
void consume_d7 (double d0, double d1, double d2, double d3,
		 double d4, double d5, double d6, double d7);
__arm_locally_streaming void
test_d7 ()
{
  asm ("");
  consume_d7 (1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 8.0);
  asm ("");
}

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
**	...
*/
void consume_q7 (int8x16x4_t q0, int8x16x4_t q4);

__arm_locally_streaming void
test_q7 (int8x16x4_t *ptr)
{
  asm ("");
  consume_q7 (ptr[0], ptr[1]);
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
**	...
*/
void consume_z7 (svint8x4_t z0, svint8x4_t z4);

__arm_locally_streaming void
test_z7 (svint8x4_t *ptr1, svint8x4_t *ptr2)
{
  asm ("");
  consume_z7 (*ptr1, *ptr2);
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
**	smstop	sm
**	ldr	p0, \[sp\]
**	ldr	p1, \[sp, #1, mul vl\]
**	ldr	p2, \[sp, #2, mul vl\]
**	ldr	p3, \[sp, #3, mul vl\]
**	addvl	sp, sp, #1
**	...
*/
void consume_p3 (svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3);

__arm_locally_streaming void
test_p3 (svbool_t *ptr1, svbool_t *ptr2, svbool_t *ptr3, svbool_t *ptr4)
{
  asm ("");
  consume_p3 (*ptr1, *ptr2, *ptr3, *ptr4);
  asm ("");
}
