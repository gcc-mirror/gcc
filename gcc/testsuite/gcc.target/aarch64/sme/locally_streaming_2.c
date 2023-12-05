// { dg-options "-O -fomit-frame-pointer" }
// { dg-final { check-function-bodies "**" "" } }

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
*/
[[arm::locally_streaming]] double
test_d0 ()
{
  asm ("");
  return 1.0f;
}

/*
** test_d0_vec:
**	...
**	smstart	sm
**	...
** (
**	fmov	x10, d0
** |
**	umov	x10, v0.d\[0\]
** )
**	smstop	sm
**	fmov	d0, x10
**	...
*/
[[arm::locally_streaming]] int8x8_t
test_d0_vec ()
{
  asm ("");
  return (int8x8_t) {};
}

/*
** test_q0:
**	...
**	smstart	sm
**	...
**	str	q0, \[sp, #?-16\]!
**	smstop	sm
**	ldr	q0, \[sp\], #?16
**	...
*/
[[arm::locally_streaming]] int8x16_t
test_q0 ()
{
  asm ("");
  return (int8x16_t) {};
}

/*
** test_q1:
**	...
**	smstart	sm
**	...
**	stp	q0, q1, \[sp, #?-32\]!
**	smstop	sm
**	ldp	q0, q1, \[sp\], #?32
**	...
*/
[[arm::locally_streaming]] int8x16x2_t
test_q1 ()
{
  asm ("");
  return (int8x16x2_t) {};
}

/*
** test_q2:
**	...
**	smstart	sm
**	...
**	stp	q0, q1, \[sp, #?-48\]!
**	str	q2, \[sp, #?32\]
**	smstop	sm
**	ldr	q2, \[sp, #?32\]
**	ldp	q0, q1, \[sp\], #?48
**	...
*/
[[arm::locally_streaming]] int8x16x3_t
test_q2 ()
{
  asm ("");
  return (int8x16x3_t) {};
}

/*
** test_q3:
**	...
**	smstart	sm
**	...
**	stp	q0, q1, \[sp, #?-64\]!
**	stp	q2, q3, \[sp, #?32\]
**	smstop	sm
**	ldp	q2, q3, \[sp, #?32\]
**	ldp	q0, q1, \[sp\], #?64
**	...
*/
[[arm::locally_streaming]] int8x16x4_t
test_q3 ()
{
  asm ("");
  return (int8x16x4_t) {};
}

/*
** test_z0:
**	...
**	smstart	sm
**	mov	z0\.b, #0
**	addvl	sp, sp, #-1
**	str	z0, \[sp\]
**	smstop	sm
**	ldr	z0, \[sp\]
**	addvl	sp, sp, #1
**	...
*/
[[arm::locally_streaming]] svint8_t
test_z0 ()
{
  asm ("");
  return (svint8_t) {};
}

/*
** test_z3:
**	...
**	smstart	sm
**	...
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
**	...
*/
[[arm::locally_streaming]] svint8x4_t
test_z3 ()
{
  asm ("");
  return (svint8x4_t) {};
}

/*
** test_p0:
**	...
**	smstart	sm
**	pfalse	p0\.b
**	addvl	sp, sp, #-1
**	str	p0, \[sp\]
**	smstop	sm
**	ldr	p0, \[sp\]
**	addvl	sp, sp, #1
**	...
*/
[[arm::locally_streaming]] svbool_t
test_p0 ()
{
  asm ("");
  return (svbool_t) {};
}
