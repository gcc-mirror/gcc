// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls -msve-vector-bits=128" }
// { dg-final { check-function-bodies "**" "" } }

#include <arm_sve.h>

svint8_t produce_z0 ();
void consume_z0 (svint8_t);

/*
** test_z0:	{ target aarch64_little_endian }
**	...
**	smstop	sm
**	bl	produce_z0
**	str	q0, \[sp, #?-16\]!
**	smstart	sm
**	ldr	q0, \[sp\], #?16
**	str	q0, \[sp, #?-16\]!
**	smstop	sm
**	ldr	q0, \[sp\], #?16
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
** test_z3:	{ target aarch64_little_endian }
**	...
**	smstop	sm
**	bl	produce_z3
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
** test_p0:	{ target aarch64_little_endian }
**	...
**	smstop	sm
**	bl	produce_p0
**	sub	sp, sp, #?16
**	str	p0, \[sp\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	add	sp, sp, #?16
**	sub	sp, sp, #?16
**	str	p0, \[sp\]
**	smstop	sm
**	ldr	p0, \[sp\]
**	add	sp, sp, #?16
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
