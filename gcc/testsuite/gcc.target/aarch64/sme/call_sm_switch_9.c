// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls -msve-vector-bits=256" }
// { dg-final { check-function-bodies "**" "" } }

#include <arm_sve.h>

svint8_t produce_z0 ();
void consume_z0 (svint8_t);

/*
** test_z0:
**	...
**	smstop	sm
**	bl	produce_z0
**	sub	sp, sp, #?32
**	str	z0, \[sp\]
**	smstart	sm
**	ldr	z0, \[sp\]
**	add	sp, sp, #?32
**	sub	sp, sp, #?32
**	str	z0, \[sp\]
**	smstop	sm
**	ldr	z0, \[sp\]
**	add	sp, sp, #?32
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
**	sub	sp, sp, #?128
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
**	str	z2, \[sp, #2, mul vl\]
**	str	z3, \[sp, #3, mul vl\]
**	smstart	sm
**	ldr	z0, \[sp\]
**	ldr	z1, \[sp, #1, mul vl\]
**	ldr	z2, \[sp, #2, mul vl\]
**	ldr	z3, \[sp, #3, mul vl\]
**	add	sp, sp, #?128
**	sub	sp, sp, #?128
**	str	z0, \[sp\]
**	str	z1, \[sp, #1, mul vl\]
**	str	z2, \[sp, #2, mul vl\]
**	str	z3, \[sp, #3, mul vl\]
**	smstop	sm
**	ldr	z0, \[sp\]
**	ldr	z1, \[sp, #1, mul vl\]
**	ldr	z2, \[sp, #2, mul vl\]
**	ldr	z3, \[sp, #3, mul vl\]
**	add	sp, sp, #?128
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
**	sub	sp, sp, #?32
**	str	p0, \[sp\]
**	smstart	sm
**	ldr	p0, \[sp\]
**	add	sp, sp, #?32
**	sub	sp, sp, #?32
**	str	p0, \[sp\]
**	smstop	sm
**	ldr	p0, \[sp\]
**	add	sp, sp, #?32
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
