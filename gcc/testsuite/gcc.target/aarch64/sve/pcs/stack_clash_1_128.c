/* { dg-do compile } */
/* { dg-options "-O -mlittle-endian -fshrink-wrap -fstack-clash-protection -msve-vector-bits=128 -g" } */
/* { dg-final { check-function-bodies "**" "" } } */

#pragma GCC aarch64 "arm_sve.h"

/*
** test_1:
**	sub	sp, sp, #288
**	str	p4, \[sp\]
**	str	p5, \[sp, #1, mul vl\]
**	str	p6, \[sp, #2, mul vl\]
**	str	p7, \[sp, #3, mul vl\]
**	str	p8, \[sp, #4, mul vl\]
**	str	p9, \[sp, #5, mul vl\]
**	str	p10, \[sp, #6, mul vl\]
**	str	p11, \[sp, #7, mul vl\]
**	str	p12, \[sp, #8, mul vl\]
**	str	p13, \[sp, #9, mul vl\]
**	str	p14, \[sp, #10, mul vl\]
**	str	p15, \[sp, #11, mul vl\]
** (
**	str	z8, \[sp, #2, mul vl\]
**	str	z9, \[sp, #3, mul vl\]
**	str	z10, \[sp, #4, mul vl\]
**	str	z11, \[sp, #5, mul vl\]
**	str	z12, \[sp, #6, mul vl\]
**	str	z13, \[sp, #7, mul vl\]
**	str	z14, \[sp, #8, mul vl\]
**	str	z15, \[sp, #9, mul vl\]
**	str	z16, \[sp, #10, mul vl\]
**	str	z17, \[sp, #11, mul vl\]
**	str	z18, \[sp, #12, mul vl\]
**	str	z19, \[sp, #13, mul vl\]
**	str	z20, \[sp, #14, mul vl\]
**	str	z21, \[sp, #15, mul vl\]
**	str	z22, \[sp, #16, mul vl\]
**	str	z23, \[sp, #17, mul vl\]
** |
**	stp	q8, q9, \[sp, 32\]
**	stp	q10, q11, \[sp, 64\]
**	stp	q12, q13, \[sp, 96\]
**	stp	q14, q15, \[sp, 128\]
**	stp	q16, q17, \[sp, 160\]
**	stp	q18, q19, \[sp, 192\]
**	stp	q20, q21, \[sp, 224\]
**	stp	q22, q23, \[sp, 256\]
** )
**	ptrue	p0\.b, vl16
** (
**	ldr	z8, \[sp, #2, mul vl\]
**	ldr	z9, \[sp, #3, mul vl\]
**	ldr	z10, \[sp, #4, mul vl\]
**	ldr	z11, \[sp, #5, mul vl\]
**	ldr	z12, \[sp, #6, mul vl\]
**	ldr	z13, \[sp, #7, mul vl\]
**	ldr	z14, \[sp, #8, mul vl\]
**	ldr	z15, \[sp, #9, mul vl\]
**	ldr	z16, \[sp, #10, mul vl\]
**	ldr	z17, \[sp, #11, mul vl\]
**	ldr	z18, \[sp, #12, mul vl\]
**	ldr	z19, \[sp, #13, mul vl\]
**	ldr	z20, \[sp, #14, mul vl\]
**	ldr	z21, \[sp, #15, mul vl\]
**	ldr	z22, \[sp, #16, mul vl\]
**	ldr	z23, \[sp, #17, mul vl\]
** |
**	ldp	q8, q9, \[sp, 32\]
**	ldp	q10, q11, \[sp, 64\]
**	ldp	q12, q13, \[sp, 96\]
**	ldp	q14, q15, \[sp, 128\]
**	ldp	q16, q17, \[sp, 160\]
**	ldp	q18, q19, \[sp, 192\]
**	ldp	q20, q21, \[sp, 224\]
**	ldp	q22, q23, \[sp, 256\]
** )
**	ldr	p4, \[sp\]
**	ldr	p5, \[sp, #1, mul vl\]
**	ldr	p6, \[sp, #2, mul vl\]
**	ldr	p7, \[sp, #3, mul vl\]
**	ldr	p8, \[sp, #4, mul vl\]
**	ldr	p9, \[sp, #5, mul vl\]
**	ldr	p10, \[sp, #6, mul vl\]
**	ldr	p11, \[sp, #7, mul vl\]
**	ldr	p12, \[sp, #8, mul vl\]
**	ldr	p13, \[sp, #9, mul vl\]
**	ldr	p14, \[sp, #10, mul vl\]
**	ldr	p15, \[sp, #11, mul vl\]
**	add	sp, sp, #?288
**	ret
*/
svbool_t
test_1 (void)
{
  asm volatile ("" :::
		"z0", "z1", "z2", "z3", "z4", "z5", "z6", "z7",
		"z8", "z9", "z10", "z11", "z12", "z13", "z14", "z15",
		"z16", "z17", "z18", "z19", "z20", "z21", "z22", "z23",
		"z24", "z25", "z26", "z27", "z28", "z29", "z30", "z31",
		"p0", "p1", "p2", "p3", "p4", "p5", "p6", "p7",
		"p8", "p9", "p10", "p11", "p12", "p13", "p14", "p15");
  return svptrue_b8 ();
}

/*
** test_2:
**	ptrue	p0\.b, vl16
**	ret
*/
svbool_t
test_2 (void)
{
  asm volatile ("" :::
		"z0", "z1", "z2", "z3", "z4", "z5", "z6", "z7",
		"z24", "z25", "z26", "z27", "z28", "z29", "z30", "z31",
		"p0", "p1", "p2", "p3");
  return svptrue_b8 ();
}

/*
** test_3:
**	sub	sp, sp, #96
**	str	p5, \[sp\]
**	str	p6, \[sp, #1, mul vl\]
**	str	p11, \[sp, #2, mul vl\]
** (
**	str	z8, \[sp, #1, mul vl\]
**	str	z13, \[sp, #2, mul vl\]
**	str	z19, \[sp, #3, mul vl\]
**	str	z20, \[sp, #4, mul vl\]
** |
**	stp	q8, q13, \[sp, 16\]
**	stp	q19, q20, \[sp, 48\]
** )
**	str	z22, \[sp, #5, mul vl\]
**	ptrue	p0\.b, vl16
** (
**	ldr	z8, \[sp, #1, mul vl\]
**	ldr	z13, \[sp, #2, mul vl\]
**	ldr	z19, \[sp, #3, mul vl\]
**	ldr	z20, \[sp, #4, mul vl\]
** |
**	ldp	q8, q13, \[sp, 16\]
**	ldp	q19, q20, \[sp, 48\]
** )
**	ldr	z22, \[sp, #5, mul vl\]
**	ldr	p5, \[sp\]
**	ldr	p6, \[sp, #1, mul vl\]
**	ldr	p11, \[sp, #2, mul vl\]
**	add	sp, sp, #?96
**	ret
*/
svbool_t
test_3 (void)
{
  asm volatile ("" :::
		"z8", "z13", "z19", "z20", "z22",
		"p5", "p6", "p11");
  return svptrue_b8 ();
}

/*
** test_4:
**	sub	sp, sp, #16
**	str	p4, \[sp\]
**	ptrue	p0\.b, vl16
**	ldr	p4, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
svbool_t
test_4 (void)
{
  asm volatile ("" ::: "p4");
  return svptrue_b8 ();
}

/*
** test_5:
**	sub	sp, sp, #16
**	str	z15, \[sp\]
**	ptrue	p0\.b, vl16
**	ldr	z15, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
svbool_t
test_5 (void)
{
  asm volatile ("" ::: "z15");
  return svptrue_b8 ();
}

/*
** test_6:
**	sub	sp, sp, #16
**	str	z15, \[sp\]
**	mov	z0\.b, #1
**	ldr	z15, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
svint8_t
test_6 (svbool_t p0, svbool_t p1, svbool_t p2, svbool_t p3)
{
  asm volatile ("" :: "Upa" (p0), "Upa" (p1), "Upa" (p2), "Upa" (p3) : "z15");
  return svdup_s8 (1);
}

/*
** test_7:
**	sub	sp, sp, #16
**	str	z16, \[sp\]
**	ptrue	p0\.b, vl16
**	ldr	z16, \[sp\]
**	add	sp, sp, #?16
**	ret
*/
svbool_t
test_7 (void)
{
  asm volatile ("" ::: "z16");
  return svptrue_b8 ();
}
