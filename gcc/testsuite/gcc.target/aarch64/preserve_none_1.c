/* { dg-do compile } */
/* { dg-options "-O2 -fno-schedule-insns2" } */
/* { dg-final { check-function-bodies "**" "" "" } } */
/* { dg-skip-if "" { *-*-mingw* } } */

void normal_callee();
void preserve_none_callee() [[gnu::preserve_none]];

#pragma GCC target "+sve"

/*
** preserve_none_caller1:
** ?#APP
**	nop
** ?#NO_APP
**	ret
*/
void preserve_none_caller1() [[gnu::preserve_none]]
{
  asm volatile ("nop" ::: "x0", "x1", "x2", "x3", "x4", "x5", "x6", "x7",
		"x8", "x9", "x10", "x11", "x12", "x13", "x14", "x15",
		"x16", "x17", "x18", "x19", "x20", "x21", "x22", "x23",
		"x24", "x25", "x26", "x27", "x28",

		"z0", "z1", "z2", "z3", "z4", "z5", "z6", "z7",
		"z8", "z9", "z10", "z11", "z12", "z13", "z14", "z15",
		"z16", "z17", "z18", "z19", "z20", "z21", "z22", "z23",
		"z24", "z25", "z26", "z27", "z28", "z29", "z30", "z31",

		"p0", "p1", "p2", "p3", "p4", "p5", "p6", "p7",
		"p8", "p9", "p10", "p11", "p12", "p13", "p14", "p15");
}

/*
** preserve_none_caller2:
**	stp	x29, x30, \[sp, #?-16\]!
**	mov	x29, sp
**	bl	normal_callee
**	mov	w0, w20
**	ldp	x29, x30, \[sp\], #?16
**	ret
*/
int preserve_none_caller2(int x) [[gnu::preserve_none]]
{
  normal_callee();
  return x;
}

/*
** preserve_none_caller3:
**	stp	x29, x30, \[sp, #?-32\]!
**	mov	x29, sp
**	str	w20, \[sp, #?[0-9]+\]
**	bl	preserve_none_callee
**	ldr	w0, \[sp, #?[0-9]+\]
**	ldp	x29, x30, \[sp\], #?32
**	ret
*/
int preserve_none_caller3(int x) [[gnu::preserve_none]]
{
  preserve_none_callee();
  return x;
}

/*
** preserve_none_caller4:
**	b	preserve_none_callee
*/
void preserve_none_caller4() [[gnu::preserve_none]]
{
  preserve_none_callee();
}

/*
** preserve_none_caller5:
**	b	preserve_none_callee
*/
void preserve_none_caller5(__SVBool_t x) [[gnu::preserve_none]]
{
  preserve_none_callee();
}

/*
** normal_caller1:
**	stp	x29, x30, \[sp, #?-160\]!
**	mov	x29, sp
**	stp	x19, x20, \[sp, #?16\]
**	stp	x21, x22, \[sp, #?32\]
**	stp	x23, x24, \[sp, #?48\]
**	stp	x25, x26, \[sp, #?64\]
**	stp	x27, x28, \[sp, #?80\]
**	stp	d8, d9, \[sp, #?96\]
**	stp	d10, d11, \[sp, #?112\]
**	stp	d12, d13, \[sp, #?128\]
**	stp	d14, d15, \[sp, #?144\]
**	bl	preserve_none_callee
**	ldp	d8, d9, \[sp, #?96\]
**	ldp	d10, d11, \[sp, #?112\]
**	ldp	d12, d13, \[sp, #?128\]
**	ldp	d14, d15, \[sp, #?144\]
**	ldp	x19, x20, \[sp, #?16\]
**	ldp	x21, x22, \[sp, #?32\]
**	ldp	x23, x24, \[sp, #?48\]
**	ldp	x25, x26, \[sp, #?64\]
**	ldp	x27, x28, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?160
**	ret
*/
void normal_caller1()
{
  preserve_none_callee();
}

/*
** normal_caller2:
**	stp	x29, x30, \[sp, #?-160\]!
**	mov	x29, sp
**	stp	x19, x20, \[sp, #?16\]
**	stp	x21, x22, \[sp, #?32\]
**	stp	x23, x24, \[sp, #?48\]
**	stp	x25, x26, \[sp, #?64\]
**	stp	x27, x28, \[sp, #?80\]
**	stp	d8, d9, \[sp, #?96\]
**	stp	d10, d11, \[sp, #?112\]
**	stp	d12, d13, \[sp, #?128\]
**	stp	d14, d15, \[sp, #?144\]
**	blr	x0
**	ldp	d8, d9, \[sp, #?96\]
**	ldp	d10, d11, \[sp, #?112\]
**	ldp	d12, d13, \[sp, #?128\]
**	ldp	d14, d15, \[sp, #?144\]
**	ldp	x19, x20, \[sp, #?16\]
**	ldp	x21, x22, \[sp, #?32\]
**	ldp	x23, x24, \[sp, #?48\]
**	ldp	x25, x26, \[sp, #?64\]
**	ldp	x27, x28, \[sp, #?80\]
**	ldp	x29, x30, \[sp\], #?160
**	ret
*/
void normal_caller2(void (*callee)() [[gnu::preserve_none]])
{
  callee();
}
