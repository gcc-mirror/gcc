// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls" }
// { dg-final { check-function-bodies "**" "" } }

#pragma GCC target "+sme2"

void inout_zt0() __arm_inout("zt0");
void out_zt0() __arm_out("zt0");
void normal();

/*
** test1:
**	str	x30, \[sp, #?-16\]!
**	bl	inout_zt0
**	ldr	x30, \[sp\], #?16
**	ret
*/
void test1() __arm_inout("zt0")
{
  inout_zt0();
}

/*
** test2:
**	str	x30, \[sp, #?-80\]!
**	add	(x[0-9]+), sp, #?16
**	str	zt0, \[\1\]
**	smstop	za
**	bl	normal
**	smstart	za
**	add	(x[0-9]+), sp, #?16
**	ldr	zt0, \[\1\]
**	ldr	x30, \[sp\], #?80
**	ret
*/
void test2() __arm_inout("zt0")
{
  normal();
}

/*
** test3:
**	...
**	smstop	za
**	bl	normal
**	smstart	za
**	bl	out_zt0
**	ldr	[^\n]+
**	ret
*/
void test3() __arm_inout("zt0")
{
  normal();
  out_zt0();
}
