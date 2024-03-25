// { dg-options "-O -fomit-frame-pointer -fno-optimize-sibling-calls" }
// { dg-final { check-function-bodies "**" "" } }

#pragma GCC target "+sme2"

void inout_za() __arm_inout("za");
void inout_za_zt0() __arm_inout("za", "zt0");

void inout_za_out_zt0() __arm_inout("za") __arm_out("zt0");
void inout_za_in_zt0() __arm_inout("za") __arm_in("zt0");

/*
** test1:
**	str	x30, \[sp, #?-16\]!
**	bl	inout_za_zt0
**	ldr	x30, \[sp\], #?16
**	ret
*/
void test1() __arm_inout("za", "zt0")
{
  inout_za_zt0();
}

/*
** test2:
**	...
**	str	zt0, \[(?:x[0-9]+|sp)\]
**	...
**	bl	inout_za
**	...
**	ldr	zt0, \[(?:x[0-9]+|sp)\]
**	...
**	ret
*/
void test2() __arm_inout("za", "zt0")
{
  inout_za();
}

/*
** test3:
**	...
**	bl	inout_za
**	bl	inout_za_out_zt0
**	[^\n]+
**	ret
*/
void test3() __arm_inout("za", "zt0")
{
  inout_za_in_zt0();
  inout_za();
  inout_za_out_zt0();
}
