// { dg-options "-O2 -fno-schedule-insns -fno-schedule-insns2" }
// { dg-final { check-function-bodies "**" "" } }

#pragma GCC target "+sme2"

void gen_zt0() __arm_preserves("za") __arm_out("zt0");
void callee() __arm_inout("za");

/*
** caller_inout:
**	...
**	str	zt0, \[[^\n]+\]
**	bl	callee
**	ldr	zt0, \[[^\n]+\]
**	...
**	ret
*/
void caller_inout() __arm_inout("za", "zt0") { callee(); }

/*
** caller_in:
**	...
**	str	zt0, \[[^\n]+\]
**	bl	callee
**	ldr	zt0, \[[^\n]+\]
**	...
**	ret
*/
void caller_in() __arm_inout("za") __arm_in("zt0") { callee(); }

/*
** caller_out:
**	...
**	str	zt0, \[[^\n]+\]
**	bl	callee
**	ldr	zt0, \[[^\n]+\]
**	...
**	ret
*/
void caller_out() __arm_inout("za") __arm_in("zt0") { gen_zt0(); callee(); }

/*
** caller_preserves:
**	...
**	str	zt0, \[[^\n]+\]
**	bl	callee
**	ldr	zt0, \[[^\n]+\]
**	...
**	ret
*/
void caller_preserves() __arm_inout("za") __arm_preserves("zt0") { callee(); }
