// PR121028
// { dg-do assemble { target aarch64_asm_sme_ok } }
// { dg-options "-O --save-temps" }
// { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {\t\.inst} } }

void ns_callee ();

/*
** sc_caller_sme:
**	...
**	mrs	x16, svcr
**	str	x16, \[x29, #?16\]
**	ldr	x16, \[x29, #?16\]
**	tbz	x16, 0, .*
**	smstop	sm
**	bl	ns_callee
**	ldr	x16, \[x29, #?16\]
**	tbz	x16, 0, .*
**	smstart	sm
**	...
*/
void sc_caller_sme() __arm_streaming_compatible
{
    ns_callee ();
}

#pragma GCC target "+nosme"

/*
** sc_caller_nosme:
**	...
**	bl	__arm_sme_state
**	str	x0, \[x29, #?16\]
**	ldr	x16, \[x29, #?16\]
**	tbz	x16, 0, .*
**	.inst 0xd503427f // smstop sm
**	bl	ns_callee
**	ldr	x16, \[x29, #?16\]
**	tbz	x16, 0, .*
**	.inst 0xd503437f // smstart sm
**	...
*/
void sc_caller_nosme() __arm_streaming_compatible
{
    ns_callee ();
}
