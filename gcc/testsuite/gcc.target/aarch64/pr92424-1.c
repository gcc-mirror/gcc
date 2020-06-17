/* { dg-do "compile" } */
/* { dg-options "-O1" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Note: this test only checks the instructions in the function bodies,
   not the placement of the patch label or nops before the futncion.  */

/*
**f10_none:
**	nop
**	ret
*/
void
__attribute__ ((target("branch-protection=none"),
		patchable_function_entry (1, 0)))
f10_none ()
{
}

/*
**f10_pac:
**	hint	34 // bti c
**	nop
**	hint	25 // paciasp
**	hint	29 // autiasp
**	ret
*/
void
__attribute__ ((target("branch-protection=bti+pac-ret+leaf"),
		patchable_function_entry (1, 0)))
f10_pac ()
{
}

/*
**f10_bti:
**	hint	34 // bti c
**	nop
**	ret
*/
void
__attribute__ ((target("branch-protection=bti"),
		patchable_function_entry (1, 0)))
f10_bti ()
{
}

/*
**f11_none:
**	ret
*/
void
__attribute__ ((target("branch-protection=none"),
		patchable_function_entry (1, 1)))
f11_none ()
{
}

/*
**f11_pac:
**	hint	25 // paciasp
**	hint	29 // autiasp
**	ret
*/
void
__attribute__ ((target("branch-protection=bti+pac-ret+leaf"),
		patchable_function_entry (1, 1)))
f11_pac ()
{
}

/*
**f11_bti:
**	hint	34 // bti c
**	ret
*/
void
__attribute__ ((target("branch-protection=bti"),
		patchable_function_entry (1, 1)))
f11_bti ()
{
}

/*
**f21_none:
**	nop
**	ret
*/
void
__attribute__ ((target("branch-protection=none"),
		patchable_function_entry (2, 1)))
f21_none ()
{
}

/*
**f21_pac:
**	hint	34 // bti c
**	nop
**	hint	25 // paciasp
**	hint	29 // autiasp
**	ret
*/
void
__attribute__ ((target("branch-protection=bti+pac-ret+leaf"),
		patchable_function_entry (2, 1)))
f21_pac ()
{
}

/*
**f21_bti:
**	hint	34 // bti c
**	nop
**	ret
*/
void
__attribute__ ((target("branch-protection=bti"),
		patchable_function_entry (2, 1)))
f21_bti ()
{
}
