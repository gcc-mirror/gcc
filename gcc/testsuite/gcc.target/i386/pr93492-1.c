/* { dg-do "compile" { target *-*-linux* } } */
/* { dg-options "-O1 -fcf-protection -mmanual-endbr" } */
/* { dg-final { check-function-bodies "**" "" } } */

/* Note: this test only checks the instructions in the function bodies,
   not the placement of the patch label or nops before the function.  */

/*
**f10_none:
**	nop
**	ret
*/
void
__attribute__ ((nocf_check,patchable_function_entry (1, 0)))
f10_none (void)
{
}

/*
**f10_endbr:
**	endbr(32|64)
**	nop
**	ret
*/
void
__attribute__ ((cf_check,patchable_function_entry (1, 0)))
f10_endbr (void)
{
}

/*
**f11_none:
**	ret
*/
void
__attribute__ ((nocf_check,patchable_function_entry (1, 1)))
f11_none (void)
{
}

/*
**f11_endbr:
**	endbr(32|64)
**	ret
*/
void
__attribute__ ((cf_check,patchable_function_entry (1, 1)))
f11_endbr (void)
{
}

/*
**f21_none:
**	nop
**	ret
*/
void
__attribute__ ((nocf_check,patchable_function_entry (2, 1)))
f21_none (void)
{
}

/*
**f21_endbr:
**	endbr(32|64)
**	nop
**	ret
*/
void
__attribute__ ((cf_check,patchable_function_entry (2, 1)))
f21_endbr (void)
{
}
