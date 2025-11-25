/* { dg-do compile { target { "i?86-*-* x86_64-*-*" } } } */
/* { dg-options "-O2 -fcf-protection" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**_Z3foov:
**.LFB[0-9]+:
**	.cfi_startproc
**	ret
**...
*/

extern void foo (void) __attribute__((nocf_check));

void
foo (void)
{
}
