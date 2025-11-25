/* { dg-do compile { target { { *-*-linux* && lp64 } && fpic } } } */
/* { dg-options "-O2 -mcmodel=large -pg -mno-fentry -fno-pic -fno-shrink-wrap" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	.cfi_.*
**	movabsq	\$mcount, %r10
**	call	\*%r10
**...
*/

void
foo (void)
{
}
