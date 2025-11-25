/* { dg-do compile { target { *-*-linux* && fpic } } } */
/* { dg-options "-O2 -pg -mno-fentry -mrecord-mcount -fno-pic -fno-shrink-wrap" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^(1|\t?\.)} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	.cfi_.*
**1:	call	mcount
**...
*/

void
foo (void)
{
}
