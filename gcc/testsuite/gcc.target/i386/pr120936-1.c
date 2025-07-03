/* { dg-do compile { target fpic } } */
/* { dg-options "-O2 -pg -mno-fentry -fno-pic -fno-shrink-wrap" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	.cfi_.*
**	call	mcount
**...
*/

void
foo (void)
{
}
