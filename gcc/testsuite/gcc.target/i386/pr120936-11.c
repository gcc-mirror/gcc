/* { dg-do compile { target { fpic && lp64 } } } */
/* { dg-options "-O2 -mrecord-mcount -mcmodel=large -pg -mno-fentry -fno-pic -fno-shrink-wrap" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^(1|\t?\.)} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	.cfi_.*
**1:	movabsq	\$mcount, %r10
**	call	\*%r10
**...
*/

void
foo (void)
{
}
