/* { dg-do compile { target { fpic && lp64 } } } */
/* { dg-options "-O2 -mcmodel=large -pg -mno-fentry -fpic -fno-shrink-wrap" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^(1|\t?\.)} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	.cfi_.*
**1:	movabsq	\$_GLOBAL_OFFSET_TABLE_-1b, %r11
**	leaq	1b\(%rip\), %r10
**	addq	%r11, %r10
**	movabsq	\$mcount@PLTOFF, %r11
**	addq	%r11, %r10
**	call	\*%r10
**...
*/

void
foo (void)
{
}
