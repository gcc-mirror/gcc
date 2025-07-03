/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -pg -mrecord-mcount -mno-fentry -fpic -fno-plt -fno-shrink-wrap" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^(1|\t?\.)} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	.cfi_.*
**1:	call	\*mcount@GOTPCREL\(%rip\)
**...
*/

void
foo (void)
{
}
