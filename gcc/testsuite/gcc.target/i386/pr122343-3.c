/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { *-*-darwin* && lp64 } } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	addl	\$123, bar(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*#

* Darwin indirects the extern.
*Dfoo:
*D	movq	_bar@GOTPCREL\(%rip\), %rax
*D	addl	\$123, \(%rax\)
*D	ret
*D...
*E
*/

extern volatile int bar;

void
foo (void)
{
  bar += 123;
}
