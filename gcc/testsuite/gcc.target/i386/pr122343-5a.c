/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { *-*-darwin* && lp64 } } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	imull	\$123, %e(di|ax), %e(di|ax)
**	addl	%e(di|ax), bar(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*#

* Darwin indirects extern accesses
*Dfoo:
*D	movq	_bar@GOTPCREL\(%rip\), %rax
*D	imull	\$123, %edi, %edi
*D	addl	%edi, \(%rax\)
*D	ret
*E
*/

extern volatile int bar;

void
foo (int z)
{
  z *= 123;
  bar += z;
}
