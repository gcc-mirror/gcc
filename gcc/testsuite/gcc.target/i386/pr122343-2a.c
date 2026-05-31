/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-add-options check_function_bodies } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target *-*-darwin* } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movl	bar1(|\(%rip\)), %eax
**	addl	bar2(|\(%rip\)), %eax
**	ret
**	.cfi_endproc
**...
*#

* Darwin indirects accesses to externs.
*Dfoo:
*D	movq	_bar1@GOTPCREL\(%rip\), %rax
*D	movq	_bar2@GOTPCREL\(%rip\), %rdx
*D	movl	\(%rax\), %eax
*D	addl	\(%rdx\), %eax
*D	ret
*D...
*E
*/

extern volatile int bar1, bar2;

int
foo (void)
{
  return bar1 + bar2;
}
