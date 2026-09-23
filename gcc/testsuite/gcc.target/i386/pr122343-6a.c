/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { *-*-darwin* && lp64 } } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movl	bar(|\(%rip\)), %eax
**	movl	bar(|\(%rip\)), %eax
**	imull	\$123, %eax, %edx
**	movl	%edx, bar(|\(%rip\))
**...
**	barrier
**...
**	movl	bar(|\(%rip\)), %edx
**	addl	%edx, %eax
**	subl	%eax, bar(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*#
* Darwin indirects extern accesses
*Dfoo:
*D	movq	_bar@GOTPCREL\(%rip\), %rax
*D	movl	\(%rax\), %edx
*D	movl	\(%rax\), %edx
*D	imull	\$123, %edx, %ecx
*D	movl	%ecx, \(%rax\)
*D...
*D	barrier
*D...
*D	movl	\(%rax\), %ecx
*D	addl	%ecx, %edx
*D	subl	%edx, \(%rax\)
*D	ret
*D...
*E
*/

extern volatile int bar;

void
foo (void)
{
  int h = bar;
  int r = bar;
  bar = r * 123;
  asm volatile ("barrier" ::: "memory");
  int p = bar;
  p = p + r;
  bar -= p;
}
