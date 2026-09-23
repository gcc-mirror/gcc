/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* { dg-final { check-function-bodies "**" "*#" "" { target { ! *-*-darwin* } } {^\t?\.} } } */
/* { dg-final { check-function-bodies "*D" "*E" "" { target { *-*-darwin* && lp64 } } {^\t?\.} } } */

/*
**test:
**.LFB0:
**	.cfi_startproc
**	movzbl	u8(|\(%rip\)), %eax
**	addb	%al, u8(|\(%rip\))
**	movzbl	u8(|\(%rip\)), %eax
**	subb	u8(|\(%rip\)), %al
**	movb	%al, u8(|\(%rip\))
**	ret
**	.cfi_endproc
**...
*#

*Dtest:
*D	movq	_u8@GOTPCREL\(%rip\), %rax
*D	movzbl	\(%rax\), %edx
*D	addb	%dl, \(%rax\)
*D	movzbl	\(%rax\), %edx
*D	subb	\(%rax\), %dl
*D	movb	%dl, \(%rax\)
*D	ret
*D...
*E
*/

extern volatile unsigned char u8;

void
test (void)
{
  u8 = u8 + u8;
  u8 = u8 - u8;
}
