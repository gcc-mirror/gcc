/* { dg-do compile } */
/* { dg-options "-O2 -fomit-frame-pointer" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-* } {^\t?\.} } } */

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
*/

extern volatile unsigned char u8;

void
test (void)
{
  u8 = u8 + u8;
  u8 = u8 - u8;
}
