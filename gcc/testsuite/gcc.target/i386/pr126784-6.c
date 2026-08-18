/* { dg-do compile { target { *-*-linux* && { ! ia32 } } } } */
/* { dg-options "-O2 -march=x86-64" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	movl	%edi, %edi
**	pxor	%xmm0, %xmm0
**	cvtsi2ssq	%rdi, %xmm0
**	cvttss2siq	%xmm0, %rax
**	movl	%eax, var\(%rip\)
**	ret
**...
*/

extern unsigned int var;

void
func (unsigned int i)
{
  var = (float) i;
}
