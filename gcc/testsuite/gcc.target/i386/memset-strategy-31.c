/* { dg-do compile } */
/* { dg-options "-O2 -mtune=generic -mno-avx -msse2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**...
**	pxor	%xmm0, %xmm0
**	xorl	%eax, %eax
**.L[0-9]+:
**	movl	%eax, %edx
**	addl	\$64, %eax
**	movups	%xmm0, \(%rdi,%rdx\)
**	movups	%xmm0, 16\(%rdi,%rdx\)
**	movups	%xmm0, 32\(%rdi,%rdx\)
**	movups	%xmm0, 48\(%rdi,%rdx\)
**	cmpl	\$192, %eax
**	jb	.L[0-9]+
**...
*/

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 254);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
