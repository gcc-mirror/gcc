/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	pxor	%xmm0, %xmm0
**	xorl	%eax, %eax
**	movups	%xmm0, 190\(%rdi\)
**	movups	%xmm0, 206\(%rdi\)
**	movups	%xmm0, 222\(%rdi\)
**	movups	%xmm0, 238\(%rdi\)
**.L[0-9]+:
**	movl	%eax, %edx
**	addl	\$64, %eax
**	movups	%xmm0, \(%rdi,%rdx\)
**	movups	%xmm0, 16\(%rdi,%rdx\)
**	movups	%xmm0, 32\(%rdi,%rdx\)
**	movups	%xmm0, 48\(%rdi,%rdx\)
**	cmpl	\$192, %eax
**	jb	.L[0-9]+
**	ret
**...
*/

void
foo (char *dest)
{
  __builtin_memset (dest, 0, 254);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
