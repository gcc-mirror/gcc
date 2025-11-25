/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	pxor	%xmm0, %xmm0
**	xorl	%eax, %eax
**.L[0-9]+:
**	movl	%eax, %edx
**	addl	\$64, %eax
**	movaps	%xmm0, dest\(%rdx\)
**	movaps	%xmm0, dest\+16\(%rdx\)
**	movaps	%xmm0, dest\+32\(%rdx\)
**	movaps	%xmm0, dest\+48\(%rdx\)
**	cmpl	\$128, %eax
**	jb	.L[0-9]+
**	movq	\$0, dest\+48\(%rax\)
**	movaps	%xmm0, dest\(%rax\)
**	movaps	%xmm0, dest\+16\(%rax\)
**	movaps	%xmm0, dest\+32\(%rax\)
**	ret
**...
*/

char dest[184];

void
foo (void)
{
  __builtin_memset (dest, 0, sizeof (dest));
}

/* { dg-final { scan-assembler-not "rep stos" } } */
