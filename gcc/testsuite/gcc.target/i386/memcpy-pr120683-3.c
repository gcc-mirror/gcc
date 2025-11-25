/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -mmemcpy-strategy=vector_loop:2048:noalign,libcall:-1:noalign" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	xorl	%eax, %eax
**.L[0-9]+:
**	movl	%eax, %edx
**	addl	\$64, %eax
**	movdqa	src\(%rdx\), %xmm3
**	movdqa	src\+16\(%rdx\), %xmm2
**	movdqa	src\+32\(%rdx\), %xmm1
**	movdqa	src\+48\(%rdx\), %xmm0
**	movaps	%xmm3, dest\(%rdx\)
**	movaps	%xmm2, dest\+16\(%rdx\)
**	movaps	%xmm1, dest\+32\(%rdx\)
**	movaps	%xmm0, dest\+48\(%rdx\)
**	cmpl	\$256, %eax
**	jb	.L[0-9]+
**	movdqa	src\(%rax\), %xmm0
**	movaps	%xmm0, dest\(%rax\)
**	movdqu	src\+15\(%rax\), %xmm0
**	movups	%xmm0, dest\+15\(%rax\)
**	ret
**...
*/

#define SIZE 16 * 16 + 31

char dest[SIZE];
char src[SIZE];

void
foo (void)
{
  __builtin_memcpy (dest, src, SIZE);
}

/* { dg-final { scan-assembler-not "rep mov" } } */
