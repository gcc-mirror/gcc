/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mmemcpy-strategy=vector_loop:2048:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB[0-9]+:
**	.cfi_startproc
**	xorl	%edx, %edx
**.L[0-9]+:
**	movl	%edx, %eax
**	addl	\$64, %edx
**	movdqa	src\(%rax\), %xmm3
**	movdqa	src\+16\(%rax\), %xmm2
**	movdqa	src\+32\(%rax\), %xmm1
**	movdqa	src\+48\(%rax\), %xmm0
**	movaps	%xmm3, dest\(%rax\)
**	movaps	%xmm2, dest\+16\(%rax\)
**	movaps	%xmm1, dest\+32\(%rax\)
**	movaps	%xmm0, dest\+48\(%rax\)
**	cmpl	\$256, %edx
**	jb	.L[0-9]+
**	movdqa	src\(%rdx\), %xmm0
**	movaps	%xmm0, dest\(%rdx\)
**	movdqu	src\+15\(%rdx\), %xmm0
**	movups	%xmm0, dest\+15\(%rdx\)
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
