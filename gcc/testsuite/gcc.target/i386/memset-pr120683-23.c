/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -minline-all-stringops -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movzbl	%dil, %edi
**	movl	\$p, %eax
**	movabsq	\$72340172838076673, %rdx
**	imulq	%rdx, %rdi
**	movq	%rdi, %xmm0
**	punpcklqdq	%xmm0, %xmm0
**	cmpq	\$64, %rsi
**	jnb	.L18
**.L2:
**	movq	%rsi, %rcx
**	andl	\$63, %ecx
**	je	.L1
**	xorl	%edx, %edx
**	andl	\$1, %esi
**	je	.L5
**	movl	\$1, %edx
**	movb	%dil, \(%rax\)
**	cmpq	%rcx, %rdx
**	jnb	.L19
**.L5:
**	movb	%dil, \(%rax,%rdx\)
**	movb	%dil, 1\(%rax,%rdx\)
**	addq	\$2, %rdx
**	cmpq	%rcx, %rdx
**	jb	.L5
**.L1:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L18:
**	movq	%rsi, %rdx
**	xorl	%eax, %eax
**	andq	\$-64, %rdx
**.L3:
**	movaps	%xmm0, p\(%rax\)
**	addq	\$64, %rax
**	movaps	%xmm0, p-48\(%rax\)
**	movaps	%xmm0, p-32\(%rax\)
**	movaps	%xmm0, p-16\(%rax\)
**	cmpq	%rdx, %rax
**	jb	.L3
**	addq	\$p, %rax
**	jmp	.L2
**.L19:
**	ret
**	.cfi_endproc
**...
*/


#define WRITE_CHUNK 256
char p[WRITE_CHUNK];

void
foo (int c, __SIZE_TYPE__ nbyte)
{
 __builtin_memset (p, c, nbyte);
}
