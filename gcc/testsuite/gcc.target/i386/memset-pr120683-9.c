/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -mmemset-strategy=vector_loop:256:noalign,libcall:-1:noalign -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	movabsq	\$72340172838076673, %rax
**	movzbl	%sil, %esi
**	imulq	%rax, %rsi
**	vpbroadcastq	%rsi, %zmm0
**	cmpq	\$256, %rdx
**	jnb	.L2
**	testb	\$-128, %dl
**	jne	.L22
**	testb	\$64, %dl
**	jne	.L23
**	testb	\$32, %dl
**	jne	.L24
**	testb	\$16, %dl
**	jne	.L25
**	testb	\$8, %dl
**	jne	.L26
**	testb	\$4, %dl
**	jne	.L27
**	testq	%rdx, %rdx
**	jne	.L28
**.L20:
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L2:
**	vmovdqu64	%zmm0, -256\(%rdi,%rdx\)
**	vmovdqu64	%zmm0, -192\(%rdi,%rdx\)
**	vmovdqu64	%zmm0, -128\(%rdi,%rdx\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rdx\)
**	subq	\$1, %rdx
**	cmpq	\$256, %rdx
**	jb	.L20
**	xorb	%dl, %dl
**	xorl	%eax, %eax
**.L11:
**	vmovdqu64	%zmm0, \(%rdi,%rax\)
**	vmovdqu64	%zmm0, 64\(%rdi,%rax\)
**	vmovdqu64	%zmm0, 128\(%rdi,%rax\)
**	vmovdqu64	%zmm0, 192\(%rdi,%rax\)
**	addq	\$256, %rax
**	cmpq	%rdx, %rax
**	jb	.L11
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L28:
**	movb	%sil, \(%rdi\)
**	testb	\$2, %dl
**	je	.L20
**	movw	%si, -2\(%rdi,%rdx\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L22:
**	vmovdqu64	%zmm0, \(%rdi\)
**	vmovdqu64	%zmm0, 64\(%rdi\)
**	vmovdqu64	%zmm0, -128\(%rdi,%rdx\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rdx\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L23:
**	vmovdqu64	%zmm0, \(%rdi\)
**	vmovdqu64	%zmm0, -64\(%rdi,%rdx\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L24:
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rdx\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L25:
**	vmovdqu	%xmm0, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rdx\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L26:
**	movq	%rsi, \(%rdi\)
**	movq	%rsi, -8\(%rdi,%rdx\)
**	jmp	.L20
**	.p2align 4,,10
**	.p2align 3
**.L27:
**	movl	%esi, \(%rdi\)
**	movl	%esi, -4\(%rdi,%rdx\)
**	jmp	.L20
**	.cfi_endproc
**...
*/

void
foo (char *dest, int c, __SIZE_TYPE__ n)
{
  __builtin_memset (dest, c, n);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
