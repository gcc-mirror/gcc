/* { dg-do compile } */
/* { dg-options "-O2 -march=x86-64-v4 -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**foo:
**.LFB0:
**	.cfi_startproc
**	cmpq	\$247, %rsi
**	jbe	.L16
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L16:
**	vpxor	%xmm0, %xmm0, %xmm0
**	vpternlogd	\$255, %zmm0, %zmm0, %zmm0
**	cmpl	\$64, %esi
**	jnb	.L17
**	cmpl	\$32, %esi
**	jnb	.L6
**	cmpl	\$16, %esi
**	jnb	.L7
**	cmpl	\$8, %esi
**	jnb	.L8
**	cmpl	\$4, %esi
**	jnb	.L9
**	cmpl	\$1, %esi
**	ja	.L10
**	jb	.L13
**	movb	\$-1, \(%rdi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L17:
**	vmovdqu8	%zmm0, \(%rdi\)
**	cmpl	\$128, %esi
**	ja	.L5
**	movl	%esi, %esi
**	vmovdqu8	%zmm0, -64\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L5:
**	movl	%esi, %esi
**	vmovdqu8	%zmm0, 64\(%rdi\)
**	vmovdqu8	%zmm0, -64\(%rdi,%rsi\)
**	vmovdqu8	%zmm0, -128\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L6:
**	movl	%esi, %esi
**	vmovdqu	%ymm0, \(%rdi\)
**	vmovdqu	%ymm0, -32\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L7:
**	movl	%esi, %esi
**	vmovdqu	%xmm0, \(%rdi\)
**	vmovdqu	%xmm0, -16\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L8:
**	movl	%esi, %esi
**	movq	\$-1, \(%rdi\)
**	movq	\$-1, -8\(%rdi,%rsi\)
**	vzeroupper
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L9:
**	movl	%esi, %esi
**	movl	\$-1, \(%rdi\)
**	movl	\$-1, -4\(%rdi,%rsi\)
**	vzeroupper
**	ret
**.L10:
**	movl	\$-1, %eax
**	movl	%esi, %esi
**	movw	%ax, \(%rdi\)
**	movw	%ax, -2\(%rdi,%rsi\)
**	vzeroupper
**	ret
**.L13:
**	vzeroupper
**	ret
**	.cfi_endproc
**...
*/

#include "builtin-memset-7a.c"
