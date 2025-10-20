/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx512f -march=x86-64-v3 -mtune=generic -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { ! ia32 } } {^\t?\.} } } */

/*
**memmove7:
**.LFB[0-9]+:
**	.cfi_startproc
**	movl	\(%(?:r|e)si\), %edx
**	movl	3\(%(?:r|e)si\), %eax
**	movl	%edx, \(%(?:r|e)di\)
**	movl	%eax, 3\(%(?:r|e)di\)
**	ret
**...
*/

/*
**memmove13:
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	\(%(?:r|e)si\), %rdx
**	movq	5\(%(?:r|e)si\), %rax
**	movq	%rdx, \(%(?:r|e)di\)
**	movq	%rax, 5\(%(?:r|e)di\)
**	ret
**...
*/

/*
**memmove31:
**.LFB[0-9]+:
**	.cfi_startproc
**	vmovdqu	\(%(?:r|e)si\), %xmm1
**	vmovdqu	15\(%(?:r|e)si\), %xmm0
**	vmovdqu	%xmm1, \(%(?:r|e)di\)
**	vmovdqu	%xmm0, 15\(%(?:r|e)di\)
**	ret
**...
*/

/*
**memmove39:
**.LFB[0-9]+:
**	.cfi_startproc
**	vmovdqu	\(%(?:r|e)si\), %ymm0
**	movq	31\(%(?:r|e)si\), %rax
**	vmovdqu	%ymm0, \(%(?:r|e)di\)
**	movq	%rax, 31\(%(?:r|e)di\)
**	vzeroupper
**	ret
**...
*/

/*
**memmove61:
**.LFB[0-9]+:
**	.cfi_startproc
**	vmovdqu	\(%(?:r|e)si\), %ymm1
**	vmovdqu	29\(%(?:r|e)si\), %ymm0
**	vmovdqu	%ymm1, \(%(?:r|e)di\)
**	vmovdqu	%ymm0, 29\(%(?:r|e)di\)
**	vzeroupper
**	ret
**...
*/

/*
**memmove69:
**.LFB[0-9]+:
**	.cfi_startproc
**	vmovdqu	32\(%(?:r|e)si\), %ymm0
**	movq	61\(%(?:r|e)si\), %rax
**	vmovdqu	\(%(?:r|e)si\), %ymm1
**	vmovdqu	%ymm0, 32\(%(?:r|e)di\)
**	movq	%rax, 61\(%(?:r|e)di\)
**	vmovdqu	%ymm1, \(%(?:r|e)di\)
**	vzeroupper
**	ret
**...
*/

/*
**memmove93:
**.LFB[0-9]+:
**	.cfi_startproc
**	vmovdqu	\(%(?:r|e)si\), %ymm2
**	vmovdqu	32\(%(?:r|e)si\), %ymm1
**	vmovdqu	61\(%(?:r|e)si\), %ymm0
**	vmovdqu	%ymm1, 32\(%(?:r|e)di\)
**	vmovdqu	%ymm2, \(%(?:r|e)di\)
**	vmovdqu	%ymm0, 61\(%(?:r|e)di\)
**	vzeroupper
**	ret
**...
*/

#include "builtin-memmove-1a.c"
