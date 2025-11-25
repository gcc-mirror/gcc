/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64 -std=c++17 -DMODE=2" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**_Z22makeDefaultConstructedv:
**.LFB[0-9]+:
**	.cfi_startproc
**	pxor	%xmm0, %xmm0
**	movl	\$0, 80\(%rdi\)
**	movq	%rdi, %rax
**	movups	%xmm0, \(%rdi\)
**	movups	%xmm0, 16\(%rdi\)
**	movups	%xmm0, 32\(%rdi\)
**	movups	%xmm0, 48\(%rdi\)
**	movups	%xmm0, 64\(%rdi\)
**	ret
**...
*/

#include "memset-pr118276-1a.C"

/* { dg-final { scan-assembler-not "rep stos" } } */
