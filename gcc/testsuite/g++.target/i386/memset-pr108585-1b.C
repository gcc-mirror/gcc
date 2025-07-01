/* { dg-do compile } */
/* { dg-options "-O3 -march=x86-64 -std=c++20 -fasynchronous-unwind-tables -fdwarf2-cfi-asm -mno-stackrealign -fomit-frame-pointer" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target lp64 } {^\t?\.} } } */

/*
**_Z6squarei:
**.LFB[0-9]+:
**	.cfi_startproc
**	subq	\$104, %rsp
**	.cfi_def_cfa_offset 112
**	pxor	%xmm0, %xmm0
**	movq	%rsp, %rdi
**	movaps	%xmm0, \(%rsp\)
**	movaps	%xmm0, 16\(%rsp\)
**	movaps	%xmm0, 32\(%rsp\)
**	movaps	%xmm0, 48\(%rsp\)
**	movaps	%xmm0, 64\(%rsp\)
**	movaps	%xmm0, 80\(%rsp\)
**	call	_Z3fooPSt4byte
**	addq	\$104, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**...
*/

#include <cstddef>

#ifdef USE_CHAR
# define TYPE char
#else
# define TYPE std::byte
#endif

extern int foo(TYPE *arr);

int square(int num)
{
    TYPE arr[96] = {};
    return foo(arr);
}

/* { dg-final { scan-assembler-not "rep stos" } } */
