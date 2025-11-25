/* { dg-do compile } */
/* { dg-options "-O2 -mno-avx -msse2 -mtune=generic -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
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
**	movdqu	\(%(?:r|e)si\), %xmm1
**	movdqu	15\(%(?:r|e)si\), %xmm0
**	movups	%xmm1, \(%(?:r|e)di\)
**	movups	%xmm0, 15\(%(?:r|e)di\)
**	ret
**...
*/

/*
**memmove39:
**.LFB[0-9]+:
**	.cfi_startproc
**	movdqu	\(%(?:r|e)si\), %xmm1
**	movdqu	16\(%(?:r|e)si\), %xmm0
**	movq	31\(%(?:r|e)si\), %rax
**	movups	%xmm0, 16\(%(?:r|e)di\)
**	movups	%xmm1, \(%(?:r|e)di\)
**	movq	%rax, 31\(%(?:r|e)di\)
**	ret
**...
*/

/*
**memmove61:
**.LFB[0-9]+:
**	.cfi_startproc
**	movdqu	\(%(?:r|e)si\), %xmm3
**	movdqu	16\(%(?:r|e)si\), %xmm2
**	movdqu	32\(%(?:r|e)si\), %xmm1
**	movdqu	45\(%(?:r|e)si\), %xmm0
**	movups	%xmm3, \(%(?:r|e)di\)
**	movups	%xmm1, 32\(%(?:r|e)di\)
**	movups	%xmm2, 16\(%(?:r|e)di\)
**	movups	%xmm0, 45\(%(?:r|e)di\)
**	ret
**...
*/

/*
**memmove69:
**.LFB[0-9]+:
**	.cfi_startproc
**	movdqu	\(%(?:r|e)si\), %xmm3
**	movdqu	16\(%(?:r|e)si\), %xmm2
**	movdqu	32\(%(?:r|e)si\), %xmm1
**	movdqu	48\(%(?:r|e)si\), %xmm0
**	movq	61\(%(?:r|e)si\), %rax
**	movups	%xmm3, \(%(?:r|e)di\)
**	movups	%xmm0, 48\(%(?:r|e)di\)
**	movups	%xmm2, 16\(%(?:r|e)di\)
**	movq	%rax, 61\(%(?:r|e)di\)
**	movups	%xmm1, 32\(%(?:r|e)di\)
**	ret
**...
*/

/*
**memmove93:
**.LFB[0-9]+:
**	.cfi_startproc
**	movdqu	\(%(?:r|e)si\), %xmm5
**	movdqu	16\(%(?:r|e)si\), %xmm4
**	movdqu	32\(%(?:r|e)si\), %xmm3
**	movdqu	48\(%(?:r|e)si\), %xmm2
**	movdqu	64\(%(?:r|e)si\), %xmm1
**	movdqu	77\(%(?:r|e)si\), %xmm0
**	movups	%xmm5, \(%(?:r|e)di\)
**	movups	%xmm4, 16\(%(?:r|e)di\)
**	movups	%xmm1, 64\(%(?:r|e)di\)
**	movups	%xmm3, 32\(%(?:r|e)di\)
**	movups	%xmm2, 48\(%(?:r|e)di\)
**	movups	%xmm0, 77\(%(?:r|e)di\)
**	ret
**...
*/

#define TEST(n) \
  void \
  memmove##n (void *a, void *b) \
  { \
    __builtin_memmove (a, b, n); \
  }

TEST (7)
TEST (13)
TEST (31)
TEST (39)
TEST (61)
TEST (69)
TEST (93)
