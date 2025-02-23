/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -mno-avx -msse2" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**assignzero:
**.LFB0:
**	.cfi_startproc
**	movl	4\(%esp\), %eax
**	pxor	%xmm0, %xmm0
**	movups	%xmm0, 32\(%eax\)
**	movups	%xmm0, \(%eax\)
**	movups	%xmm0, 16\(%eax\)
**	movups	%xmm0, 44\(%eax\)
**	ret
**...
*/

#include "pr82142a.c"
