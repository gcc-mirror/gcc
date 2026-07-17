/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -fno-pic -std=c23 -march=diamondrapids -mno-avx" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-linux* } {^\t?\.} } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	movq	%rdi, %xmm0
**	pinsrq	\$1, %rsi, %xmm0
**	movaps	%xmm0, store\(%rip\)
**	lock orq	\$0, \(%rsp\)
**	ret
**	.cfi_endproc
**...
*/

#include "pr126293-4a.c"
