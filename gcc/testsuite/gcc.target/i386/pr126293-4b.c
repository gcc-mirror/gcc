/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -fno-pic -std=c23 -march=diamondrapids" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-linux* } {^\t?\.} } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	vmovq	%rdi, %xmm1
**	vpinsrq	\$1, %rsi, %xmm1, %xmm0
**	vmovdqa	%xmm0, store\(%rip\)
**	lock orq	\$0, \(%rsp\)
**	ret
**	.cfi_endproc
**...
*/

#include "pr126293-4a.c"
