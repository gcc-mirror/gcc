/* { dg-do compile { target { ! ia32 } } } */
/* { dg-additional-options "-O2 -fno-pic -std=c23 -march=skylake" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target *-*-linux* } {^\t?\.} } } */

/*
**func:
**.LFB0:
**	.cfi_startproc
**	movq	%rsi, %rdx
**	movl	\$5, %ecx
**	movq	%rdi, %rsi
**	movl	\$store, %edi
**	jmp	__atomic_store_16
**	.cfi_endproc
**...
*/

#define ATTRIBUTE __attribute__ ((target("arch=x86-64")))
#include "pr126293-4a.c"
