/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O2 -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-push-args -fomit-frame-pointer -mtune=generic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**entry:
**.LFB0:
**	.cfi_startproc
**	subq	\$88, %rsp
**	.cfi_def_cfa_offset 96
**	movl	%edi, %r12d
**	leaq	96\(%rsp\), %rax
**	movq	%rsi, 40\(%rsp\)
**	movl	40\(%rsp\), %r13d
**	movq	%rdx, 48\(%rsp\)
**	movl	48\(%rsp\), %r14d
**	movq	%rcx, 56\(%rsp\)
**	movl	56\(%rsp\), %r15d
**	movq	%r8, 64\(%rsp\)
**	movl	64\(%rsp\), %edi
**	movq	%r9, 72\(%rsp\)
**	movl	72\(%rsp\), %esi
**	movq	%rax, 16\(%rsp\)
**	leaq	32\(%rsp\), %rax
**	movq	%rax, 24\(%rsp\)
**	xorl	%eax, %eax
**	movl	\$8, 8\(%rsp\)
**	call	continuation
**	addq	\$88, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**	.cfi_endproc
**...
*/

#define CALLER_ATTRIBUTE __attribute__ ((no_callee_saved_registers, sysv_abi))

#include "preserve-none-35a.c"
