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
**	leaq	104\(%rsp\), %rax
**	movq	%rdx, 104\(%rsp\)
**	movq	%r8, 112\(%rsp\)
**	movq	%r9, 120\(%rsp\)
**	movq	\$0, 64\(%rsp\)
**	movq	\$0, 72\(%rsp\)
**	movq	%rax, 56\(%rsp\)
**	cmpl	\$47, %eax
**	ja	.L2
**	leal	8\(%rax\), %edx
**	movl	\(%eax\), %esi
**	cmpl	\$47, %edx
**	ja	.L18
**	leal	16\(%rax\), %edi
**	movl	\(%edx\), %r8d
**	cmpl	\$47, %edi
**	ja	.L19
**	leal	24\(%rax\), %edx
**	movl	\(%edi\), %r9d
**	cmpl	\$47, %edx
**	ja	.L20
**	addl	\$32, %eax
**	movl	\(%edx\), %edi
**	cmpl	\$47, %eax
**	movl	%eax, %edx
**	movl	\$0, %eax
**	cmovbe	%rdx, %rax
**.L9:
**	movl	\(%rax\), %eax
**	movl	%edi, 32\(%rsp\)
**	movl	%esi, %edx
**	movl	%eax, 40\(%rsp\)
**	call	continuation
**	addq	\$88, %rsp
**	.cfi_remember_state
**	.cfi_def_cfa_offset 8
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L2:
**	.cfi_restore_state
**	movl	0, %esi
**	movl	8, %r8d
**	movl	\$16, %eax
**.L5:
**	movl	\(%rax\), %r9d
**	leaq	8\(%rax\), %rdx
**.L7:
**	movl	\(%rdx\), %edi
**	leaq	8\(%rdx\), %rax
**	jmp	.L9
**.L18:
**	movl	0, %r8d
**	movl	\$8, %eax
**	jmp	.L5
**.L20:
**	xorl	%edx, %edx
**	jmp	.L7
**.L19:
**	xorl	%eax, %eax
**	jmp	.L5
**	.cfi_endproc
**...
*/

#define CALLEE_ATTRIBUTE __attribute__ ((no_callee_saved_registers, ms_abi))
#define CALLER_ATTRIBUTE __attribute__ ((preserve_none, ms_abi))

#include "preserve-none-35a.c"
