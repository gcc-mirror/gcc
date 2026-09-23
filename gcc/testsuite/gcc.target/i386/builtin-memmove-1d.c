/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -march=x86-64 -mtune=generic -minline-all-stringops" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**memmove7:
**.LFB[0-9]+:
**	.cfi_startproc
**	movl	\(%rsi\), %edx
**	movl	3\(%rsi\), %eax
**	movl	%edx, \(%rdi\)
**	movl	%eax, 3\(%rdi\)
**	ret
**...
*/

/*
**memmove13:
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	\(%rsi\), %rdx
**	movq	5\(%rsi\), %rax
**	movq	%rdx, \(%rdi\)
**	movq	%rax, 5\(%rdi\)
**	ret
**...
*/

/*
**memmove31:
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	\(%(e|r)si\), %r8
**	movq	8\(%(e|r)si\), %rcx
**	movq	16\(%(e|r)si\), %rdx
**	movq	23\(%(e|r)si\), %rax
**	movq	%r8, \(%(e|r)di\)
**	movq	%rdx, 16\(%(e|r)di\)
**	movq	%rcx, 8\(%(e|r)di\)
**	movq	%rax, 23\(%(e|r)di\)
**	ret
**...
*/

/*
**memmove39:
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	\(%rsi\), %r9
**	movq	8\(%rsi\), %r8
**	movq	16\(%rsi\), %rcx
**	movq	24\(%rsi\), %rdx
**	movq	31\(%rsi\), %rax
**	movq	%r9, \(%rdi\)
**	movq	%rdx, 24\(%rdi\)
**	movq	%r8, 8\(%rdi\)
**	movq	%rcx, 16\(%rdi\)
**	movq	%rax, 31\(%rdi\)
**	ret
**...
*/

/*
**memmove61:
**.LFB[0-9]+:
**	.cfi_startproc
**	movq	8\(%rsi\), %r11
**	movq	16\(%rsi\), %r10
**	pushq	%rbx
**	.cfi_def_cfa_offset 16
**	.cfi_offset 3, -16
**	movq	24\(%rsi\), %r9
**	movq	\(%rsi\), %rbx
**	movq	32\(%rsi\), %r8
**	movq	40\(%rsi\), %rcx
**	movq	48\(%rsi\), %rdx
**	movq	53\(%rsi\), %rax
**	movq	%rbx, \(%rdi\)
**	movq	%r11, 8\(%rdi\)
**	popq	%rbx
**	.cfi_def_cfa_offset 8
**	movq	%rdx, 48\(%rdi\)
**	movq	%r10, 16\(%rdi\)
**	movq	%r9, 24\(%rdi\)
**	movq	%r8, 32\(%rdi\)
**	movq	%rcx, 40\(%rdi\)
**	movq	%rax, 53\(%rdi\)
**	ret
**...
*/

/*
**memmove69:
**.LFB5:
**	.cfi_startproc
**	movq	16\(%rsi\), %r11
**	movq	24\(%rsi\), %r10
**	pushq	%rbp
**	.cfi_def_cfa_offset 16
**	.cfi_offset 6, -16
**	movq	32\(%rsi\), %r9
**	movq	\(%rsi\), %rbp
**	pushq	%rbx
**	.cfi_def_cfa_offset 24
**	.cfi_offset 3, -24
**	movq	40\(%rsi\), %r8
**	movq	8\(%rsi\), %rbx
**	movq	48\(%rsi\), %rcx
**	movq	56\(%rsi\), %rdx
**	movq	61\(%rsi\), %rax
**	movq	%rbp, \(%rdi\)
**	movq	%rbx, 8\(%rdi\)
**	popq	%rbx
**	.cfi_def_cfa_offset 16
**	movq	%rdx, 56\(%rdi\)
**	popq	%rbp
**	.cfi_def_cfa_offset 8
**	movq	%r11, 16\(%rdi\)
**	movq	%r10, 24\(%rdi\)
**	movq	%r9, 32\(%rdi\)
**	movq	%r8, 40\(%rdi\)
**	movq	%rcx, 48\(%rdi\)
**	movq	%rax, 61\(%rdi\)
**	ret
**...
*/

/*
**memmove93:
**.LFB[0-9]+:
**	.cfi_startproc
**	sub(l|q)	\$24, %(e|r)sp
**	.cfi_def_cfa_offset 32
**	mov(l|q)	%(e|r)si, %(e|r)ax
**	movl	\$93, %ecx
**	cmp(l|q)	%(e|r)di, %(e|r)si
**	jb	.L14
**	je	.L10
**	movq	%rbx, \(%(e|r)sp\)
**	mov(l|q)	%(e|r)di, %(e|r)dx
**	movq	%r14, 8\(%(e|r)sp\)
**	movq	%r15, 16\(%(e|r)sp\)
**	.cfi_offset 3, -32
**	.cfi_offset 14, -24
**	.cfi_offset 15, -16
**	movq	85\(%(e|r)si\), %r14
**	movq	77\(%(e|r)si\), %r15
**	movq	69\(%(e|r)si\), %r10
**	movq	61\(%(e|r)si\), %r11
**.L15:
**	movq	8\(%(e|r)ax\), %r9
**	movq	16\(%(e|r)ax\), %r8
**	subl	\$32, %ecx
**	add(l|q)	\$32, %(e|r)dx
**	movq	24\(%(e|r)ax\), %rsi
**	movq	\(%(e|r)ax\), %rbx
**	add(l|q)	\$32, %(e|r)ax
**	movq	%r9, -24\(%(e|r)dx\)
**	movq	%rbx, -32\(%(e|r)dx\)
**	movq	%r8, -16\(%(e|r)dx\)
**	movq	%rsi, -8\(%(e|r)dx\)
**	cmpl	\$32, %ecx
**	ja	.L15
**	movq	%r10, 69\(%(e|r)di\)
**	movq	\(%(e|r)sp\), %rbx
**	.cfi_restore 3
**	movq	%r11, 61\(%(e|r)di\)
**	movq	%r14, 85\(%(e|r)di\)
**	movq	8\(%(e|r)sp\), %r14
**	.cfi_restore 14
**	movq	%r15, 77\(%(e|r)di\)
**	movq	16\(%(e|r)sp\), %r15
**	.cfi_restore 15
**.L10:
**	add(l|q)	\$24, %(e|r)sp
**	.cfi_remember_state
**	.cfi_def_cfa_offset 8
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L14:
**	.cfi_restore_state
**	movq	%rbx, \(%(e|r)sp\)
**	lea(l|q)	93\(%(e|r)di\), %(e|r)dx
**	add(l|q)	\$93, %(e|r)ax
**	movq	%r14, 8\(%(e|r)sp\)
**	movq	%r15, 16\(%(e|r)sp\)
**	.cfi_offset 3, -32
**	.cfi_offset 14, -24
**	.cfi_offset 15, -16
**	movq	\(%(e|r)si\), %r14
**	movq	8\(%(e|r)si\), %r15
**	movq	16\(%(e|r)si\), %r10
**	movq	24\(%(e|r)si\), %r11
**.L16:
**	movq	-16\(%(e|r)ax\), %r9
**	movq	-24\(%(e|r)ax\), %r8
**	subl	\$32, %ecx
**	sub(l|q)	\$32, %(e|r)dx
**	movq	-32\(%(e|r)ax\), %rsi
**	movq	-8\(%(e|r)ax\), %rbx
**	sub(l|q)	\$32, %(e|r)ax
**	movq	%r9, 16\(%(e|r)dx\)
**	movq	%rbx, 24\(%(e|r)dx\)
**	movq	%r8, 8\(%(e|r)dx\)
**	movq	%rsi, \(%(e|r)dx\)
**	cmpl	\$32, %ecx
**	ja	.L16
**	movq	%r14, \(%(e|r)di\)
**	movq	\(%(e|r)sp\), %rbx
**	.cfi_restore 3
**	movq	%r15, 8\(%(e|r)di\)
**	movq	8\(%(e|r)sp\), %r14
**	.cfi_restore 14
**	movq	%r10, 16\(%(e|r)di\)
**	movq	16\(%(e|r)sp\), %r15
**	.cfi_restore 15
**	movq	%r11, 24\(%(e|r)di\)
**	add(l|q)	\$24, %(e|r)sp
**	.cfi_def_cfa_offset 8
**	ret
**...
*/

#include "builtin-memmove-1a.c"
