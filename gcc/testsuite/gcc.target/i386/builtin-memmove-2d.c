/* { dg-do compile } */
/* { dg-options "-O2 -mgeneral-regs-only -march=x86-64 -mtune=generic -minline-all-stringops" } */
/* { dg-add-options check_function_bodies } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target { lp64 } } {^\t?\.} } } */

/*
**gcc_memmove_gpr:
**.LFB0:
**	.cfi_startproc
**	movq	%rdi, %rax
**	cmpq	\$8, %rdx
**	jb	.L3
**	cmpq	\$16, %rdx
**	jbe	.L19
**	subq	\$32, %rsp
**	.cfi_def_cfa_offset 40
**	cmpq	\$64, %rdx
**	jbe	.L20
**	movq	%rsi, %rcx
**	movq	%rdx, %rsi
**	cmpq	%rdi, %rcx
**	jb	.L10
**	je	.L2
**	movq	%rbx, \(%rsp\)
**	movq	%rbp, 8\(%rsp\)
**	movq	%r14, 16\(%rsp\)
**	movq	%r15, 24\(%rsp\)
**	.cfi_offset 3, -40
**	.cfi_offset 6, -32
**	.cfi_offset 14, -24
**	.cfi_offset 15, -16
**	movq	-8\(%rcx,%rdx\), %r15
**	movq	-16\(%rcx,%rdx\), %r14
**	movq	-24\(%rcx,%rdx\), %rbp
**	movq	-32\(%rcx,%rdx\), %r11
**.L11:
**	movq	8\(%rcx\), %r10
**	movq	16\(%rcx\), %r9
**	subq	\$32, %rsi
**	addq	\$32, %rdi
**	movq	24\(%rcx\), %r8
**	movq	\(%rcx\), %rbx
**	addq	\$32, %rcx
**	movq	%r10, -24\(%rdi\)
**	movq	%rbx, -32\(%rdi\)
**	movq	%r9, -16\(%rdi\)
**	movq	%r8, -8\(%rdi\)
**	cmpq	\$32, %rsi
**	ja	.L11
**	movq	%r15, -8\(%rax,%rdx\)
**	movq	%r14, -16\(%rax,%rdx\)
**	movq	%rbp, -24\(%rax,%rdx\)
**	movq	%r11, -32\(%rax,%rdx\)
**	movq	\(%rsp\), %rbx
**	.cfi_restore 3
**	movq	8\(%rsp\), %rbp
**	.cfi_restore 6
**	movq	16\(%rsp\), %r14
**	.cfi_restore 14
**	movq	24\(%rsp\), %r15
**	.cfi_restore 15
**	jmp	.L2
**	.p2align 4,,10
**	.p2align 3
**.L3:
**	.cfi_def_cfa_offset 8
**	cmpq	\$4, %rdx
**	jb	.L21
**	movl	\(%rsi\), %edi
**	movl	-4\(%rsi,%rdx\), %ecx
**	movl	%edi, \(%rax\)
**	movl	%ecx, -4\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L21:
**	cmpq	\$1, %rdx
**	ja	.L6
**	jb	.L16
**	movzbl	\(%rsi\), %edx
**	movb	%dl, \(%rdi\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L19:
**	movq	\(%rsi\), %rdi
**	movq	-8\(%rsi,%rdx\), %rcx
**	movq	%rdi, \(%rax\)
**	movq	%rcx, -8\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L20:
**	.cfi_def_cfa_offset 40
**	cmpq	\$32, %rdx
**	jb	.L9
**	movq	%rbx, \(%rsp\)
**	movq	%r14, 16\(%rsp\)
**	.cfi_offset 3, -40
**	.cfi_offset 14, -24
**	movq	\(%rsi\), %rbx
**	movq	8\(%rsi\), %r14
**	movq	16\(%rsi\), %r11
**	movq	24\(%rsi\), %r10
**	movq	-8\(%rsi,%rdx\), %r9
**	movq	-16\(%rsi,%rdx\), %r8
**	movq	-24\(%rsi,%rdx\), %rdi
**	movq	-32\(%rsi,%rdx\), %rcx
**	movq	%rbx, \(%rax\)
**	movq	%r14, 8\(%rax\)
**	movq	%r11, 16\(%rax\)
**	movq	%r10, 24\(%rax\)
**	movq	%r9, -8\(%rax,%rdx\)
**	movq	%r8, -16\(%rax,%rdx\)
**	movq	%rdi, -24\(%rax,%rdx\)
**	movq	%rcx, -32\(%rax,%rdx\)
**	movq	\(%rsp\), %rbx
**	.cfi_restore 3
**	movq	16\(%rsp\), %r14
**	.cfi_restore 14
**.L2:
**	addq	\$32, %rsp
**	.cfi_def_cfa_offset 8
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L6:
**	movzwl	\(%rsi\), %edi
**	movzwl	-2\(%rsi,%rdx\), %ecx
**	movw	%di, \(%rax\)
**	movw	%cx, -2\(%rax,%rdx\)
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L16:
**	ret
**	.p2align 4,,10
**	.p2align 3
**.L9:
**	.cfi_def_cfa_offset 40
**	movq	\(%rsi\), %r9
**	movq	8\(%rsi\), %r8
**	movq	-8\(%rsi,%rdx\), %rdi
**	movq	-16\(%rsi,%rdx\), %rcx
**	movq	%r9, \(%rax\)
**	movq	%r8, 8\(%rax\)
**	movq	%rdi, -8\(%rax,%rdx\)
**	movq	%rcx, -16\(%rax,%rdx\)
**	jmp	.L2
**	.p2align 4,,10
**	.p2align 3
**.L10:
**	movq	%rbx, \(%rsp\)
**	leaq	\(%rdi,%rdx\), %rdi
**	movq	%r14, 16\(%rsp\)
**	movq	%r15, 24\(%rsp\)
**	.cfi_offset 3, -40
**	.cfi_offset 14, -24
**	.cfi_offset 15, -16
**	movq	\(%rcx\), %r14
**	movq	8\(%rcx\), %r15
**	movq	16\(%rcx\), %r10
**	movq	24\(%rcx\), %r11
**	addq	%rdx, %rcx
**.L12:
**	movq	-16\(%rcx\), %r9
**	movq	-24\(%rcx\), %r8
**	subq	\$32, %rsi
**	subq	\$32, %rdi
**	movq	-32\(%rcx\), %rdx
**	movq	-8\(%rcx\), %rbx
**	subq	\$32, %rcx
**	movq	%r9, 16\(%rdi\)
**	movq	%rbx, 24\(%rdi\)
**	movq	%r8, 8\(%rdi\)
**	movq	%rdx, \(%rdi\)
**	cmpq	\$32, %rsi
**	ja	.L12
**	movq	%r14, \(%rax\)
**	movq	\(%rsp\), %rbx
**	.cfi_restore 3
**	movq	%r15, 8\(%rax\)
**	movq	16\(%rsp\), %r14
**	.cfi_restore 14
**	movq	%r10, 16\(%rax\)
**	movq	24\(%rsp\), %r15
**	.cfi_restore 15
**	movq	%r11, 24\(%rax\)
**	jmp	.L2
**	.cfi_endproc
**...
*/

#define gcc_memmove gcc_memmove_gpr
#include "builtin-memmove-2a.c"
