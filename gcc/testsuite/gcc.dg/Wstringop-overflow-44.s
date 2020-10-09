	.file	"Wstringop-overflow-44.c"
	.text
	.p2align 4
	.globl	f0
	.type	f0, @function
f0:
.LFB0:
	.cfi_startproc
	ret
	.cfi_endproc
.LFE0:
	.size	f0, .-f0
	.p2align 4
	.globl	f1
	.type	f1, @function
f1:
.LFB1:
	.cfi_startproc
	ret
	.cfi_endproc
.LFE1:
	.size	f1, .-f1
	.p2align 4
	.globl	f2
	.type	f2, @function
f2:
.LFB2:
	.cfi_startproc
	movl	n(%rip), %eax
	testl	%eax, %eax
	jle	.L12
.L4:
	ret
	.p2align 4,,10
	.p2align 3
.L12:
	movslq	%eax, %rdx
	movq	d(%rip), %rcx
	testq	%rdx, %rdx
	je	.L4
	xorl	%eax, %eax
.L6:
	movb	$0, (%rcx,%rax)
	addq	$1, %rax
	cmpq	%rdx, %rax
	jb	.L6
	ret
	.cfi_endproc
.LFE2:
	.size	f2, .-f2
	.p2align 4
	.globl	f3
	.type	f3, @function
f3:
.LFB3:
	.cfi_startproc
	movslq	n(%rip), %rdx
	testl	%edx, %edx
	jle	.L15
	ret
	.p2align 4,,10
	.p2align 3
.L15:
	movq	%rdi, %rsi
	movq	d(%rip), %rdi
	jmp	strncpy
	.cfi_endproc
.LFE3:
	.size	f3, .-f3
	.p2align 4
	.globl	f4
	.type	f4, @function
f4:
.LFB4:
	.cfi_startproc
	movl	n(%rip), %eax
	testl	%eax, %eax
	jle	.L18
	ret
	.p2align 4,,10
	.p2align 3
.L18:
	movq	d(%rip), %rax
	movq	%rdi, %rsi
	movb	$0, (%rax)
	movslq	n(%rip), %rdx
	movq	d(%rip), %rdi
	jmp	strncat
	.cfi_endproc
.LFE4:
	.size	f4, .-f4
	.p2align 4
	.globl	g0
	.type	g0, @function
g0:
.LFB5:
	.cfi_startproc
	movl	n(%rip), %eax
	testl	%eax, %eax
	jle	.L25
	ret
	.p2align 4,,10
	.p2align 3
.L25:
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	15(%rsp), %rdi
	call	sink
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE5:
	.size	g0, .-g0
	.p2align 4
	.globl	g1
	.type	g1, @function
g1:
.LFB6:
	.cfi_startproc
	movl	n(%rip), %eax
	testl	%eax, %eax
	jle	.L32
	ret
	.p2align 4,,10
	.p2align 3
.L32:
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	leaq	15(%rsp), %rdi
	call	sink
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE6:
	.size	g1, .-g1
	.p2align 4
	.globl	g2
	.type	g2, @function
g2:
.LFB7:
	.cfi_startproc
	movl	n(%rip), %eax
	testl	%eax, %eax
	jle	.L45
	ret
	.p2align 4,,10
	.p2align 3
.L45:
	movslq	%eax, %rdx
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	testq	%rdx, %rdx
	je	.L36
	xorl	%eax, %eax
.L35:
	movb	$0, 15(%rsp,%rax)
	addq	$1, %rax
	cmpq	%rdx, %rax
	jb	.L35
.L36:
	leaq	15(%rsp), %rdi
	call	sink
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE7:
	.size	g2, .-g2
	.p2align 4
	.globl	g3
	.type	g3, @function
g3:
.LFB8:
	.cfi_startproc
	movslq	n(%rip), %rdx
	testl	%edx, %edx
	jle	.L52
	ret
	.p2align 4,,10
	.p2align 3
.L52:
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, %rsi
	leaq	15(%rsp), %rdi
	call	strncpy
	leaq	15(%rsp), %rdi
	call	sink
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE8:
	.size	g3, .-g3
	.p2align 4
	.globl	g4
	.type	g4, @function
g4:
.LFB9:
	.cfi_startproc
	movslq	n(%rip), %rdx
	testl	%edx, %edx
	jle	.L59
	ret
	.p2align 4,,10
	.p2align 3
.L59:
	subq	$24, %rsp
	.cfi_def_cfa_offset 32
	movq	%rdi, %rsi
	leaq	15(%rsp), %rdi
	movb	$0, 15(%rsp)
	call	strncat
	leaq	15(%rsp), %rdi
	call	sink
	addq	$24, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE9:
	.size	g4, .-g4
	.p2align 4
	.globl	h0
	.type	h0, @function
h0:
.LFB10:
	.cfi_startproc
	movl	n(%rip), %eax
	testl	%eax, %eax
	jle	.L66
	ret
	.p2align 4,,10
	.p2align 3
.L66:
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movl	$1, %edi
	call	malloc
	movq	%rax, d(%rip)
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE10:
	.size	h0, .-h0
	.p2align 4
	.globl	h1
	.type	h1, @function
h1:
.LFB16:
	.cfi_startproc
	movl	n(%rip), %eax
	testl	%eax, %eax
	jle	.L73
	ret
	.p2align 4,,10
	.p2align 3
.L73:
	subq	$8, %rsp
	.cfi_def_cfa_offset 16
	movl	$1, %edi
	call	malloc
	movq	%rax, d(%rip)
	addq	$8, %rsp
	.cfi_def_cfa_offset 8
	ret
	.cfi_endproc
.LFE16:
	.size	h1, .-h1
