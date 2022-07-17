	.text
	.p2align 4,,15
.globl snapshot
	.type	snapshot, @function
snapshot:
.LFB3:
	movq	%rax, rax(%rip)
	movq	%rbx, rbx(%rip)
	movq	%rcx, rcx(%rip)
	movq	%rdx, rdx(%rip)
	movq	%rdi, rdi(%rip)
	movq	%rsi, rsi(%rip)
	movq	%rbp, rbp(%rip)
	movq	%rsp, rsp(%rip)
	movq	%r8, r8(%rip)
	movq	%r9, r9(%rip)
	movq	%r10, r10(%rip)
	movq	%r11, r11(%rip)
	movq	%r12, r12(%rip)
	movq	%r13, r13(%rip)
	movq	%r14, r14(%rip)
	movq	%r15, r15(%rip)
	vmovdqu	%ymm0, ymm_regs+0(%rip)
	vmovdqu	%ymm1, ymm_regs+32(%rip)
	vmovdqu	%ymm2, ymm_regs+64(%rip)
	vmovdqu	%ymm3, ymm_regs+96(%rip)
	vmovdqu	%ymm4, ymm_regs+128(%rip)
	vmovdqu	%ymm5, ymm_regs+160(%rip)
	vmovdqu	%ymm6, ymm_regs+192(%rip)
	vmovdqu	%ymm7, ymm_regs+224(%rip)
	vmovdqu	%ymm8, ymm_regs+256(%rip)
	vmovdqu	%ymm9, ymm_regs+288(%rip)
	vmovdqu	%ymm10, ymm_regs+320(%rip)
	vmovdqu	%ymm11, ymm_regs+352(%rip)
	vmovdqu	%ymm12, ymm_regs+384(%rip)
	vmovdqu	%ymm13, ymm_regs+416(%rip)
	vmovdqu	%ymm14, ymm_regs+448(%rip)
	vmovdqu	%ymm15, ymm_regs+480(%rip)
	jmp	*callthis(%rip)
.LFE3:
	.size	snapshot, .-snapshot

	.p2align 4,,15
.globl snapshot_ret
	.type	snapshot_ret, @function
snapshot_ret:
	movq	%rdi, rdi(%rip)
	subq	$8, %rsp
	call	*callthis(%rip)
	addq	$8, %rsp
	movq	%rax, rax(%rip)
	movq	%rdx, rdx(%rip)
	vmovdqu	%ymm0, ymm_regs+0(%rip)
	vmovdqu	%ymm1, ymm_regs+32(%rip)
	fstpt	x87_regs(%rip)
	fstpt	x87_regs+16(%rip)
	fldt	x87_regs+16(%rip)
	fldt	x87_regs(%rip)
	ret
	.size	snapshot_ret, .-snapshot_ret

	.comm	callthis,8,8
	.comm	rax,8,8
	.comm	rbx,8,8
	.comm	rcx,8,8
	.comm	rdx,8,8
	.comm	rsi,8,8
	.comm	rdi,8,8
	.comm	rsp,8,8
	.comm	rbp,8,8
	.comm	r8,8,8
	.comm	r9,8,8
	.comm	r10,8,8
	.comm	r11,8,8
	.comm	r12,8,8
	.comm	r13,8,8
	.comm	r14,8,8
	.comm	r15,8,8
	.comm	ymm_regs,512,32
	.comm	x87_regs,128,32
	.comm   volatile_var,8,8
#ifdef __linux__
	.section	.note.GNU-stack,"",@progbits
#endif
