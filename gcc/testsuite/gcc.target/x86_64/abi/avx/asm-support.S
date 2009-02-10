	.file	"snapshot.S"
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
	vmovdqu	%ymm2, ymm_regs+32*2(%rip)
	vmovdqu	%ymm3, ymm_regs+32*3(%rip)
	vmovdqu	%ymm4, ymm_regs+32*4(%rip)
	vmovdqu	%ymm5, ymm_regs+32*5(%rip)
	vmovdqu	%ymm6, ymm_regs+32*6(%rip)
	vmovdqu	%ymm7, ymm_regs+32*7(%rip)
	vmovdqu	%ymm8, ymm_regs+32*8(%rip)
	vmovdqu	%ymm9, ymm_regs+32*9(%rip)
	vmovdqu	%ymm10, ymm_regs+32*10(%rip)
	vmovdqu	%ymm11, ymm_regs+32*11(%rip)
	vmovdqu	%ymm12, ymm_regs+32*12(%rip)
	vmovdqu	%ymm13, ymm_regs+32*13(%rip)
	vmovdqu	%ymm14, ymm_regs+32*14(%rip)
	vmovdqu	%ymm15, ymm_regs+32*15(%rip)
	jmp	*callthis(%rip)
.LFE3:
	.size	snapshot, .-snapshot

	.p2align 4,,15
.globl snapshot_ret
	.type	snapshot_ret, @function
snapshot_ret:
	movq	%rdi, rdi(%rip)
	call	*callthis(%rip)
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
