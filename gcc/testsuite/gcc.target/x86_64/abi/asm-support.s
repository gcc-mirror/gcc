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
	movdqu	%xmm0, xmm_regs+0(%rip)
	movdqu	%xmm1, xmm_regs+16(%rip)
	movdqu	%xmm2, xmm_regs+16*2(%rip)
	movdqu	%xmm3, xmm_regs+16*3(%rip)
	movdqu	%xmm4, xmm_regs+16*4(%rip)
	movdqu	%xmm5, xmm_regs+16*5(%rip)
	movdqu	%xmm6, xmm_regs+16*6(%rip)
	movdqu	%xmm7, xmm_regs+16*7(%rip)
	movdqu	%xmm8, xmm_regs+16*8(%rip)
	movdqu	%xmm9, xmm_regs+16*9(%rip)
	movdqu	%xmm10, xmm_regs+16*10(%rip)
	movdqu	%xmm11, xmm_regs+16*11(%rip)
	movdqu	%xmm12, xmm_regs+16*12(%rip)
	movdqu	%xmm13, xmm_regs+16*13(%rip)
	movdqu	%xmm14, xmm_regs+16*14(%rip)
	movdqu	%xmm15, xmm_regs+16*15(%rip)
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
	movdqu	%xmm0, xmm_regs+0(%rip)
	movdqu	%xmm1, xmm_regs+16(%rip)
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
	.comm	xmm_regs,256,32
	.comm	x87_regs,128,32
	.comm   volatile_var,8,8
