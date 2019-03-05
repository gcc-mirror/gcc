	.text
	.p2align 4,,15
	.globl _snapshot
_snapshot:
LFB3:
	movq	%rax, _rax(%rip)
	movq	%rbx, _rbx(%rip)
	movq	%rcx, _rcx(%rip)
	movq	%rdx, _rdx(%rip)
	movq	%rdi, _rdi(%rip)
	movq	%rsi, _rsi(%rip)
	movq	%rbp, _rbp(%rip)
	movq	%rsp, _rsp(%rip)
	movq	%r8, _r8(%rip)
	movq	%r9, _r9(%rip)
	movq	%r10, _r10(%rip)
	movq	%r11, _r11(%rip)
	movq	%r12, _r12(%rip)
	movq	%r13, _r13(%rip)
	movq	%r14, _r14(%rip)
	movq	%r15, _r15(%rip)
	vmovdqu	%ymm0, _ymm_regs+0(%rip)
	vmovdqu	%ymm1, _ymm_regs+32(%rip)
	vmovdqu	%ymm2, _ymm_regs+64(%rip)
	vmovdqu	%ymm3, _ymm_regs+96(%rip)
	vmovdqu	%ymm4, _ymm_regs+128(%rip)
	vmovdqu	%ymm5, _ymm_regs+160(%rip)
	vmovdqu	%ymm6, _ymm_regs+192(%rip)
	vmovdqu	%ymm7, _ymm_regs+224(%rip)
	vmovdqu	%ymm8, _ymm_regs+256(%rip)
	vmovdqu	%ymm9, _ymm_regs+288(%rip)
	vmovdqu	%ymm10, _ymm_regs+320(%rip)
	vmovdqu	%ymm11, _ymm_regs+352(%rip)
	vmovdqu	%ymm12, _ymm_regs+384(%rip)
	vmovdqu	%ymm13, _ymm_regs+416(%rip)
	vmovdqu	%ymm14, _ymm_regs+448(%rip)
	vmovdqu	%ymm15, _ymm_regs+480(%rip)
	jmp	*_callthis(%rip)
LFE3:
	.p2align 4,,15
	.globl _snapshot_ret
_snapshot_ret:
	movq	%rdi, _rdi(%rip)
	subq	$8, %rsp
	call	*_callthis(%rip)
	addq	$8, %rsp
	movq	%rax, _rax(%rip)
	movq	%rdx, _rdx(%rip)
	vmovdqu	%ymm0, _ymm_regs+0(%rip)
	vmovdqu	%ymm1, _ymm_regs+32(%rip)
	fstpt	_x87_regs(%rip)
	fstpt	_x87_regs+16(%rip)
	fldt	_x87_regs+16(%rip)
	fldt	_x87_regs(%rip)
	ret

	.comm	_callthis,8,3
	.comm	_rax,8,3
	.comm	_rbx,8,3
	.comm	_rcx,8,3
	.comm	_rdx,8,3
	.comm	_rsi,8,3
	.comm	_rdi,8,3
	.comm	_rsp,8,3
	.comm	_rbp,8,3
	.comm	_r8,8,3
	.comm	_r9,8,3
	.comm	_r10,8,3
	.comm	_r11,8,3
	.comm	_r12,8,3
	.comm	_r13,8,3
	.comm	_r14,8,3
	.comm	_r15,8,3
	.comm	_ymm_regs,512,5
	.comm	_x87_regs,128,5
	.comm   _volatile_var,8,3
