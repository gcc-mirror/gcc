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
	vmovdqu32 %zmm0, _zmm_regs+0(%rip)
	vmovdqu32 %zmm1, _zmm_regs+64(%rip)
	vmovdqu32 %zmm2, _zmm_regs+128(%rip)
	vmovdqu32 %zmm3, _zmm_regs+192(%rip)
	vmovdqu32 %zmm4, _zmm_regs+256(%rip)
	vmovdqu32 %zmm5, _zmm_regs+320(%rip)
	vmovdqu32 %zmm6, _zmm_regs+384(%rip)
	vmovdqu32 %zmm7, _zmm_regs+448(%rip)
	vmovdqu32 %zmm8, _zmm_regs+512(%rip)
	vmovdqu32 %zmm9, _zmm_regs+576(%rip)
	vmovdqu32 %zmm10, _zmm_regs+640(%rip)
	vmovdqu32 %zmm11, _zmm_regs+704(%rip)
	vmovdqu32 %zmm12, _zmm_regs+768(%rip)
	vmovdqu32 %zmm13, _zmm_regs+832(%rip)
	vmovdqu32 %zmm14, _zmm_regs+896(%rip)
	vmovdqu32 %zmm15, _zmm_regs+960(%rip)
	vmovdqu32 %zmm16, _zmm_regs+1024(%rip)
	vmovdqu32 %zmm17, _zmm_regs+1088(%rip)
	vmovdqu32 %zmm18, _zmm_regs+1152(%rip)
	vmovdqu32 %zmm19, _zmm_regs+1216(%rip)
	vmovdqu32 %zmm20, _zmm_regs+1280(%rip)
	vmovdqu32 %zmm21, _zmm_regs+1344(%rip)
	vmovdqu32 %zmm22, _zmm_regs+1408(%rip)
	vmovdqu32 %zmm23, _zmm_regs+1472(%rip)
	vmovdqu32 %zmm24, _zmm_regs+1536(%rip)
	vmovdqu32 %zmm25, _zmm_regs+1600(%rip)
	vmovdqu32 %zmm26, _zmm_regs+1664(%rip)
	vmovdqu32 %zmm27, _zmm_regs+1728(%rip)
	vmovdqu32 %zmm28, _zmm_regs+1792(%rip)
	vmovdqu32 %zmm29, _zmm_regs+1856(%rip)
	vmovdqu32 %zmm30, _zmm_regs+1920(%rip)
	vmovdqu32 %zmm31, _zmm_regs+1984(%rip)
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
	vmovdqu32	%zmm0, _zmm_regs+0(%rip)
	vmovdqu32	%zmm1, _zmm_regs+64(%rip)
	fstpt	_x87_regs(%rip)
	fstpt	_x87_regs+16(%rip)
	fldt	_x87_regs+16(%rip)
	fldt	_x87_regs(%rip)
	ret

	.comm	_callthis,8,8
	.comm	_rax,8,8
	.comm	_rbx,8,8
	.comm	_rcx,8,8
	.comm	_rdx,8,8
	.comm	_rsi,8,8
	.comm	_rdi,8,8
	.comm	_rsp,8,8
	.comm	_rbp,8,8
	.comm	_r8,8,8
	.comm	_r9,8,8
	.comm	_r10,8,8
	.comm	_r11,8,8
	.comm	_r12,8,8
	.comm	_r13,8,8
	.comm	_r14,8,8
	.comm	_r15,8,8
	.comm	_zmm_regs,2048,64
	.comm	_x87_regs,128,32
	.comm   _volatile_var,8,8
