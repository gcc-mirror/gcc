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
	vmovdqu32 %zmm0, zmm_regs+0(%rip)
	vmovdqu32 %zmm1, zmm_regs+64(%rip)
	vmovdqu32 %zmm2, zmm_regs+128(%rip)
	vmovdqu32 %zmm3, zmm_regs+192(%rip)
	vmovdqu32 %zmm4, zmm_regs+256(%rip)
	vmovdqu32 %zmm5, zmm_regs+320(%rip)
	vmovdqu32 %zmm6, zmm_regs+384(%rip)
	vmovdqu32 %zmm7, zmm_regs+448(%rip)
	vmovdqu32 %zmm8, zmm_regs+512(%rip)
	vmovdqu32 %zmm9, zmm_regs+576(%rip)
	vmovdqu32 %zmm10, zmm_regs+640(%rip)
	vmovdqu32 %zmm11, zmm_regs+704(%rip)
	vmovdqu32 %zmm12, zmm_regs+768(%rip)
	vmovdqu32 %zmm13, zmm_regs+832(%rip)
	vmovdqu32 %zmm14, zmm_regs+896(%rip)
	vmovdqu32 %zmm15, zmm_regs+960(%rip)
	vmovdqu32 %zmm16, zmm_regs+1024(%rip)
	vmovdqu32 %zmm17, zmm_regs+1088(%rip)
	vmovdqu32 %zmm18, zmm_regs+1152(%rip)
	vmovdqu32 %zmm19, zmm_regs+1216(%rip)
	vmovdqu32 %zmm20, zmm_regs+1280(%rip)
	vmovdqu32 %zmm21, zmm_regs+1344(%rip)
	vmovdqu32 %zmm22, zmm_regs+1408(%rip)
	vmovdqu32 %zmm23, zmm_regs+1472(%rip)
	vmovdqu32 %zmm24, zmm_regs+1536(%rip)
	vmovdqu32 %zmm25, zmm_regs+1600(%rip)
	vmovdqu32 %zmm26, zmm_regs+1664(%rip)
	vmovdqu32 %zmm27, zmm_regs+1728(%rip)
	vmovdqu32 %zmm28, zmm_regs+1792(%rip)
	vmovdqu32 %zmm29, zmm_regs+1856(%rip)
	vmovdqu32 %zmm30, zmm_regs+1920(%rip)
	vmovdqu32 %zmm31, zmm_regs+1984(%rip)
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
	vmovdqu32	%zmm0, zmm_regs+0(%rip)
	vmovdqu32	%zmm1, zmm_regs+64(%rip)
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
	.comm	zmm_regs,2048,64
	.comm	x87_regs,128,32
	.comm   volatile_var,8,8
