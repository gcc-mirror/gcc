	.text
	.p2align 4,,15
	.globl _snapshot
_snapshot:
.LFB3:
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
.LFE3:

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

	.globl	_callthis
	.zerofill __DATA,__bss,_callthis,8,3
	.globl	_rax
	.zerofill __DATA,__bss,_rax,8,3
	.globl	_rbx
	.zerofill __DATA,__bss,_rbx,8,3
	.globl	_rcx
	.zerofill __DATA,__bss,_rcx,8,3
	.globl	_rdx
	.zerofill __DATA,__bss,_rdx,8,3
	.globl	_rsi
	.zerofill __DATA,__bss,_rsi,8,3
	.globl	_rdi
	.zerofill __DATA,__bss,_rdi,8,3
	.globl	_rsp
	.zerofill __DATA,__bss,_rsp,8,3
	.globl	_rbp
	.zerofill __DATA,__bss,_rbp,8,3
	.globl	_r8
	.zerofill __DATA,__bss,_r8,8,3
	.globl	_r9
	.zerofill __DATA,__bss,_r9,8,3
	.globl	_r10
	.zerofill __DATA,__bss,_r10,8,3
	.globl	_r11
	.zerofill __DATA,__bss,_r11,8,3
	.globl	_r12
	.zerofill __DATA,__bss,_r12,8,3
	.globl	_r13
	.zerofill __DATA,__bss,_r13,8,3
	.globl	_r14
	.zerofill __DATA,__bss,_r14,8,3
	.globl	_r15
	.zerofill __DATA,__bss,_r15,8,3
	.globl	_ymm_regs
	.zerofill __DATA,__bss,_ymm_regs,512,5
	.globl	_x87_regs
	.zerofill __DATA,__bss,_x87_regs,128,5
	.globl	_volatile_var
	.zerofill __DATA,__bss,_volatile_var,8,3
