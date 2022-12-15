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
	movdqu	%xmm0, _xmm_regs+0(%rip)
	movdqu	%xmm1, _xmm_regs+16(%rip)
	movdqu	%xmm2, _xmm_regs+32(%rip)
	movdqu	%xmm3, _xmm_regs+48(%rip)
	movdqu	%xmm4, _xmm_regs+64(%rip)
	movdqu	%xmm5, _xmm_regs+80(%rip)
	movdqu	%xmm6, _xmm_regs+96(%rip)
	movdqu	%xmm7, _xmm_regs+112(%rip)
	movdqu	%xmm8, _xmm_regs+128(%rip)
	movdqu	%xmm9, _xmm_regs+144(%rip)
	movdqu	%xmm10, _xmm_regs+160(%rip)
	movdqu	%xmm11, _xmm_regs+176(%rip)
	movdqu	%xmm12, _xmm_regs+192(%rip)
	movdqu	%xmm13, _xmm_regs+208(%rip)
	movdqu	%xmm14, _xmm_regs+224(%rip)
	movdqu	%xmm15, _xmm_regs+240(%rip)
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
	movdqu	%xmm0, _xmm_regs+0(%rip)
	movdqu	%xmm1, _xmm_regs+16(%rip)
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
	.globl	_xmm_regs
	.zerofill __DATA,__bss,_xmm_regs,256,5
	.globl	_x87_regs
	.zerofill __DATA,__bss,_x87_regs,128,5
	.globl	_volatile_var
	.zerofill __DATA,__bss,_volatile_var,8,3
