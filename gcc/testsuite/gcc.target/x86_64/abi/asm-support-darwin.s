	.file	"snapshot.S"
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
	movdqu	%xmm0, _xmm_regs+0(%rip)
	movdqu	%xmm1, _xmm_regs+16(%rip)
	movdqu	%xmm2, _xmm_regs+16*2(%rip)
	movdqu	%xmm3, _xmm_regs+16*3(%rip)
	movdqu	%xmm4, _xmm_regs+16*4(%rip)
	movdqu	%xmm5, _xmm_regs+16*5(%rip)
	movdqu	%xmm6, _xmm_regs+16*6(%rip)
	movdqu	%xmm7, _xmm_regs+16*7(%rip)
	movdqu	%xmm8, _xmm_regs+16*8(%rip)
	movdqu	%xmm9, _xmm_regs+16*9(%rip)
	movdqu	%xmm10, _xmm_regs+16*10(%rip)
	movdqu	%xmm11, _xmm_regs+16*11(%rip)
	movdqu	%xmm12, _xmm_regs+16*12(%rip)
	movdqu	%xmm13, _xmm_regs+16*13(%rip)
	movdqu	%xmm14, _xmm_regs+16*14(%rip)
	movdqu	%xmm15, _xmm_regs+16*15(%rip)
	jmp	*_callthis(%rip)
.LFE3:
	.p2align 4,,15
.globl _snapshot_ret
_snapshot_ret:
	movq	%rdi, _rdi(%rip)
	call	*_callthis(%rip)
	movq	%rax, _rax(%rip)
	movq	%rdx, _rdx(%rip)
	movdqu	%xmm0, _xmm_regs+0(%rip)
	movdqu	%xmm1, _xmm_regs+16(%rip)
	fstpt	_x87_regs(%rip)
	fstpt	_x87_regs+16(%rip)
	fldt	_x87_regs+16(%rip)
	fldt	_x87_regs(%rip)
	ret

	.comm	_callthis,8
	.comm	_rax,8
	.comm	_rbx,8
	.comm	_rcx,8
	.comm	_rdx,8
	.comm	_rsi,8
	.comm	_rdi,8
	.comm	_rsp,8
	.comm	_rbp,8
	.comm	_r8,8
	.comm	_r9,8
	.comm	_r10,8
	.comm	_r11,8
	.comm	_r12,8
	.comm	_r13,8
	.comm	_r14,8
	.comm	_r15,8
	.comm	_xmm_regs,256
	.comm	_x87_regs,128
	.comm   _volatile_var,8
