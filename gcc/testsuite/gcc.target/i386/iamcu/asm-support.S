	.comm	callthis,4,4
	.comm	eax,4,4
	.comm	ebx,4,4
	.comm	ecx,4,4
	.comm	edx,4,4
	.comm	esi,4,4
	.comm	edi,4,4
	.comm	esp,4,4
	.comm	ebp,4,4
	.comm	sret_eax,4,4
	.comm   volatile_var,4,4

	.text
	.p2align 4,,15
.globl snapshot
	.type	snapshot, @function
snapshot:
	movl	%eax, eax
	movl	%ebx, ebx
	movl	%ecx, ecx
	movl	%edx, edx
	movl	%edi, edi
	movl	%esi, esi
	movl	%ebp, ebp
	movl	%esp, esp
	jmp	*callthis
	.size	snapshot, .-snapshot

	.p2align 4,,15
.globl snapshot_ret
	.type	snapshot_ret, @function
snapshot_ret:
	movl	%eax, sret_eax
	call	*callthis
	movl	%eax, eax
	movl	%edx, edx
	ret
	.size	snapshot_ret, .-snapshot_ret

	.p2align 4,,15
	.globl	__nesf2
	.type	__nesf2, @function
__nesf2:
	.cfi_startproc
	subl	$4, %esp
	.cfi_def_cfa_offset 8
	movl	%eax, (%esp)
	movl	$1, %eax
	fildl	(%esp)
	movl	%edx, (%esp)
	xorl	%edx, %edx
	fildl	(%esp)
	fucomip	%st(1), %st
	fstp	%st(0)
	setp	%dl
	cmove	%edx, %eax
	addl	$4, %esp
	.cfi_def_cfa_offset 4
	ret
	.cfi_endproc
	.size	__nesf2, .-__nesf2

	.p2align 4,,15
	.globl	__nedf2
	.type	__nedf2, @function
__nedf2:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	andl	$-8, %esp
	subl	$8, %esp
	movl	%eax, (%esp)
	movl	$1, %eax
	movl	%edx, 4(%esp)
	xorl	%edx, %edx
	fildq	(%esp)
	fildq	8(%ebp)
	fucomip	%st(1), %st
	fstp	%st(0)
	setp	%dl
	cmove	%edx, %eax
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
	.size	__nedf2, .-__nedf2

	.p2align 4,,15
	.globl	__addsf3
	.type	__addsf3, @function
__addsf3:
	.cfi_startproc
	subl	$4, %esp
	.cfi_def_cfa_offset 8
	movl	%eax, (%esp)
	flds	(%esp)
	movl	%edx, (%esp)
	flds	(%esp)
	faddp	%st, %st(1)
	fstps	(%esp)
	movl	(%esp), %eax
	addl	$4, %esp
	.cfi_def_cfa_offset 4
	ret
	.cfi_endproc
	.size	__addsf3, .-__addsf3

	.p2align 4,,15
	.globl	__adddf3
	.type	__adddf3, @function
__adddf3:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	andl	$-8, %esp
	subl	$8, %esp
	movl	%eax, (%esp)
	movl	%edx, 4(%esp)
	fldl	(%esp)
	faddl	8(%ebp)
	fstpl	(%esp)
	movl	(%esp), %eax
	movl	4(%esp), %edx
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
	.size	__adddf3, .-__adddf3

	.p2align 4,,15
	.globl	__floatsisf
	.type	__floatsisf, @function
__floatsisf:
	.cfi_startproc
	subl	$4, %esp
	.cfi_def_cfa_offset 8
	movl	%eax, (%esp)
	fildl	(%esp)
	addl	$4, %esp
	.cfi_def_cfa_offset 4
	ret
	.cfi_endproc
	.size	__floatsisf, .-__floatsisf

	.p2align 4,,15
	.globl	__floatunsisf
	.type	__floatunsisf, @function
__floatunsisf:
	.cfi_startproc
	subl	$8, %esp
	.cfi_def_cfa_offset 12
	xorl	%edx, %edx
	movl	%eax, (%esp)
	movl	%edx, 4(%esp)
	fildq	(%esp)
	addl	$8, %esp
	.cfi_def_cfa_offset 4
	ret
	.cfi_endproc
	.size	__floatunsisf, .-__floatunsisf

	.globl	__extendsfdf2
	.type	__extendsfdf2, @function
__extendsfdf2:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	andl	$-8, %esp
	subl	$8, %esp
	movl	%eax, (%esp)
	flds	(%esp)
	fstpl	(%esp)
	movl	(%esp), %eax
	movl	4(%esp), %edx
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
	.size	__extendsfdf2, .-__extendsfdf2

	.p2align 4,,15
	.globl	__truncdfsf2
	.type	__truncdfsf2, @function
__truncdfsf2:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	andl	$-8, %esp
	subl	$12, %esp
	movl	%eax, (%esp)
	movl	%edx, 4(%esp)
	fldl	(%esp)
	fstps	(%esp)
	movl	(%esp), %eax
	leave
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
	.cfi_endproc
	.size	__truncdfsf2, .-__truncdfsf2

	.p2align 4,,15
	.globl	iamcu_memset
	.type	iamcu_memset, @function
iamcu_memset:
	.cfi_startproc
	pushl	%edi
	.cfi_adjust_cfa_offset 4
	.cfi_rel_offset %edi, 0
	movl	%eax, %edi
	movzbl	%dl, %eax
	movl	%edi, %edx
	rep stosb
	movl	%edx, %eax
	popl	%edi
	.cfi_adjust_cfa_offset -4
	.cfi_restore %edi
	ret
	.cfi_endproc
	.size	iamcu_memset, .-iamcu_memset

	.p2align 4,,15
	.globl	iamcu_noprintf
	.type	iamcu_noprintf, @function
iamcu_noprintf:
	.cfi_startproc
	pushl	%ebp
	.cfi_def_cfa_offset 8
	.cfi_offset 5, -8
	movl	%esp, %ebp
	.cfi_def_cfa_register 5
	cmpl	$-1414676753, 8(%ebp)
	fldl	16(%ebp)
	fldl	28(%ebp)
	jne	7f
	cmpl	$256, 12(%ebp)
	jne	8f
	flds	.LCiamcu_noprintf0
	movl	$1, %eax
	fucomip	%st(2), %st
	fstp	%st(1)
	setp	%dl
	cmovne	%eax, %edx
	testb	%dl, %dl
	jne	9f
	cmpl	$-1146241297, 24(%ebp)
	jne	10f
	flds	.LCiamcu_noprintf1
	fucomip	%st(1), %st
	fstp	%st(0)
	setp	%dl
	cmove	%edx, %eax
	testb	%al, %al
	jne	2f
	cmpl	$259, 36(%ebp)
	jne	2f
	popl	%ebp
	.cfi_remember_state
	.cfi_restore 5
	.cfi_def_cfa 4, 4
	ret
7:
	.cfi_restore_state
	fstp	%st(0)
	fstp	%st(0)
	jmp	2f
8:
	fstp	%st(0)
	fstp	%st(0)
	.p2align 4,,3
	jmp	2f
9:
	fstp	%st(0)
	jmp	2f
10:
	fstp	%st(0)
2:
	call	abort
	.cfi_endproc
	.size	iamcu_noprintf, .-iamcu_noprintf
	.section	.rodata.cst4,"aM",@progbits,4
	.align 4
.LCiamcu_noprintf0:
	.long	1132494848
	.align 4
.LCiamcu_noprintf1:
	.long	1132527616
