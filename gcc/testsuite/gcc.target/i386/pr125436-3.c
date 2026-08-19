/* { dg-do compile { target { *-*-linux* && lp64 } } } */
/* { dg-options "-O1 -fPIC -mtls-dialect=gnu -mtune-ctrl=^prologue_using_move,^epilogue_using_move -mno-avx -mno-push-args -fomit-frame-pointer -mtune=generic" } */
/* Keep labels and directives ('.cfi_startproc', '.cfi_endproc').  */
/* { dg-final { check-function-bodies "**" "" "" { target "*-*-*" } {^\t?\.}  } } */

/*
**ms_tls_access:
**.LFB[0-9]+:
**	.cfi_startproc
**	pushq	%rdi
**	.cfi_def_cfa_offset 16
**	.cfi_offset 5, -16
**	pushq	%rsi
**	.cfi_def_cfa_offset 24
**	.cfi_offset 4, -24
**	pushq	%rbx
**	.cfi_def_cfa_offset 32
**	.cfi_offset 3, -32
**	subq	\$160, %rsp
**	.cfi_def_cfa_offset 192
**	movaps	%xmm6, \(%rsp\)
**	movaps	%xmm7, 16\(%rsp\)
**	movaps	%xmm8, 32\(%rsp\)
**	movaps	%xmm9, 48\(%rsp\)
**	movaps	%xmm10, 64\(%rsp\)
**	movaps	%xmm11, 80\(%rsp\)
**	movaps	%xmm12, 96\(%rsp\)
**	movaps	%xmm13, 112\(%rsp\)
**	movaps	%xmm14, 128\(%rsp\)
**	movaps	%xmm15, 144\(%rsp\)
**	.cfi_offset 23, -192
**	.cfi_offset 24, -176
**	.cfi_offset 25, -160
**	.cfi_offset 26, -144
**	.cfi_offset 27, -128
**	.cfi_offset 28, -112
**	.cfi_offset 29, -96
**	.cfi_offset 30, -80
**	.cfi_offset 31, -64
**	.cfi_offset 32, -48
**	movl	%ecx, %ebx
**	leaq	tls_instance@tlsld\(%rip\), %rdi
**	call	__tls_get_addr@PLT
**	addl	%ebx, tls_instance@dtpoff\(%rax\)
**	movaps	\(%rsp\), %xmm6
**	movaps	16\(%rsp\), %xmm7
**	movaps	32\(%rsp\), %xmm8
**	movaps	48\(%rsp\), %xmm9
**	movaps	64\(%rsp\), %xmm10
**	movaps	80\(%rsp\), %xmm11
**	movaps	96\(%rsp\), %xmm12
**	movaps	112\(%rsp\), %xmm13
**	movaps	128\(%rsp\), %xmm14
**	movaps	144\(%rsp\), %xmm15
**	addq	\$160, %rsp
**	.cfi_restore 32
**	.cfi_restore 31
**	.cfi_restore 30
**	.cfi_restore 29
**	.cfi_restore 28
**	.cfi_restore 27
**	.cfi_restore 26
**	.cfi_restore 25
**	.cfi_restore 24
**	.cfi_restore 23
**	.cfi_def_cfa_offset 32
**	popq	%rbx
**	.cfi_restore 3
**	.cfi_def_cfa_offset 24
**	popq	%rsi
**	.cfi_restore 4
**	.cfi_def_cfa_offset 16
**	popq	%rdi
**	.cfi_restore 5
**	.cfi_def_cfa_offset 8
**	ret
**	.cfi_endproc
**...
*/

static __thread int tls_instance = 0;

__attribute__ ((ms_abi))
void
ms_tls_access (int i)
{
  tls_instance += i;
}
