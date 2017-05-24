/* Assembly proxy functions for ms_abi tests.
   Copyright (C) 2016-2017 Free Software Foundation, Inc.
   Contributed by Daniel Santos <daniel.santos@pobox.com>

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

Under Section 7 of GPL version 3, you are granted additional
permissions described in the GCC Runtime Library Exception, version
3.1, as published by the Free Software Foundation.

You should have received a copy of the GNU General Public License and
a copy of the GCC Runtime Library Exception along with this program;
see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see
<http://www.gnu.org/licenses/>.  */

#ifdef __x86_64__

# ifdef __ELF__
#  define ELFFN_BEGIN(fn)       .type fn,@function
#  define ELFFN_END(fn)         .size fn,.-fn
# else
#  define ELFFN_BEGIN(fn)
#  define ELFFN_END(fn)
# endif

# define FUNC(fn)		\
	.global fn;		\
	ELFFN_BEGIN(fn);	\
fn:

#define FUNC_END(fn) ELFFN_END(fn)

# ifdef __AVX__
#  define MOVAPS vmovaps
# else
#  define MOVAPS movaps
# endif

/* TODO: Is there a cleaner way to provide these offsets?  */
	.struct 0
test_data_save:

	.struct test_data_save + 224
test_data_input:

	.struct test_data_save + 448
test_data_output:

	.struct test_data_save + 672
test_data_fn:

	.struct test_data_save + 680
test_data_retaddr:

	.text

regs_to_mem:
	MOVAPS	%xmm6, (%rax)
	MOVAPS	%xmm7, 0x10(%rax)
	MOVAPS	%xmm8, 0x20(%rax)
	MOVAPS	%xmm9, 0x30(%rax)
	MOVAPS	%xmm10, 0x40(%rax)
	MOVAPS	%xmm11, 0x50(%rax)
	MOVAPS	%xmm12, 0x60(%rax)
	MOVAPS	%xmm13, 0x70(%rax)
	MOVAPS	%xmm14, 0x80(%rax)
	MOVAPS	%xmm15, 0x90(%rax)
	mov	%rsi, 0xa0(%rax)
	mov	%rdi, 0xa8(%rax)
	mov	%rbx, 0xb0(%rax)
	mov	%rbp, 0xb8(%rax)
	mov	%r12, 0xc0(%rax)
	mov	%r13, 0xc8(%rax)
	mov	%r14, 0xd0(%rax)
	mov	%r15, 0xd8(%rax)
	retq

mem_to_regs:
	MOVAPS	(%rax), %xmm6
	MOVAPS	0x10(%rax),%xmm7
	MOVAPS	0x20(%rax),%xmm8
	MOVAPS	0x30(%rax),%xmm9
	MOVAPS	0x40(%rax),%xmm10
	MOVAPS	0x50(%rax),%xmm11
	MOVAPS	0x60(%rax),%xmm12
	MOVAPS	0x70(%rax),%xmm13
	MOVAPS	0x80(%rax),%xmm14
	MOVAPS	0x90(%rax),%xmm15
	mov	0xa0(%rax),%rsi
	mov	0xa8(%rax),%rdi
	mov	0xb0(%rax),%rbx
	mov	0xb8(%rax),%rbp
	mov	0xc0(%rax),%r12
	mov	0xc8(%rax),%r13
	mov	0xd0(%rax),%r14
	mov	0xd8(%rax),%r15
	retq

# NOTE: Not MT safe
FUNC(do_test_unaligned)
	.cfi_startproc
	# The below alignment checks are to verify correctness of the test
	# its self.

	# Verify that incoming stack is aligned + 8
	pushf
	test	$0x8, %rsp
	jne	L0
	int	$3		# Stack not unaligned

FUNC(do_test_aligned)
	# Verify that incoming stack is aligned
	pushf
	test	$0xf, %rsp
	je	L0
	int	$3		# Stack not aligned
L0:
	popf

	# Save registers
	lea	test_data(%rip), %rax
	call	regs_to_mem

	# Load register with random data
	lea	test_data + test_data_input(%rip), %rax
	call	mem_to_regs

	# Save original return address
	pop	%rax
	movq    %rax, test_data + test_data_retaddr(%rip)

	# Call the test function
	call	*test_data + test_data_fn(%rip)

	# Restore the original return address
	movq    test_data + test_data_retaddr(%rip), %rcx
	push	%rcx

	# Save test function return value and store resulting register values
	push	%rax
	lea	test_data + test_data_output(%rip), %rax
	call	regs_to_mem

	# Restore registers
	lea	test_data(%rip), %rax
	call	mem_to_regs
	pop	%rax
	retq
        .cfi_endproc
FUNC_END(do_test_aligned)
FUNC_END(do_test_unaligned)

#endif /* __x86_64__ */
