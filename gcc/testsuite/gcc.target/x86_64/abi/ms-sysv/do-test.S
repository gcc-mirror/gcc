/* Assembly proxy functions for ms_abi tests.
   Copyright (C) 2016-2021 Free Software Foundation, Inc.
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

#if defined(__x86_64__) && defined(__SSE2__)

/* These macros currently support GNU/Linux, Solaris and Darwin.  */

#ifdef __ELF__
# define FN_TYPE(fn) .type fn,@function
# define FN_SIZE(fn) .size fn,.-fn
#else
# define FN_TYPE(fn)
# define FN_SIZE(fn)
#endif

#ifdef __USER_LABEL_PREFIX__
# define ASMNAME2(prefix, name)	prefix ## name
# define ASMNAME1(prefix, name)	ASMNAME2(prefix, name)
# define ASMNAME(name)		ASMNAME1(__USER_LABEL_PREFIX__, name)
#else
# define ASMNAME(name)		name
#endif

#define FUNC_BEGIN(fn)		\
	.globl ASMNAME(fn);	\
	FN_TYPE (ASMNAME(fn));	\
ASMNAME(fn):

#define FUNC_END(fn) FN_SIZE(ASMNAME(fn))

#ifdef __AVX__
# define MOVAPS vmovaps
#else
# define MOVAPS movaps
#endif

	.text

FUNC_BEGIN(regs_to_mem)
	MOVAPS	%xmm6, (%r10)
	MOVAPS	%xmm7, 0x10(%r10)
	MOVAPS	%xmm8, 0x20(%r10)
	MOVAPS	%xmm9, 0x30(%r10)
	MOVAPS	%xmm10, 0x40(%r10)
	MOVAPS	%xmm11, 0x50(%r10)
	MOVAPS	%xmm12, 0x60(%r10)
	MOVAPS	%xmm13, 0x70(%r10)
	MOVAPS	%xmm14, 0x80(%r10)
	MOVAPS	%xmm15, 0x90(%r10)
	mov	%rsi, 0xa0(%r10)
	mov	%rdi, 0xa8(%r10)
	mov	%rbx, 0xb0(%r10)
	mov	%rbp, 0xb8(%r10)
	mov	%r12, 0xc0(%r10)
	mov	%r13, 0xc8(%r10)
	mov	%r14, 0xd0(%r10)
	mov	%r15, 0xd8(%r10)
	retq
FUNC_END(regs_to_mem)

FUNC_BEGIN(mem_to_regs)
	MOVAPS	(%r10), %xmm6
	MOVAPS	0x10(%r10),%xmm7
	MOVAPS	0x20(%r10),%xmm8
	MOVAPS	0x30(%r10),%xmm9
	MOVAPS	0x40(%r10),%xmm10
	MOVAPS	0x50(%r10),%xmm11
	MOVAPS	0x60(%r10),%xmm12
	MOVAPS	0x70(%r10),%xmm13
	MOVAPS	0x80(%r10),%xmm14
	MOVAPS	0x90(%r10),%xmm15
	mov	0xa0(%r10),%rsi
	mov	0xa8(%r10),%rdi
	mov	0xb0(%r10),%rbx
	mov	0xb8(%r10),%rbp
	mov	0xc0(%r10),%r12
	mov	0xc8(%r10),%r13
	mov	0xd0(%r10),%r14
	mov	0xd8(%r10),%r15
	retq
FUNC_END(mem_to_regs)

# NOTE: Not MT safe
FUNC_BEGIN(do_test_unaligned)
	# The below alignment checks are to verify correctness of the test
	# its self.

	# Verify that incoming stack is aligned + 8
	test	$0xf, %rsp
	je	ASMNAME(do_test_body)
	int	$3		# Stack not unaligned
FUNC_END(do_test_unaligned)

FUNC_BEGIN(do_test_aligned)
	# Verify that incoming stack is aligned
	test	$0x8, %rsp
	jne	ASMNAME(do_test_body)
	int	$3		# Stack not aligned
FUNC_END(do_test_aligned)

#endif /* __x86_64__ */
