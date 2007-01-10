/* Copyright (C) 2006, 2007 Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 2 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with this file; see the file COPYING.  If not, write to the Free
   Software Foundation, 51 Franklin Street, Fifth Floor, Boston, MA
   02110-1301, USA.  */

#ifndef _SPU_PROTOS_
#define _SPU_PROTOS_

#include "rtl.h"

extern enum machine_mode spu_eh_return_filter_mode (void);
extern void spu_cpu_cpp_builtins (struct cpp_reader * pfile);
extern void builtin_define_std (const char *);
extern void spu_override_options (void);
extern void spu_c_common_override_options (void);
extern int valid_subreg (rtx op);
extern void spu_expand_extv (rtx * ops, int unsignedp);
extern void spu_expand_insv (rtx * ops);
extern int spu_expand_block_move (rtx * ops);
extern void spu_emit_branch_or_set (int is_set, enum rtx_code code,
				    rtx * operands);
extern HOST_WIDE_INT const_double_to_hwint (rtx x);
extern rtx hwint_to_const_double (enum machine_mode mode, HOST_WIDE_INT v);
extern void print_operand_address (FILE * file, register rtx addr);
extern void print_operand (FILE * file, rtx x, int code);
extern int spu_split_immediate (rtx * ops);
extern int spu_saved_regs_size (void);
extern int direct_return (void);
extern void spu_expand_prologue (void);
extern void spu_expand_epilogue (unsigned char sibcall_p);
extern rtx spu_return_addr (int count, rtx frame);
extern rtx spu_const (enum machine_mode mode, HOST_WIDE_INT val);
extern struct rtx_def *spu_float_const (const char *string,
					enum machine_mode mode);
extern int immediate_load_p (rtx op, enum machine_mode mode);
extern int logical_immediate_p (rtx op, enum machine_mode mode);
extern int iohl_immediate_p (rtx op, enum machine_mode mode);
extern int arith_immediate_p (rtx op, enum machine_mode mode,
			      HOST_WIDE_INT low, HOST_WIDE_INT high);
extern int legitimate_const (rtx x, int aligned);
extern int spu_constant_address_p (rtx x);
extern int spu_legitimate_constant_p (rtx x);
extern int spu_legitimate_address (enum machine_mode mode, rtx x,
				   int reg_ok_strict);
extern rtx spu_legitimize_address (rtx x, rtx oldx, enum machine_mode mode);
extern int spu_initial_elimination_offset (int from, int to);
extern rtx spu_function_value (tree type, tree func);
extern rtx spu_function_arg (int cum, enum machine_mode mode, tree type,
			     int named);
extern void spu_va_start (tree valist, rtx nextarg);
extern void spu_setup_incoming_varargs (int *cum, enum machine_mode mode,
					tree type, int *pretend_size,
					int no_rtl);
extern void spu_conditional_register_usage (void);
extern int aligned_mem_p (rtx mem);
extern int spu_expand_mov (rtx * ops, enum machine_mode mode);
extern void spu_split_load (rtx * ops);
extern void spu_split_store (rtx * ops);
extern int spu_valid_move (rtx * ops);
extern int fsmbi_const_p (rtx x);
extern int cpat_const_p (rtx x, enum machine_mode mode);
extern rtx gen_cpat_const (rtx * ops);
extern void constant_to_array (enum machine_mode mode, rtx x,
			       unsigned char *arr);
extern rtx array_to_constant (enum machine_mode mode, unsigned char *arr);
extern enum machine_mode spu_eh_return_filter_mode (void);
extern void spu_allocate_stack (rtx op0, rtx op1);
extern void spu_restore_stack_nonlocal (rtx op0, rtx op1);
extern void spu_restore_stack_block (rtx op0, rtx op1);
extern rtx spu_gen_subreg (enum machine_mode mode, rtx x);
extern int spu_safe_dma(HOST_WIDE_INT channel);
extern void spu_builtin_splats (rtx ops[]);
extern void spu_builtin_extract (rtx ops[]);
extern void spu_builtin_insert (rtx ops[]);
extern void spu_builtin_promote (rtx ops[]);
extern void spu_initialize_trampoline (rtx tramp, rtx fnaddr, rtx cxt);
extern void spu_expand_sign_extend (rtx ops[]);
extern void spu_expand_vector_init (rtx target, rtx vals);

/* spu-c.c */
extern tree spu_resolve_overloaded_builtin (tree fndecl, tree fnargs);
extern rtx spu_expand_builtin (tree exp, rtx target, rtx subtarget,
			       enum machine_mode mode, int ignore);
extern rtx spu_expand_builtin (tree, rtx, rtx, enum machine_mode, int);

#endif
