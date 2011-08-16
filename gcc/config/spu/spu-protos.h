/* Copyright (C) 2006, 2007, 2008, 2009, 2010, 2011
   Free Software Foundation, Inc.

   This file is free software; you can redistribute it and/or modify it under
   the terms of the GNU General Public License as published by the Free
   Software Foundation; either version 3 of the License, or (at your option) 
   any later version.

   This file is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
   FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
   for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef _SPU_PROTOS_
#define _SPU_PROTOS_

extern void spu_cpu_cpp_builtins (struct cpp_reader * pfile);
extern void builtin_define_std (const char *);
extern void spu_c_common_override_options (void);
extern int valid_subreg (rtx op);
extern void spu_expand_extv (rtx * ops, int unsignedp);
extern void spu_expand_insv (rtx * ops);
extern int spu_expand_block_move (rtx * ops);
extern void spu_emit_branch_or_set (int is_set, rtx cmp, rtx * operands);
extern int spu_emit_vector_cond_expr (rtx, rtx, rtx, rtx, rtx, rtx);
extern HOST_WIDE_INT const_double_to_hwint (rtx x);
extern void print_operand_address (FILE * file, register rtx addr);
extern void print_operand (FILE * file, rtx x, int code);
extern int spu_split_immediate (rtx * ops);
extern int spu_saved_regs_size (void);
extern int direct_return (void);
extern void spu_expand_prologue (void);
extern void spu_expand_epilogue (bool sibcall_p);
extern rtx spu_return_addr (int count, rtx frame);

#ifdef RTX_CODE
extern rtx hwint_to_const_double (enum machine_mode mode, HOST_WIDE_INT v);
extern rtx spu_const (enum machine_mode mode, HOST_WIDE_INT val);
extern rtx spu_const_from_ints (enum machine_mode mode, 
			        int a, int b, int c, int d);
extern rtx spu_float_const (const char *string,
			    enum machine_mode mode);
extern int immediate_load_p (rtx op, enum machine_mode mode);
extern int logical_immediate_p (rtx op, enum machine_mode mode);
extern int iohl_immediate_p (rtx op, enum machine_mode mode);
extern int arith_immediate_p (rtx op, enum machine_mode mode,
			      HOST_WIDE_INT low, HOST_WIDE_INT high);
extern bool exp2_immediate_p (rtx op, enum machine_mode mode, int low,
			      int high);
extern int spu_constant_address_p (rtx x);
extern bool spu_legitimate_constant_p (enum machine_mode, rtx);
extern int spu_initial_elimination_offset (int from, int to);
extern rtx spu_function_value (const_tree type, const_tree func);
extern int spu_expand_mov (rtx * ops, enum machine_mode mode);
extern int spu_split_load (rtx * ops);
extern int spu_split_store (rtx * ops);
extern int fsmbi_const_p (rtx x);
extern int cpat_const_p (rtx x, enum machine_mode mode);
extern rtx gen_cpat_const (rtx * ops);
extern void constant_to_array (enum machine_mode mode, rtx x,
			       unsigned char *arr);
extern rtx array_to_constant (enum machine_mode mode, const unsigned char *arr);
extern rtx spu_gen_exp2 (enum machine_mode mode, rtx x);
extern void spu_allocate_stack (rtx op0, rtx op1);
extern void spu_restore_stack_nonlocal (rtx op0, rtx op1);
extern void spu_restore_stack_block (rtx op0, rtx op1);
extern rtx spu_gen_subreg (enum machine_mode mode, rtx x);
extern int spu_safe_dma(HOST_WIDE_INT channel);
extern void spu_builtin_splats (rtx ops[]);
extern void spu_builtin_extract (rtx ops[]);
extern void spu_builtin_insert (rtx ops[]);
extern void spu_builtin_promote (rtx ops[]);
extern void spu_expand_sign_extend (rtx ops[]);
extern void spu_expand_vector_init (rtx target, rtx vals);
extern rtx spu_legitimize_reload_address (rtx, enum machine_mode, int, int);
#endif /* RTX_CODE  */

extern void spu_init_expanders (void);
extern void spu_split_convert (rtx *);
extern void spu_function_profiler (FILE *, int);

/* spu-c.c */
extern tree spu_resolve_overloaded_builtin (location_t, tree fndecl,
    					    void *fnargs);
extern rtx spu_expand_builtin (tree exp, rtx target, rtx subtarget,
			       enum machine_mode mode, int ignore);
extern rtx spu_expand_builtin (tree, rtx, rtx, enum machine_mode, int);

#endif /* _SPU_PROTOS_  */

