/* d30v prototypes.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* External functions called.  */

extern void override_options (void);
#ifdef RTX_CODE
extern int short_memory_operand (rtx, enum machine_mode);
extern int long_memory_operand (rtx, enum machine_mode);
extern int d30v_memory_operand (rtx, enum machine_mode);
extern int single_reg_memory_operand (rtx, enum machine_mode);
extern int const_addr_memory_operand (rtx, enum machine_mode);
extern int call_operand (rtx, enum machine_mode);
extern int gpr_operand (rtx, enum machine_mode);
extern int accum_operand (rtx, enum machine_mode);
extern int gpr_or_accum_operand (rtx, enum machine_mode);
extern int cr_operand (rtx, enum machine_mode);
extern int repeat_operand (rtx, enum machine_mode);
extern int flag_operand (rtx, enum machine_mode);
extern int br_flag_operand (rtx, enum machine_mode);
extern int br_flag_or_constant_operand (rtx, enum machine_mode);
extern int gpr_br_flag_operand (rtx, enum machine_mode);
extern int f0_operand (rtx, enum machine_mode);
extern int f1_operand (rtx, enum machine_mode);
extern int carry_operand (rtx, enum machine_mode);
extern int reg_or_0_operand (rtx, enum machine_mode);
extern int gpr_or_signed6_operand (rtx, enum machine_mode);
extern int gpr_or_unsigned5_operand (rtx, enum machine_mode);
extern int gpr_or_unsigned6_operand (rtx, enum machine_mode);
extern int gpr_or_constant_operand (rtx, enum machine_mode);
extern int gpr_or_dbl_const_operand (rtx, enum machine_mode);
extern int gpr_or_memory_operand (rtx, enum machine_mode);
extern int move_input_operand (rtx, enum machine_mode);
extern int move_output_operand (rtx, enum machine_mode);
extern int signed6_operand (rtx, enum machine_mode);
extern int unsigned5_operand (rtx, enum machine_mode);
extern int unsigned6_operand (rtx, enum machine_mode);
extern int bitset_operand (rtx, enum machine_mode);
extern int condexec_test_operator (rtx, enum machine_mode);
extern int condexec_branch_operator (rtx, enum machine_mode);
extern int condexec_unary_operator (rtx, enum machine_mode);
extern int condexec_addsub_operator (rtx, enum machine_mode);
extern int condexec_binary_operator (rtx, enum machine_mode);
extern int condexec_shiftl_operator (rtx, enum machine_mode);
extern int condexec_extend_operator (rtx, enum machine_mode);
extern int branch_zero_operator (rtx, enum machine_mode);
extern int cond_move_dest_operand (rtx, enum machine_mode);
extern int cond_move_operand (rtx, enum machine_mode);
extern int cond_exec_operand (rtx, enum machine_mode);
extern int srelational_si_operator (rtx, enum machine_mode);
extern int urelational_si_operator (rtx, enum machine_mode);
extern int relational_di_operator (rtx, enum machine_mode);
#endif
extern d30v_stack_t *d30v_stack_info (void);
extern int direct_return (void);

#ifdef TREE_CODE
#ifdef RTX_CODE
extern void d30v_init_cumulative_args (CUMULATIVE_ARGS *, tree,
				       rtx, tree, int);
#endif
extern int d30v_function_arg_boundary (enum machine_mode, tree);
#ifdef RTX_CODE
extern rtx d30v_function_arg (CUMULATIVE_ARGS *,
			      enum machine_mode, tree, int, int);
#endif
extern int d30v_function_arg_partial_nregs (CUMULATIVE_ARGS *,
					    enum machine_mode, tree, int);

extern int d30v_function_arg_pass_by_reference (CUMULATIVE_ARGS *,
						enum machine_mode, tree, int);

extern void d30v_function_arg_advance (CUMULATIVE_ARGS *,
				       enum machine_mode, tree, int);
#endif

#ifdef RTX_CODE
extern rtx d30v_expand_builtin_saveregs (void);
#endif
#ifdef TREE_CODE
extern void d30v_setup_incoming_varargs (CUMULATIVE_ARGS *,
					 enum machine_mode, tree, int *, int);
#ifdef RTX_CODE
extern void d30v_expand_builtin_va_start (tree, rtx);
extern rtx d30v_expand_builtin_va_arg (tree, tree);
#endif /* RTX_CODE */
#endif /* TREE_CODE */

extern void d30v_expand_prologue (void);
extern void d30v_expand_epilogue (void);
extern void d30v_function_profiler (FILE *, int);
#ifdef RTX_CODE
extern void d30v_split_double (rtx, rtx *, rtx *);
extern void d30v_print_operand (FILE *, rtx, int);
extern void d30v_print_operand_address (FILE *, rtx);
#endif
extern int d30v_trampoline_size (void);
#ifdef RTX_CODE
extern void d30v_initialize_trampoline (rtx, rtx, rtx);
extern int d30v_legitimate_address_p (enum machine_mode, rtx, int);
extern rtx d30v_legitimize_address (rtx, rtx, enum machine_mode, int);
extern int d30v_mode_dependent_address_p (rtx);
extern rtx d30v_emit_comparison (int, rtx, rtx, rtx);
extern const char *d30v_move_2words (rtx *, rtx);
extern int d30v_emit_cond_move (rtx, rtx, rtx, rtx);
extern rtx d30v_return_addr (void);
#endif
extern void d30v_init_expanders (void);
extern void debug_stack_info (d30v_stack_t *);


/* External variables referenced */

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  */

extern GTY(()) rtx d30v_compare_op0;
extern GTY(()) rtx d30v_compare_op1;

/* Define the information needed to modify the epilogue for EH.  */

#ifdef RTX_CODE
extern rtx d30v_eh_epilogue_sp_ofs;
#endif
