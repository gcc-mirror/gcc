/* d30v prototypes.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Cygnus Solutions.

This file is part of GNU CC.

GNU CC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU CC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU CC; see the file COPYING.  If not, write to
the Free Software Foundation, 59 Temple Place - Suite 330,
Boston, MA 02111-1307, USA.  */

/* External functions called.  */

extern void override_options		PARAMS ((void));
#ifdef RTX_CODE
extern int short_memory_operand		PARAMS ((rtx, enum machine_mode));
extern int long_memory_operand		PARAMS ((rtx, enum machine_mode));
extern int d30v_memory_operand		PARAMS ((rtx, enum machine_mode));
extern int single_reg_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int const_addr_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int call_operand			PARAMS ((rtx, enum machine_mode));
extern int gpr_operand			PARAMS ((rtx, enum machine_mode));
extern int accum_operand		PARAMS ((rtx, enum machine_mode));
extern int gpr_or_accum_operand		PARAMS ((rtx, enum machine_mode));
extern int cr_operand			PARAMS ((rtx, enum machine_mode));
extern int repeat_operand		PARAMS ((rtx, enum machine_mode));
extern int flag_operand			PARAMS ((rtx, enum machine_mode));
extern int br_flag_operand		PARAMS ((rtx, enum machine_mode));
extern int br_flag_or_constant_operand	PARAMS ((rtx, enum machine_mode));
extern int gpr_br_flag_operand		PARAMS ((rtx, enum machine_mode));
extern int f0_operand			PARAMS ((rtx, enum machine_mode));
extern int f1_operand			PARAMS ((rtx, enum machine_mode));
extern int carry_operand		PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_operand		PARAMS ((rtx, enum machine_mode));
extern int gpr_or_signed6_operand	PARAMS ((rtx, enum machine_mode));
extern int gpr_or_unsigned5_operand	PARAMS ((rtx, enum machine_mode));
extern int gpr_or_unsigned6_operand	PARAMS ((rtx, enum machine_mode));
extern int gpr_or_constant_operand	PARAMS ((rtx, enum machine_mode));
extern int gpr_or_dbl_const_operand	PARAMS ((rtx, enum machine_mode));
extern int gpr_or_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int move_input_operand		PARAMS ((rtx, enum machine_mode));
extern int move_output_operand		PARAMS ((rtx, enum machine_mode));
extern int signed6_operand		PARAMS ((rtx, enum machine_mode));
extern int unsigned5_operand		PARAMS ((rtx, enum machine_mode));
extern int unsigned6_operand		PARAMS ((rtx, enum machine_mode));
extern int bitset_operand		PARAMS ((rtx, enum machine_mode));
extern int condexec_test_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_branch_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_unary_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_addsub_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_binary_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_shiftl_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_extend_operator	PARAMS ((rtx, enum machine_mode));
extern int branch_zero_operator		PARAMS ((rtx, enum machine_mode));
extern int cond_move_dest_operand	PARAMS ((rtx, enum machine_mode));
extern int cond_move_operand		PARAMS ((rtx, enum machine_mode));
extern int cond_exec_operand		PARAMS ((rtx, enum machine_mode));
extern int srelational_si_operand	PARAMS ((rtx, enum machine_mode));
extern int urelational_si_operand	PARAMS ((rtx, enum machine_mode));
extern int relational_di_operand	PARAMS ((rtx, enum machine_mode));
#endif
extern d30v_stack_t *d30v_stack_info	PARAMS ((void));
extern int direct_return		PARAMS ((void));

#ifdef TREE_CODE
#ifdef RTX_CODE
extern void d30v_init_cumulative_args	PARAMS ((CUMULATIVE_ARGS *, tree,
						 rtx, int, int));
#endif
extern int d30v_function_arg_boundary	PARAMS ((enum machine_mode, tree));
#ifdef RTX_CODE
extern rtx d30v_function_arg		PARAMS ((CUMULATIVE_ARGS *,
						 enum machine_mode,
						 tree, int, int));
#endif
extern int d30v_function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS *,
						    enum machine_mode,
						    tree, int));

extern int d30v_function_arg_pass_by_reference PARAMS ((CUMULATIVE_ARGS *,
							enum machine_mode,
							tree, int));

extern void d30v_function_arg_advance	PARAMS ((CUMULATIVE_ARGS *,
						 enum machine_mode,
						 tree, int));
#endif

#ifdef RTX_CODE
extern rtx d30v_expand_builtin_saveregs	PARAMS ((void));
#endif
#ifdef TREE_CODE
extern void d30v_setup_incoming_varargs	PARAMS ((CUMULATIVE_ARGS *,
						 enum machine_mode,
						 tree, int *, int));
extern tree d30v_build_va_list		PARAMS ((void));
#ifdef RTX_CODE
extern void d30v_expand_builtin_va_start PARAMS ((int, tree, rtx));
extern rtx d30v_expand_builtin_va_arg	PARAMS ((tree, tree));
#endif	/* RTX_CODE */
#endif	/* TREE_CODE */

extern void d30v_function_prologue	PARAMS ((FILE *, int));
extern void d30v_function_epilogue	PARAMS ((FILE *, int));
extern void d30v_function_profiler	PARAMS ((FILE *, int));
#ifdef RTX_CODE
extern void d30v_split_double		PARAMS ((rtx, rtx *, rtx *));
extern void d30v_print_operand		PARAMS ((FILE *, rtx, int));
extern void d30v_print_operand_address	PARAMS ((FILE *, rtx));
#endif
extern int d30v_trampoline_size		PARAMS ((void));
#ifdef RTX_CODE
extern void d30v_initialize_trampoline	PARAMS ((rtx, rtx, rtx));
extern int d30v_legitimate_address_p	PARAMS ((enum machine_mode, rtx, int));
extern rtx d30v_legitimize_address	PARAMS ((rtx, rtx,
						 enum machine_mode, int));
extern int d30v_mode_dependent_address_p PARAMS ((rtx));
extern rtx d30v_emit_comparison		PARAMS ((int, rtx, rtx, rtx));
extern char *d30v_move_2words		PARAMS ((rtx *, rtx));
extern int d30v_emit_cond_move		PARAMS ((rtx, rtx, rtx, rtx));
extern void d30v_machine_dependent_reorg PARAMS ((rtx));
extern int d30v_adjust_cost		PARAMS ((rtx, rtx, rtx, int));
extern rtx d30v_return_addr		PARAMS ((void));
#endif


/* External variables referenced */

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  */

extern struct rtx_def *d30v_compare_op0;
extern struct rtx_def *d30v_compare_op1;

/* Define the information needed to modify the epilogue for EH.  */

#ifdef RTX_CODE
extern rtx d30v_eh_epilogue_sp_ofs;
#endif
