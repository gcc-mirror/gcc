/* Frv prototypes.
   Copyright (C) 1999, 2000, 2001 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

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

/* Define the information needed to generate branch and scc insns.  This is
   stored from the compare operation.  Note that we can't use "rtx" here
   since it hasn't been defined!  */

/* Define global data defined in frv.c */
extern const char *frv_branch_cost_string;	/* -mbranch-cost option */
extern int frv_branch_cost_int;			/* value of -mbranch_cost */

extern const char *frv_cpu_string;		/* -mcpu= option */

extern const char *frv_condexec_insns_str;	/* -mcond-exec-insns= option */
extern int frv_condexec_insns;			/* value of -mcond-exec-insns */

extern const char *frv_condexec_temps_str;	/* -mcond-exec-temps= option */
extern int frv_condexec_temps;			/* value of -mcond-exec-temps */

extern const char *frv_sched_lookahead_str;	/* -msched-lookahead= option */
extern int frv_sched_lookahead;			/* value -msched-lookahead= */

/* CPU type.  This must be identical to the cpu enumeration in frv.md.  */
typedef enum frv_cpu
{
  FRV_CPU_GENERIC,
  FRV_CPU_FR500,
  FRV_CPU_FR400,
  FRV_CPU_FR300,
  FRV_CPU_SIMPLE,
  FRV_CPU_TOMCAT
} frv_cpu_t;

extern frv_cpu_t frv_cpu_type;			/* value of -mcpu= */

/* Define functions defined in frv.c */
extern void frv_expand_prologue			PARAMS ((void));
extern void frv_expand_epilogue			PARAMS ((int));
extern void frv_override_options		PARAMS ((void));
extern void frv_optimization_options		PARAMS ((int, int));
extern void frv_conditional_register_usage	PARAMS ((void));
extern frv_stack_t *frv_stack_info		PARAMS ((void));
extern void frv_debug_stack			PARAMS ((frv_stack_t *));
extern int frv_frame_pointer_required		PARAMS ((void));
extern int frv_initial_elimination_offset	PARAMS ((int, int));

#ifdef RTX_CODE
extern int frv_legitimate_address_p		PARAMS ((enum machine_mode, rtx,
						       int, int));
extern rtx frv_legitimize_address		PARAMS ((rtx, rtx,
							 enum machine_mode));

#ifdef TREE_CODE
extern void frv_init_cumulative_args		PARAMS ((CUMULATIVE_ARGS *, tree,
						       rtx, int, int));

extern int frv_function_arg_boundary		PARAMS ((enum machine_mode, tree));
extern rtx frv_function_arg			PARAMS ((CUMULATIVE_ARGS *,
						       enum machine_mode,
						       tree, int, int));

extern void frv_function_arg_advance		PARAMS ((CUMULATIVE_ARGS *,
						       enum machine_mode,
						       tree, int));

extern int frv_function_arg_partial_nregs	PARAMS ((CUMULATIVE_ARGS *,
						       enum machine_mode,
						       tree, int));

extern int frv_function_arg_pass_by_reference	PARAMS ((CUMULATIVE_ARGS *,
						       enum machine_mode,
						       tree, int));

extern int frv_function_arg_callee_copies	PARAMS ((CUMULATIVE_ARGS *,
						       enum machine_mode,
						       tree, int));

extern int frv_function_arg_keep_as_reference	PARAMS ((CUMULATIVE_ARGS *,
						       enum machine_mode,
						       tree, int));

extern rtx frv_expand_builtin_saveregs		PARAMS ((void));
extern void frv_setup_incoming_varargs		PARAMS ((CUMULATIVE_ARGS *,
							 enum machine_mode,
							 tree, int *, int));

extern void frv_expand_builtin_va_start		PARAMS ((tree, rtx));
extern rtx frv_expand_builtin_va_arg		PARAMS ((tree, tree));
#endif /* TREE_CODE */

extern int frv_expand_block_move		PARAMS ((rtx *));
extern int frv_expand_block_clear		PARAMS ((rtx *));
extern rtx frv_dynamic_chain_address		PARAMS ((rtx));
extern rtx frv_return_addr_rtx			PARAMS ((int, rtx));
extern rtx frv_index_memory			PARAMS ((rtx,
							 enum machine_mode,
							 int));
extern const char *frv_asm_output_opcode
				 	PARAMS ((FILE *, const char *));
extern void frv_final_prescan_insn	PARAMS ((rtx, rtx *, int));
extern void frv_print_operand		PARAMS ((FILE *, rtx, int));
extern void frv_print_operand_address	PARAMS ((FILE *, rtx));
extern int frv_emit_movsi		PARAMS ((rtx, rtx));
extern const char *output_move_single	PARAMS ((rtx *, rtx));
extern const char *output_move_double	PARAMS ((rtx *, rtx));
extern const char *output_condmove_single
					PARAMS ((rtx *, rtx));
extern int frv_emit_cond_branch		PARAMS ((enum rtx_code, rtx));
extern int frv_emit_scc			PARAMS ((enum rtx_code, rtx));
extern rtx frv_split_scc		PARAMS ((rtx, rtx, rtx, rtx,
						 HOST_WIDE_INT));
extern int frv_emit_cond_move		PARAMS ((rtx, rtx, rtx, rtx));
extern rtx frv_split_cond_move		PARAMS ((rtx *));
extern rtx frv_split_minmax		PARAMS ((rtx *));
extern rtx frv_split_abs		PARAMS ((rtx *));
extern void frv_split_double_load	PARAMS ((rtx, rtx));
extern void frv_split_double_store	PARAMS ((rtx, rtx));
#ifdef BLOCK_HEAD
extern void frv_ifcvt_init_extra_fields	PARAMS ((ce_if_block_t *));
extern void frv_ifcvt_modify_tests	PARAMS ((ce_if_block_t *,
						 rtx *, rtx *));
extern void frv_ifcvt_modify_multiple_tests
					PARAMS ((ce_if_block_t *,
						 basic_block,
						 rtx *, rtx *));
extern rtx frv_ifcvt_modify_insn	PARAMS ((ce_if_block_t *,
						 rtx, rtx));
extern void frv_ifcvt_modify_final	PARAMS ((ce_if_block_t *));
extern void frv_ifcvt_modify_cancel	PARAMS ((ce_if_block_t *));
#endif
extern int frv_trampoline_size		PARAMS ((void));
extern void frv_initialize_trampoline	PARAMS ((rtx, rtx, rtx));
extern enum reg_class frv_secondary_reload_class
					PARAMS ((enum reg_class class,
					       enum machine_mode mode,
					       rtx x, int));
extern int frv_class_likely_spilled_p	PARAMS ((enum reg_class class));
extern int frv_hard_regno_mode_ok	PARAMS ((int, enum machine_mode));
extern int frv_hard_regno_nregs		PARAMS ((int, enum machine_mode));
extern int frv_class_max_nregs		PARAMS ((enum reg_class class,
						 enum machine_mode mode));
extern int frv_legitimate_constant_p	PARAMS ((rtx));
#endif	/* RTX_CODE */

extern int direct_return_p		PARAMS ((void));
extern int frv_register_move_cost	PARAMS ((enum reg_class, enum reg_class));

#ifdef TREE_CODE
extern int frv_adjust_field_align	PARAMS ((tree, int));
#endif

extern void fixup_section		PARAMS ((void));
extern void sdata_section		PARAMS ((void));
extern void sbss_section		PARAMS ((void));
extern void const_section		PARAMS ((void));
extern void data_section		PARAMS ((void));

#ifdef RTX_CODE
extern int integer_register_operand	PARAMS ((rtx, enum machine_mode));
extern int frv_load_operand		PARAMS ((rtx, enum machine_mode));
extern int gpr_or_fpr_operand		PARAMS ((rtx, enum machine_mode));
extern int gpr_no_subreg_operand	PARAMS ((rtx, enum machine_mode));
extern int gpr_or_int6_operand		PARAMS ((rtx, enum machine_mode));
extern int fpr_or_int6_operand		PARAMS ((rtx, enum machine_mode));
extern int gpr_or_int_operand		PARAMS ((rtx, enum machine_mode));
extern int gpr_or_int12_operand		PARAMS ((rtx, enum machine_mode));
extern int gpr_fpr_or_int12_operand	PARAMS ((rtx, enum machine_mode));
extern int gpr_or_int10_operand		PARAMS ((rtx, enum machine_mode));
extern int move_source_operand		PARAMS ((rtx, enum machine_mode));
extern int move_destination_operand	PARAMS ((rtx, enum machine_mode));
extern int condexec_source_operand	PARAMS ((rtx, enum machine_mode));
extern int condexec_dest_operand	PARAMS ((rtx, enum machine_mode));
extern int lr_operand			PARAMS ((rtx, enum machine_mode));
extern int gpr_or_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int fpr_or_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_operand		PARAMS ((rtx, enum machine_mode));
extern int fcc_operand			PARAMS ((rtx, enum machine_mode));
extern int icc_operand			PARAMS ((rtx, enum machine_mode));
extern int cc_operand			PARAMS ((rtx, enum machine_mode));
extern int fcr_operand			PARAMS ((rtx, enum machine_mode));
extern int icr_operand			PARAMS ((rtx, enum machine_mode));
extern int cr_operand			PARAMS ((rtx, enum machine_mode));
extern int call_operand			PARAMS ((rtx, enum machine_mode));
extern int fpr_operand			PARAMS ((rtx, enum machine_mode));
extern int even_reg_operand		PARAMS ((rtx, enum machine_mode));
extern int odd_reg_operand		PARAMS ((rtx, enum machine_mode));
extern int even_gpr_operand		PARAMS ((rtx, enum machine_mode));
extern int odd_gpr_operand		PARAMS ((rtx, enum machine_mode));
extern int quad_fpr_operand		PARAMS ((rtx, enum machine_mode));
extern int even_fpr_operand		PARAMS ((rtx, enum machine_mode));
extern int odd_fpr_operand		PARAMS ((rtx, enum machine_mode));
extern int dbl_memory_one_insn_operand	PARAMS ((rtx, enum machine_mode));
extern int dbl_memory_two_insn_operand	PARAMS ((rtx, enum machine_mode));
extern int int12_operand		PARAMS ((rtx, enum machine_mode));
extern int int6_operand			PARAMS ((rtx, enum machine_mode));
extern int int5_operand			PARAMS ((rtx, enum machine_mode));
extern int uint5_operand		PARAMS ((rtx, enum machine_mode));
extern int uint4_operand		PARAMS ((rtx, enum machine_mode));
extern int uint1_operand		PARAMS ((rtx, enum machine_mode));
extern int int_2word_operand		PARAMS ((rtx, enum machine_mode));
extern int pic_register_operand		PARAMS ((rtx, enum machine_mode));
extern int pic_symbolic_operand		PARAMS ((rtx, enum machine_mode));
extern int small_data_register_operand	PARAMS ((rtx, enum machine_mode));
extern int small_data_symbolic_operand	PARAMS ((rtx, enum machine_mode));
extern int upper_int16_operand		PARAMS ((rtx, enum machine_mode));
extern int uint16_operand		PARAMS ((rtx, enum machine_mode));
extern int relational_operator		PARAMS ((rtx, enum machine_mode));
extern int signed_relational_operator	PARAMS ((rtx, enum machine_mode));
extern int unsigned_relational_operator	PARAMS ((rtx, enum machine_mode));
extern int float_relational_operator	PARAMS ((rtx, enum machine_mode));
extern int ccr_eqne_operator		PARAMS ((rtx, enum machine_mode));
extern int minmax_operator		PARAMS ((rtx, enum machine_mode));
extern int condexec_si_binary_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_si_media_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_si_divide_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_si_unary_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_sf_conv_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_sf_add_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int intop_compare_operator	PARAMS ((rtx, enum machine_mode));
extern int condexec_intop_cmp_operator	PARAMS ((rtx, enum machine_mode));
extern int acc_operand			PARAMS ((rtx, enum machine_mode));
extern int even_acc_operand		PARAMS ((rtx, enum machine_mode));
extern int quad_acc_operand		PARAMS ((rtx, enum machine_mode));
extern int accg_operand			PARAMS ((rtx, enum machine_mode));
extern rtx frv_matching_accg_for_acc	PARAMS ((rtx));
extern void frv_machine_dependent_reorg	PARAMS ((rtx));
#endif

