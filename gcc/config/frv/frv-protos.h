/* Frv prototypes.
   Copyright (C) 1999-2017 Free Software Foundation, Inc.
   Contributed by Red Hat, Inc.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 3, or (at your option)
any later version.

GCC is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

/* Define functions defined in frv.c */
extern void frv_expand_prologue			(void);
extern void frv_expand_epilogue			(bool);
extern frv_stack_t *frv_stack_info		(void);
extern void frv_debug_stack			(frv_stack_t *);
extern int frv_initial_elimination_offset	(int, int);
extern void frv_ifcvt_machdep_init		(void *);

#ifdef RTX_CODE
extern int frv_legitimate_address_p_1		(machine_mode, rtx,
						 int, int, int);
extern rtx frv_find_base_term			(rtx);

#ifdef TREE_CODE
extern void frv_init_cumulative_args		(CUMULATIVE_ARGS *, tree,
						 rtx, tree, int);

extern bool frv_function_value_regno_p		(const unsigned int);
#endif /* TREE_CODE */

extern int frv_expand_block_move		(rtx *);
extern int frv_expand_block_clear		(rtx *);
extern rtx frv_dynamic_chain_address		(rtx);
extern rtx frv_return_addr_rtx			(int, rtx);
extern rtx frv_index_memory			(rtx, machine_mode, int);
extern const char *frv_asm_output_opcode
				 	(FILE *, const char *);
extern void frv_final_prescan_insn	(rtx_insn *, rtx *, int);
extern void frv_emit_move		(machine_mode, rtx, rtx);
extern int frv_emit_movsi		(rtx, rtx);
extern const char *output_move_single	(rtx *, rtx);
extern const char *output_move_double	(rtx *, rtx);
extern const char *output_condmove_single
					(rtx *, rtx);
extern int frv_emit_cond_branch		(rtx *);
extern int frv_emit_scc			(rtx *);
extern rtx frv_split_scc		(rtx, rtx, rtx, rtx, HOST_WIDE_INT);
extern int frv_emit_cond_move		(rtx, rtx, rtx, rtx);
extern rtx frv_split_cond_move		(rtx *);
extern rtx frv_split_minmax		(rtx *);
extern rtx frv_split_abs		(rtx *);
extern void frv_split_double_load	(rtx, rtx);
extern void frv_split_double_store	(rtx, rtx);
#ifdef BB_HEAD
extern void frv_ifcvt_init_extra_fields	(struct ce_if_block *);
extern void frv_ifcvt_modify_tests	(struct ce_if_block *, rtx *, rtx *);
extern void frv_ifcvt_modify_multiple_tests
					(struct ce_if_block *, basic_block,
					 rtx *, rtx *);
extern rtx frv_ifcvt_modify_insn	(struct ce_if_block *, rtx, rtx);
extern void frv_ifcvt_modify_final	(struct ce_if_block *);
extern void frv_ifcvt_modify_cancel	(struct ce_if_block *);
#endif
extern enum reg_class frv_secondary_reload_class
					(enum reg_class,
					 machine_mode, rtx);
extern int frv_hard_regno_nregs		(int, machine_mode);
extern int frv_class_max_nregs		(enum reg_class rclass,
					 machine_mode mode);
extern machine_mode frv_select_cc_mode (enum rtx_code, rtx, rtx);
#endif	/* RTX_CODE */

extern int frv_trampoline_size		(void);
extern int direct_return_p		(void);
extern int frv_issue_rate		(void);
extern int frv_acc_group		(rtx);

#ifdef TREE_CODE
extern int frv_adjust_field_align	(tree, int);
#endif

#ifdef RTX_CODE
extern int integer_register_operand	(rtx, machine_mode);
extern int frv_load_operand		(rtx, machine_mode);
extern int gpr_or_fpr_operand		(rtx, machine_mode);
extern int gpr_no_subreg_operand	(rtx, machine_mode);
extern int gpr_or_int6_operand		(rtx, machine_mode);
extern int fpr_or_int6_operand		(rtx, machine_mode);
extern int gpr_or_int_operand		(rtx, machine_mode);
extern int gpr_or_int12_operand		(rtx, machine_mode);
extern int gpr_fpr_or_int12_operand	(rtx, machine_mode);
extern int gpr_or_int10_operand		(rtx, machine_mode);
extern int move_source_operand		(rtx, machine_mode);
extern int move_destination_operand	(rtx, machine_mode);
extern int condexec_source_operand	(rtx, machine_mode);
extern int condexec_dest_operand	(rtx, machine_mode);
extern int lr_operand			(rtx, machine_mode);
extern int gpr_or_memory_operand	(rtx, machine_mode);
extern int fpr_or_memory_operand	(rtx, machine_mode);
extern int reg_or_0_operand		(rtx, machine_mode);
extern int fcc_operand			(rtx, machine_mode);
extern int icc_operand			(rtx, machine_mode);
extern int cc_operand			(rtx, machine_mode);
extern int fcr_operand			(rtx, machine_mode);
extern int icr_operand			(rtx, machine_mode);
extern int cr_operand			(rtx, machine_mode);
extern int call_operand			(rtx, machine_mode);
extern int fpr_operand			(rtx, machine_mode);
extern int even_reg_operand		(rtx, machine_mode);
extern int odd_reg_operand		(rtx, machine_mode);
extern int even_gpr_operand		(rtx, machine_mode);
extern int odd_gpr_operand		(rtx, machine_mode);
extern int quad_fpr_operand		(rtx, machine_mode);
extern int even_fpr_operand		(rtx, machine_mode);
extern int odd_fpr_operand		(rtx, machine_mode);
extern int dbl_memory_one_insn_operand	(rtx, machine_mode);
extern int dbl_memory_two_insn_operand	(rtx, machine_mode);
extern int int12_operand		(rtx, machine_mode);
extern int int6_operand			(rtx, machine_mode);
extern int int5_operand			(rtx, machine_mode);
extern int uint5_operand		(rtx, machine_mode);
extern int uint4_operand		(rtx, machine_mode);
extern int uint1_operand		(rtx, machine_mode);
extern int int_2word_operand		(rtx, machine_mode);
extern int pic_register_operand		(rtx, machine_mode);
extern int pic_symbolic_operand		(rtx, machine_mode);
extern int small_data_register_operand	(rtx, machine_mode);
extern int small_data_symbolic_operand	(rtx, machine_mode);
extern int upper_int16_operand		(rtx, machine_mode);
extern int uint16_operand		(rtx, machine_mode);
extern int symbolic_operand		(rtx, machine_mode);
extern int relational_operator		(rtx, machine_mode);
extern int signed_relational_operator	(rtx, machine_mode);
extern int unsigned_relational_operator	(rtx, machine_mode);
extern int float_relational_operator	(rtx, machine_mode);
extern int ccr_eqne_operator		(rtx, machine_mode);
extern int minmax_operator		(rtx, machine_mode);
extern int condexec_si_binary_operator	(rtx, machine_mode);
extern int condexec_si_media_operator	(rtx, machine_mode);
extern int condexec_si_divide_operator	(rtx, machine_mode);
extern int condexec_si_unary_operator	(rtx, machine_mode);
extern int condexec_sf_conv_operator	(rtx, machine_mode);
extern int condexec_sf_add_operator	(rtx, machine_mode);
extern int condexec_memory_operand	(rtx, machine_mode);
extern int intop_compare_operator	(rtx, machine_mode);
extern int acc_operand			(rtx, machine_mode);
extern int even_acc_operand		(rtx, machine_mode);
extern int quad_acc_operand		(rtx, machine_mode);
extern int accg_operand			(rtx, machine_mode);
extern rtx frv_matching_accg_for_acc	(rtx);
extern void frv_expand_fdpic_call	(rtx *, bool, bool);
extern rtx frv_gen_GPsym2reg		(rtx, rtx);
extern int frv_legitimate_memory_operand (rtx, machine_mode, int);

/* Information about a relocation unspec.  SYMBOL is the relocation symbol
   (a SYMBOL_REF or LABEL_REF), RELOC is the type of relocation and OFFSET
   is the constant addend.  */
struct frv_unspec {
  rtx symbol;
  int reloc;
  HOST_WIDE_INT offset;
};

extern bool frv_const_unspec_p (rtx, struct frv_unspec *);

#endif

