/* Prototypes for pa.c functions used in the md file & elsewhere.
   Copyright (C) 2000 Free Software Foundation, Inc.

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

#ifdef RTX_CODE
/* Prototype function used in MACHINE_DEPENDENT_REORG macro. */
extern void pa_reorg PARAMS ((rtx));

/* Prototype function used in various macros. */
extern int symbolic_operand PARAMS ((rtx, enum machine_mode));

/* Used in insn-*.c. */
extern int following_call PARAMS ((rtx));
extern int function_label_operand PARAMS ((rtx, enum machine_mode));
extern int lhs_lshift_cint_operand PARAMS ((rtx, enum machine_mode));

#ifdef TREE_CODE
extern void hppa_va_start PARAMS ((int, tree, rtx));
extern rtx hppa_va_arg PARAMS ((tree, tree));
#endif /* TREE_CODE */
extern rtx hppa_legitimize_address PARAMS ((rtx, rtx, enum machine_mode));

/* Define functions in pa.c and used in insn-output.c.  */

extern const char *output_and PARAMS ((rtx *));
extern const char *output_ior PARAMS ((rtx *));
extern const char *output_move_double PARAMS ((rtx *));
extern const char *output_fp_move_double PARAMS ((rtx *));
extern const char *output_block_move PARAMS ((rtx *, int));
extern const char *output_cbranch PARAMS ((rtx *, int, int, int, rtx));
extern const char *output_bb PARAMS ((rtx *, int, int, int, rtx, int));
extern const char *output_bvb PARAMS ((rtx *, int, int, int, rtx, int));
extern const char *output_dbra PARAMS ((rtx *, rtx, int));
extern const char *output_movb PARAMS ((rtx *, rtx, int, int));
extern const char *output_parallel_movb PARAMS ((rtx *, int));
extern const char *output_parallel_addb PARAMS ((rtx *, int));
extern const char *output_call PARAMS ((rtx, rtx, int));
extern const char *output_millicode_call PARAMS ((rtx, rtx));
extern const char *output_mul_insn PARAMS ((int, rtx));
extern const char *output_div_insn PARAMS ((rtx *, int, rtx));
extern const char *output_mod_insn PARAMS ((int, rtx));
extern const char *singlemove_string PARAMS ((rtx *));
extern void output_arg_descriptor PARAMS ((rtx));
extern void output_global_address PARAMS ((FILE *, rtx, int));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern rtx legitimize_pic_address PARAMS ((rtx, enum machine_mode, rtx));
extern struct rtx_def *gen_cmp_fp PARAMS ((enum rtx_code, rtx, rtx));
extern void hppa_encode_label PARAMS ((rtx, int));
extern int arith11_operand PARAMS ((rtx, enum machine_mode));
extern int symbolic_expression_p PARAMS ((rtx));
extern int hppa_address_cost PARAMS ((rtx));
extern int symbolic_memory_operand PARAMS ((rtx, enum machine_mode));
extern int pa_adjust_cost PARAMS ((rtx, rtx, rtx, int));
extern int pa_adjust_insn_length PARAMS ((rtx, int));
extern int int11_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_cint_move_operand PARAMS ((rtx, enum machine_mode));
extern int arith5_operand PARAMS ((rtx, enum machine_mode));
extern int uint5_operand PARAMS ((rtx, enum machine_mode));
extern int pic_label_operand PARAMS ((rtx, enum machine_mode));
extern int plus_xor_ior_operator PARAMS ((rtx, enum machine_mode));
extern int basereg_operand PARAMS ((rtx, enum machine_mode));
extern int shadd_operand PARAMS ((rtx, enum machine_mode));
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int read_only_operand PARAMS ((rtx, enum machine_mode));
extern int move_operand PARAMS ((rtx, enum machine_mode));
extern int and_operand PARAMS ((rtx, enum machine_mode));
extern int ior_operand PARAMS ((rtx, enum machine_mode));
extern int arith32_operand PARAMS ((rtx, enum machine_mode));
extern int uint32_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_nonsymb_mem_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_operand PARAMS ((rtx, enum machine_mode));
extern int reg_or_0_or_nonsymb_mem_operand PARAMS ((rtx, enum machine_mode));
extern int pre_cint_operand PARAMS ((rtx, enum machine_mode));
extern int post_cint_operand PARAMS ((rtx, enum machine_mode));
extern int div_operand PARAMS ((rtx, enum machine_mode));
extern int int5_operand PARAMS ((rtx, enum machine_mode));
extern int movb_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int ireg_or_int5_operand PARAMS ((rtx, enum machine_mode));
extern int fmpyaddoperands PARAMS ((rtx *));
extern int fmpysuboperands PARAMS ((rtx *));
extern int call_operand_address PARAMS ((rtx, enum machine_mode));
extern int ior_operand PARAMS ((rtx, enum machine_mode));
extern void emit_bcond_fp PARAMS ((enum rtx_code, rtx));
extern int emit_move_sequence PARAMS ((rtx *, enum machine_mode, rtx));
extern int emit_hpdiv_const PARAMS ((rtx *, int));
extern int is_function_label_plus_const PARAMS ((rtx));
extern int jump_in_call_delay PARAMS ((rtx));
extern enum reg_class secondary_reload_class PARAMS ((enum reg_class,
						      enum machine_mode, rtx));

/* Declare functions defined in pa.c and used in templates.  */

extern struct rtx_def *return_addr_rtx PARAMS ((int, rtx));

extern int fp_reg_operand PARAMS ((rtx, enum machine_mode));
extern int arith_double_operand PARAMS ((rtx, enum machine_mode));
extern int ireg_operand PARAMS ((rtx, enum machine_mode));
extern int lhs_lshift_operand PARAMS ((rtx, enum machine_mode));
extern int pc_or_label_operand PARAMS ((rtx, enum machine_mode));
#ifdef ARGS_SIZE_RTX
/* expr.h defines ARGS_SIZE_RTX and `enum direction' */
#ifdef TREE_CODE
extern enum direction function_arg_padding PARAMS ((enum machine_mode, tree));
#endif
#endif /* ARGS_SIZE_RTX */
extern int non_hard_reg_operand PARAMS ((rtx, enum machine_mode));
extern int eq_neq_comparison_operator PARAMS ((rtx, enum machine_mode));
extern int insn_refs_are_delayed PARAMS ((rtx));
#endif /* RTX_CODE */

/* Prototype function used in macro CONST_OK_FOR_LETTER_P. */
extern int zdepi_cint_p PARAMS ((unsigned HOST_WIDE_INT));

extern struct rtx_def *hppa_builtin_saveregs PARAMS ((void));

extern void output_deferred_plabels PARAMS ((FILE *));
extern void override_options PARAMS ((void));
extern void output_ascii PARAMS ((FILE *, const unsigned char *, int));
extern void output_function_prologue PARAMS ((FILE *, int));
extern void output_function_epilogue PARAMS ((FILE *, int));
extern int compute_frame_size PARAMS ((int, int *));
extern int and_mask_p PARAMS ((unsigned HOST_WIDE_INT));
extern int cint_ok_for_move PARAMS ((HOST_WIDE_INT));
extern void hppa_expand_prologue PARAMS ((void));
extern void hppa_expand_epilogue PARAMS ((void));
extern int hppa_can_use_return_insn_p PARAMS ((void));
extern int ior_mask_p PARAMS ((unsigned HOST_WIDE_INT));
extern void compute_zdepdi_operands PARAMS ((unsigned HOST_WIDE_INT,
					     unsigned *));
#ifdef RTX_CODE
extern char * output_64bit_and PARAMS ((rtx *));
extern char * output_64bit_ior PARAMS ((rtx *));
extern int cmpib_comparison_operator PARAMS ((rtx, enum machine_mode));
#endif



#ifdef TREE_CODE
extern int reloc_needed PARAMS ((tree));
#ifdef RTX_CODE
extern rtx function_arg PARAMS ((CUMULATIVE_ARGS *, enum machine_mode,
				 tree, int, int));
#endif
extern int function_arg_partial_nregs PARAMS ((CUMULATIVE_ARGS *,
					       enum machine_mode,
					       tree, int));
#endif /* TREE_CODE */
