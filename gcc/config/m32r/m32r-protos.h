/* Prototypes for m32r.c functions used in the md file & elsewhere.
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.

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

/* Function prototypes that cannot exist in v850.h due to dependency
   compilcations.  */
#define Mmode enum machine_mode

extern void   sbss_section			PARAMS ((void));
extern void   sdata_section			PARAMS ((void));
extern void   m32r_init				PARAMS ((void));
extern void   m32r_init_expanders		PARAMS ((void));
extern unsigned m32r_compute_frame_size		PARAMS ((int));
extern int    m32r_first_insn_address		PARAMS ((void));
extern void   m32r_expand_prologue		PARAMS ((void));
extern void   m32r_output_function_prologue	PARAMS ((FILE *, int));
extern void   m32r_output_function_epilogue	PARAMS ((FILE *, int));
extern void   m32r_finalize_pic			PARAMS ((void));
extern void   m32r_asm_file_start		PARAMS ((FILE *));
extern void   m32r_sched_init 			PARAMS ((FILE *, int));
extern int    direct_return 			PARAMS ((void));
#ifdef TREE_CODE
extern int    m32r_valid_machine_decl_attribute	PARAMS ((tree, tree, tree, tree));
extern int    m32r_comp_type_attributes		PARAMS ((tree, tree));
extern void   m32r_select_section		PARAMS ((tree, int));
extern void   m32r_encode_section_info		PARAMS ((tree));
extern enum m32r_function_type m32r_compute_function_type PARAMS ((tree));
extern void   m32r_select_section 		PARAMS ((tree, int));
extern void   m32r_set_default_type_attributes  PARAMS ((tree));

#ifdef HAVE_MACHINE_MODES
extern void   m32r_setup_incoming_varargs	PARAMS ((CUMULATIVE_ARGS *, Mmode, tree, int *, int));
extern int    function_arg_partial_nregs	PARAMS ((CUMULATIVE_ARGS *, Mmode, tree, int));
#endif
#endif /* TREE_CODE */

#ifdef RTX_CODE
extern int    easy_di_const			PARAMS ((rtx));
extern int    easy_df_const			PARAMS ((rtx));
extern int    m32r_select_cc_mode		PARAMS ((int, rtx, rtx));
extern rtx    gen_compare			PARAMS ((enum rtx_code, rtx, rtx, int));
extern rtx    gen_split_move_double		PARAMS ((rtx *));
extern int    m32r_address_code			PARAMS ((rtx));
extern void   m32r_initialize_trampoline	PARAMS ((rtx, rtx, rtx));
extern int    zero_and_one			PARAMS ((rtx, rtx));
extern char * emit_cond_move			PARAMS ((rtx *, rtx));
extern char * m32r_output_block_move 		PARAMS ((rtx, rtx *));
extern void   m32r_expand_block_move 		PARAMS ((rtx *));
extern void   m32r_print_operand		PARAMS ((FILE *, rtx, int));
extern void   m32r_print_operand_address	PARAMS ((FILE *, rtx));
extern int    m32r_address_cost 		PARAMS ((rtx));
extern int    m32r_adjust_cost 			PARAMS ((rtx, rtx, rtx, int));
extern int    m32r_adjust_priority 		PARAMS ((rtx, int));
extern void   m32r_sched_reorder 		PARAMS ((FILE *, int, rtx *, int));
extern int    m32r_sched_variable_issue 	PARAMS ((FILE *, int, rtx, int));
extern int    m32r_not_same_reg 		PARAMS ((rtx, rtx));

#ifdef HAVE_MACHINE_MODES
extern int    call_address_operand		PARAMS ((rtx, Mmode));
extern int    call_operand			PARAMS ((rtx, Mmode));
extern int    symbolic_operand			PARAMS ((rtx, Mmode));
extern int    small_data_operand		PARAMS ((rtx, Mmode));
extern int    addr24_operand			PARAMS ((rtx, Mmode));
extern int    addr32_operand			PARAMS ((rtx, Mmode));
extern int    call26_operand			PARAMS ((rtx, Mmode));
extern int    seth_add3_operand			PARAMS ((rtx, Mmode));
extern int    cmp_int16_operand			PARAMS ((rtx, Mmode));
extern int    uint16_operand			PARAMS ((rtx, Mmode));
extern int    reg_or_int16_operand		PARAMS ((rtx, Mmode));
extern int    reg_or_uint16_operand		PARAMS ((rtx, Mmode));
extern int    reg_or_cmp_int16_operand		PARAMS ((rtx, Mmode));
extern int    two_insn_const_operand		PARAMS ((rtx, Mmode));
extern int    move_src_operand			PARAMS ((rtx, Mmode));
extern int    move_double_src_operand		PARAMS ((rtx, Mmode));
extern int    move_dest_operand			PARAMS ((rtx, Mmode));
extern int    eqne_comparison_operator		PARAMS ((rtx, Mmode));
extern int    signed_comparison_operator	PARAMS ((rtx, Mmode));
extern int    memreg_operand			PARAMS ((rtx, Mmode));
extern int    small_insn_p			PARAMS ((rtx, Mmode));
extern int    large_insn_p			PARAMS ((rtx, Mmode));
extern int    conditional_move_operand		PARAMS ((rtx, Mmode));
extern int    carry_compare_operand		PARAMS ((rtx, Mmode));
extern int    m32r_block_immediate_operand 	PARAMS ((rtx, Mmode));
extern int    extend_operand			PARAMS ((rtx, Mmode));
extern int    reg_or_eq_int16_operand		PARAMS ((rtx, Mmode));
extern int    int8_operand			PARAMS ((rtx, Mmode));
#endif /* HAVE_MACHINE_MODES */

#ifdef TREE_CODE
extern struct rtx_def * m32r_va_arg		PARAMS ((tree, tree));
#endif /* TREE_CODE */
#endif /* RTX_CODE */

#undef  Mmode
