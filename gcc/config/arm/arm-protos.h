/* Prototypes for exported functions defined in arm.c and pe.c
   Copyright (C) 1999, 2000 Free Software Foundation, Inc.
   Contributed by Richard Earnshaw (rearnsha@arm.com)
   Minor hacks by Nick Clifton (nickc@cygnus.com)

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

extern void   arm_expand_prologue		PARAMS ((void));
extern void   arm_finalize_pic 			PARAMS ((void));
extern char * arm_output_epilogue		PARAMS ((void));
extern void   arm_override_options 		PARAMS ((void));
extern void   arm_poke_function_name 		PARAMS ((FILE *, char *));
extern int    arm_process_pragma		PARAMS ((int (*)(void), void (*) (int), char *));
extern int    arm_regno_class 			PARAMS ((int));
extern int    arm_volatile_func			PARAMS ((void));
extern int    const_ok_for_arm			PARAMS ((HOST_WIDE_INT));
extern void   output_arm_prologue		PARAMS ((FILE *, int));
extern void   output_ascii_pseudo_op		PARAMS ((FILE *, unsigned char *, int));
extern void   output_func_epilogue		PARAMS ((int));
extern void   output_func_prologue		PARAMS ((FILE *, int));
extern int    use_return_insn			PARAMS ((int));
extern const char * arm_strip_name_encoding	PARAMS ((const char *));
#if defined AOF_ASSEMBLER 
extern void   aof_add_import			PARAMS ((char *));
extern char * aof_data_section			PARAMS ((void));
extern void   aof_delete_import			PARAMS ((char *));
extern void   aof_dump_imports			PARAMS ((FILE *));
extern void   aof_dump_pic_table		PARAMS ((FILE *));
extern char * aof_text_section			PARAMS ((void));
#endif /* AOF_ASSEMBLER */
/* Defined in pe.c */
extern int    arm_dllexport_name_p 		PARAMS ((char *));
extern int    arm_dllimport_name_p 		PARAMS ((char *));

#define Mmode enum machine_mode

#ifdef TREE_CODE
extern int    arm_comp_type_attributes		PARAMS ((tree, tree));
extern int    arm_return_in_memory		PARAMS ((tree));
extern int    arm_valid_machine_decl_attribute	PARAMS ((tree, tree, tree));
extern int    arm_valid_type_attribute_p 	PARAMS ((tree, tree, tree, tree));
/* Defined in pe.c */
extern int    arm_dllexport_p 			PARAMS ((tree));
extern int    arm_dllimport_p 			PARAMS ((tree));
extern void   arm_mark_dllexport 		PARAMS ((tree));
extern void   arm_mark_dllimport 		PARAMS ((tree));
extern void   arm_pe_encode_section_info 	PARAMS ((tree));
extern tree   arm_pe_merge_machine_decl_attributes PARAMS ((tree, tree));
extern void   arm_pe_unique_section 		PARAMS ((tree, int));
extern int    arm_pe_valid_machine_decl_attribute PARAMS ((tree, tree, tree, tree));
extern void   arm_set_default_type_attributes	PARAMS ((tree));
extern void   arm_encode_call_attribute		PARAMS ((tree, char));
extern int    arm_pe_return_in_memory		PARAMS ((tree));
#endif

#ifdef RTX_CODE
extern int    adjacent_mem_locations		PARAMS ((rtx, rtx));
extern char * arithmetic_instr			PARAMS ((rtx, int));
extern int    arm_adjust_cost			PARAMS ((rtx, rtx, rtx, int));
extern RTX_CODE arm_canonicalize_comparison 	PARAMS ((RTX_CODE, rtx *));
extern int    arm_debugger_arg_offset		PARAMS ((int, rtx));
extern void   arm_final_prescan_insn		PARAMS ((rtx));
extern rtx    arm_gen_load_multiple		PARAMS ((int, int, rtx, int, int, int, int, int));
extern int    arm_gen_movstrqi			PARAMS ((rtx *));
extern rtx    arm_gen_store_multiple		PARAMS ((int, int, rtx, int, int, int, int, int));
extern void   arm_print_operand			PARAMS ((FILE *, rtx, int));
extern void   arm_reload_in_hi			PARAMS ((rtx *));
extern void   arm_reload_out_hi			PARAMS ((rtx *));
extern void   arm_reorg				PARAMS ((rtx));
extern int    arm_rtx_costs			PARAMS ((rtx, RTX_CODE));
extern Mmode  arm_select_cc_mode 		PARAMS ((RTX_CODE, rtx, rtx));
extern int    const_double_rtx_ok_for_fpu	PARAMS ((rtx));
extern int    const_ok_for_arm			PARAMS ((HOST_WIDE_INT));
extern char * emit_ldm_seq			PARAMS ((rtx *, int));
extern char * emit_stm_seq			PARAMS ((rtx *, int));
extern char * fp_immediate_constant		PARAMS ((rtx));
extern rtx    gen_compare_reg			PARAMS ((RTX_CODE, rtx, rtx));
extern rtx    gen_rotated_half_load		PARAMS ((rtx));
extern int    is_pic				PARAMS ((rtx));
extern int    label_mentioned_p 		PARAMS ((rtx));
extern int    legitimate_pic_operand_p		PARAMS ((rtx));
extern int    load_multiple_sequence		PARAMS ((rtx *, int, int *, int *, HOST_WIDE_INT *));
extern RTX_CODE minmax_code			PARAMS ((rtx));
extern int    neg_const_double_rtx_ok_for_fpu	PARAMS ((rtx));
extern char * output_add_immediate		PARAMS ((rtx *));
extern char * output_call			PARAMS ((rtx *));
extern char * output_call_mem			PARAMS ((rtx *));
extern char * output_mov_double_arm_from_fpu    PARAMS ((rtx *));
extern char * output_mov_double_fpu_from_arm    PARAMS ((rtx *));
extern char * output_mov_immediate		PARAMS ((rtx *));
extern char * output_mov_long_double_arm_from_arm PARAMS ((rtx *));
extern char * output_mov_long_double_arm_from_fpu PARAMS ((rtx *));
extern char * output_mov_long_double_fpu_from_arm PARAMS ((rtx *));
extern char * output_move_double		PARAMS ((rtx *));
extern char * output_return_instruction		PARAMS ((rtx, int, int));
extern int    store_multiple_sequence		PARAMS ((rtx *, int, int *, int *, HOST_WIDE_INT *));
extern int    symbol_mentioned_p		PARAMS ((rtx));
extern int    arm_is_longcall_p			PARAMS ((rtx, int, int));
#if defined AOF_ASSEMBLER 
extern rtx    aof_pic_entry			PARAMS ((rtx));
#endif /* AOF_ASSEMBLER */

#ifdef HAVE_MACHINE_MODES
extern int    alignable_memory_operand		PARAMS ((rtx, Mmode));
extern int    arm_add_operand			PARAMS ((rtx, Mmode));
extern int    arm_go_if_legitimate_address 	PARAMS ((Mmode, rtx));
extern int    arm_not_operand			PARAMS ((rtx, Mmode));
extern int    arm_reload_memory_operand		PARAMS ((rtx, Mmode));
extern int    arm_rhs_operand			PARAMS ((rtx, Mmode));
extern int    arm_rhsm_operand			PARAMS ((rtx, Mmode));
extern Mmode  arm_select_cc_mode 		PARAMS ((RTX_CODE, rtx, rtx));
extern int    arm_split_constant		PARAMS ((RTX_CODE, Mmode, HOST_WIDE_INT, rtx, rtx, int));
extern int    bad_signed_byte_operand		PARAMS ((rtx, Mmode));
extern int    cc_register			PARAMS ((rtx, Mmode));
extern int    const_shift_operand		PARAMS ((rtx, Mmode));
extern int    di_operand			PARAMS ((rtx, Mmode));
extern int    dominant_cc_register		PARAMS ((rtx, Mmode));
extern int    equality_operator			PARAMS ((rtx, Mmode));
extern int    f_register_operand		PARAMS ((rtx, Mmode));
extern int    fpu_add_operand			PARAMS ((rtx, Mmode));
extern int    fpu_rhs_operand			PARAMS ((rtx, Mmode));
extern int    index_operand			PARAMS ((rtx, Mmode));
extern rtx    legitimize_pic_address		PARAMS ((rtx, Mmode, rtx));
extern int    load_multiple_operation		PARAMS ((rtx, Mmode));
extern int    logical_binary_operator		PARAMS ((rtx, Mmode));
extern int    minmax_operator			PARAMS ((rtx, Mmode));
extern int    multi_register_push		PARAMS ((rtx, Mmode));
extern int    nonimmediate_di_operand		PARAMS ((rtx, Mmode));
extern int    nonimmediate_soft_df_operand 	PARAMS ((rtx, Mmode));
extern int    offsettable_memory_operand 	PARAMS ((rtx, Mmode));
extern int    power_of_two_operand		PARAMS ((rtx, Mmode));
extern int    reg_or_int_operand		PARAMS ((rtx, Mmode));
extern int    s_register_operand		PARAMS ((rtx, Mmode));
extern int    shift_operator			PARAMS ((rtx, Mmode));
extern int    shiftable_operator		PARAMS ((rtx, Mmode));
extern int    soft_df_operand			PARAMS ((rtx, Mmode));
extern int    store_multiple_operation		PARAMS ((rtx, Mmode));

#if defined TREE_CODE
extern rtx    arm_function_arg			PARAMS ((CUMULATIVE_ARGS *, Mmode, tree, int));
extern void   arm_init_cumulative_args		PARAMS ((CUMULATIVE_ARGS *, tree, rtx, int));
#endif /* TREE_CODE */
#endif /* HAVE_MACHINE_MODES */
#endif /* RTX_CODE */

#undef Mmode
