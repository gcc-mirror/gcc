/* Prototypes for exported functions defined in arm.c and pe.c
   Copyright (C) 1999, 2000, 2001, 2002 Free Software Foundation, Inc.
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

#ifndef GCC_ARM_PROTOS_H
#define GCC_ARM_PROTOS_H

extern void   rdata_section		PARAMS ((void));
extern void   arm_override_options	PARAMS ((void));
extern int    use_return_insn		PARAMS ((int));
extern int    arm_regno_class 		PARAMS ((int));
extern void   arm_finalize_pic		PARAMS ((int));
extern int    arm_volatile_func		PARAMS ((void));
extern const char * arm_output_epilogue	PARAMS ((int));
extern void   arm_expand_prologue	PARAMS ((void));
extern HOST_WIDE_INT arm_get_frame_size	PARAMS ((void));
/* Used in arm.md, but defined in output.c.  */
extern void   assemble_align		PARAMS ((int)); 
extern const char * arm_strip_name_encoding	PARAMS ((const char *));
extern void   arm_asm_output_labelref	PARAMS ((FILE *, const char *));
extern unsigned long arm_current_func_type	PARAMS ((void));
extern unsigned int  arm_compute_initial_elimination_offset PARAMS ((unsigned int, unsigned int));

#ifdef TREE_CODE
extern int    arm_return_in_memory	PARAMS ((tree));
extern void   arm_encode_call_attribute	PARAMS ((tree, int));
extern int    arm_function_ok_for_sibcall PARAMS ((tree));
#endif
#ifdef RTX_CODE
extern int    arm_hard_regno_mode_ok	PARAMS ((unsigned int,
						enum machine_mode));
extern int    const_ok_for_arm		PARAMS ((HOST_WIDE_INT));
extern int    arm_split_constant	PARAMS ((RTX_CODE, enum machine_mode,
						HOST_WIDE_INT, rtx, rtx, int));
extern RTX_CODE arm_canonicalize_comparison PARAMS ((RTX_CODE, rtx *));
extern int    legitimate_pic_operand_p	PARAMS ((rtx));
extern rtx    legitimize_pic_address	PARAMS ((rtx, enum machine_mode, rtx));
extern int    arm_rtx_costs		PARAMS ((rtx, RTX_CODE, RTX_CODE));
extern int    const_double_rtx_ok_for_fpu	PARAMS ((rtx));
extern int    neg_const_double_rtx_ok_for_fpu	PARAMS ((rtx));

/* Predicates.  */
extern int    s_register_operand	PARAMS ((rtx, enum machine_mode));
extern int    arm_hard_register_operand	PARAMS ((rtx, enum machine_mode));
extern int    f_register_operand	PARAMS ((rtx, enum machine_mode));
extern int    reg_or_int_operand	PARAMS ((rtx, enum machine_mode));
extern int    arm_reload_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int    arm_rhs_operand		PARAMS ((rtx, enum machine_mode));
extern int    arm_rhsm_operand		PARAMS ((rtx, enum machine_mode));
extern int    arm_add_operand		PARAMS ((rtx, enum machine_mode));
extern int    arm_not_operand		PARAMS ((rtx, enum machine_mode));
extern int    offsettable_memory_operand PARAMS ((rtx, enum machine_mode));
extern int    alignable_memory_operand	PARAMS ((rtx, enum machine_mode));
extern int    bad_signed_byte_operand	PARAMS ((rtx, enum machine_mode));
extern int    fpu_rhs_operand		PARAMS ((rtx, enum machine_mode));
extern int    fpu_add_operand		PARAMS ((rtx, enum machine_mode));
extern int    power_of_two_operand	PARAMS ((rtx, enum machine_mode));
extern int    nonimmediate_di_operand	PARAMS ((rtx, enum machine_mode));
extern int    di_operand		PARAMS ((rtx, enum machine_mode));
extern int    nonimmediate_soft_df_operand PARAMS ((rtx, enum machine_mode));
extern int    soft_df_operand		PARAMS ((rtx, enum machine_mode));
extern int    index_operand		PARAMS ((rtx, enum machine_mode));
extern int    const_shift_operand	PARAMS ((rtx, enum machine_mode));
extern int    arm_comparison_operator	PARAMS ((rtx, enum machine_mode));
extern int    shiftable_operator	PARAMS ((rtx, enum machine_mode));
extern int    shift_operator		PARAMS ((rtx, enum machine_mode));
extern int    equality_operator		PARAMS ((rtx, enum machine_mode));
extern int    minmax_operator		PARAMS ((rtx, enum machine_mode));
extern int    cc_register		PARAMS ((rtx, enum machine_mode));
extern int    dominant_cc_register	PARAMS ((rtx, enum machine_mode));
extern int    logical_binary_operator	PARAMS ((rtx, enum machine_mode));
extern int    multi_register_push	PARAMS ((rtx, enum machine_mode));
extern int    load_multiple_operation	PARAMS ((rtx, enum machine_mode));
extern int    store_multiple_operation	PARAMS ((rtx, enum machine_mode));

extern int    symbol_mentioned_p	PARAMS ((rtx));
extern int    label_mentioned_p		PARAMS ((rtx));
extern RTX_CODE minmax_code		PARAMS ((rtx));
extern int    adjacent_mem_locations	PARAMS ((rtx, rtx));
extern int    load_multiple_sequence	PARAMS ((rtx *, int, int *, int *,
						HOST_WIDE_INT *));
extern const char * emit_ldm_seq	PARAMS ((rtx *, int));
extern int    store_multiple_sequence	PARAMS ((rtx *, int, int *, int *,
						HOST_WIDE_INT *));
extern const char * emit_stm_seq	PARAMS ((rtx *, int));
extern rtx    arm_gen_load_multiple	PARAMS ((int, int, rtx, int, int, int,
						int, int));
extern rtx    arm_gen_store_multiple	PARAMS ((int, int, rtx, int, int, int,
						int, int));
extern int    arm_gen_movstrqi		PARAMS ((rtx *));
extern rtx    arm_gen_rotated_half_load	PARAMS ((rtx));
extern enum machine_mode arm_select_cc_mode PARAMS ((RTX_CODE, rtx, rtx));
extern rtx    arm_gen_compare_reg	PARAMS ((RTX_CODE, rtx, rtx));
extern rtx    arm_gen_return_addr_mask	PARAMS ((void));
extern void   arm_reload_in_hi		PARAMS ((rtx *));
extern void   arm_reload_out_hi		PARAMS ((rtx *));
extern void   arm_reorg			PARAMS ((rtx));
extern const char * fp_immediate_constant PARAMS ((rtx));
extern const char * output_call		PARAMS ((rtx *));
extern const char * output_call_mem	PARAMS ((rtx *));
extern const char * output_mov_long_double_fpu_from_arm PARAMS ((rtx *));
extern const char * output_mov_long_double_arm_from_fpu PARAMS ((rtx *));
extern const char * output_mov_long_double_arm_from_arm PARAMS ((rtx *));
extern const char * output_mov_double_fpu_from_arm      PARAMS ((rtx *));
extern const char * output_mov_double_arm_from_fpu      PARAMS ((rtx *));
extern const char * output_move_double	PARAMS ((rtx *));
extern const char * output_mov_immediate PARAMS ((rtx *));
extern const char * output_add_immediate PARAMS ((rtx *));
extern const char * arithmetic_instr	PARAMS ((rtx, int));
extern void   output_ascii_pseudo_op	PARAMS ((FILE *, const unsigned char *,
						int));
extern const char * output_return_instruction PARAMS ((rtx, int, int));
extern void   arm_poke_function_name	PARAMS ((FILE *, const char *));
extern void   arm_print_operand		PARAMS ((FILE *, rtx, int));
extern void   arm_print_operand_address	PARAMS ((FILE *, rtx));
extern void   arm_final_prescan_insn	PARAMS ((rtx));
extern int    arm_go_if_legitimate_address PARAMS ((enum machine_mode, rtx));
extern int    arm_debugger_arg_offset	PARAMS ((int, rtx));
extern int    arm_is_longcall_p 	PARAMS ((rtx, int, int));

#if defined TREE_CODE
extern rtx    arm_function_arg		PARAMS ((CUMULATIVE_ARGS *,
						enum machine_mode, tree, int));
extern void   arm_init_cumulative_args	PARAMS ((CUMULATIVE_ARGS *, tree, rtx,
						int));
extern rtx    arm_va_arg                PARAMS ((tree, tree));
extern int    arm_function_arg_pass_by_reference PARAMS ((CUMULATIVE_ARGS *,
							 enum machine_mode,
						         tree, int));

#endif

#if defined AOF_ASSEMBLER 
extern rtx    aof_pic_entry		PARAMS ((rtx));
extern void   aof_dump_pic_table	PARAMS ((FILE *));
extern char * aof_text_section		PARAMS ((void));
extern char * aof_data_section		PARAMS ((void));
extern void   aof_add_import		PARAMS ((const char *));
extern void   aof_delete_import		PARAMS ((const char *));
extern void   aof_dump_imports		PARAMS ((FILE *));
extern void   zero_init_section		PARAMS ((void));
extern void   common_section		PARAMS ((void));
#endif /* AOF_ASSEMBLER */

#endif /* RTX_CODE */

extern int    arm_float_words_big_endian PARAMS ((void));

/* Thumb functions.  */
extern void   arm_init_expanders	PARAMS ((void));
extern int    thumb_far_jump_used_p	PARAMS ((int));
extern const char * thumb_unexpanded_epilogue	PARAMS ((void));
extern HOST_WIDE_INT thumb_get_frame_size PARAMS ((void));
extern void   thumb_expand_prologue	PARAMS ((void));
extern void   thumb_expand_epilogue	PARAMS ((void));
#ifdef TREE_CODE
extern int    is_called_in_ARM_mode	PARAMS ((tree));
#endif
extern int    thumb_shiftable_const	PARAMS ((unsigned HOST_WIDE_INT));
#ifdef RTX_CODE
extern void   thumb_final_prescan_insn	PARAMS ((rtx));
extern const char * thumb_load_double_from_address
					PARAMS ((rtx *));
extern const char * thumb_output_move_mem_multiple
					PARAMS ((int, rtx *));
extern void   thumb_expand_movstrqi	PARAMS ((rtx *));
extern int    thumb_cmp_operand		PARAMS ((rtx, enum machine_mode));
extern rtx *  thumb_legitimize_pic_address
					PARAMS ((rtx, enum machine_mode, rtx));
extern int    thumb_go_if_legitimate_address
					PARAMS ((enum machine_mode, rtx));
extern rtx    arm_return_addr		PARAMS ((int, rtx));
extern void   thumb_reload_out_hi	PARAMS ((rtx *));
extern void   thumb_reload_in_hi	PARAMS ((rtx *));
#endif

/* Defined in pe.c.  */
extern int  arm_dllexport_name_p 	PARAMS ((const char *));
extern int  arm_dllimport_name_p 	PARAMS ((const char *));

#ifdef TREE_CODE
extern void arm_pe_unique_section 	PARAMS ((tree, int));
extern void arm_pe_encode_section_info 	PARAMS ((tree, int));
extern int  arm_dllexport_p 		PARAMS ((tree));
extern int  arm_dllimport_p 		PARAMS ((tree));
extern void arm_mark_dllexport 		PARAMS ((tree));
extern void arm_mark_dllimport 		PARAMS ((tree));
#endif

extern void arm_init_builtins		PARAMS ((void));
#if defined (TREE_CODE) && defined (RTX_CODE)
extern rtx arm_expand_builtin		PARAMS ((tree, rtx, rtx,
					       enum machine_mode, int));
#endif

#ifdef GCC_C_PRAGMA_H  /* included from code that cares about pragmas */
extern void arm_pr_long_calls		PARAMS ((cpp_reader *));
extern void arm_pr_no_long_calls	PARAMS ((cpp_reader *));
extern void arm_pr_long_calls_off	PARAMS ((cpp_reader *));
#endif

#endif /* ! GCC_ARM_PROTOS_H */
