/* Prototypes for exported functions defined in mmix.c
   Copyright (C) 2000, 2001, 2002  Free Software Foundation, Inc.
   Contributed by Hans-Peter Nilsson (hp@bitrange.com)

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

extern void mmix_override_options PARAMS ((void));
extern void mmix_init_expanders PARAMS ((void));
extern int mmix_eh_return_data_regno PARAMS ((int));
extern int mmix_initial_elimination_offset PARAMS ((int, int));
extern int mmix_starting_frame_offset PARAMS ((void));
extern int mmix_function_arg_regno_p PARAMS ((int, int));
extern void mmix_function_profiler PARAMS ((FILE *, int));
extern void mmix_trampoline_template PARAMS ((FILE *));
extern int mmix_trampoline_size;
extern int mmix_reversible_cc_mode PARAMS ((enum machine_mode));
extern int mmix_register_move_cost
  PARAMS ((enum machine_mode, enum reg_class, enum reg_class));
extern const char *mmix_text_section_asm_op PARAMS ((void));
extern const char *mmix_data_section_asm_op PARAMS ((void));
extern void mmix_asm_file_start PARAMS ((FILE *));
extern void mmix_asm_file_end PARAMS ((FILE *));
extern void mmix_asm_output_source_filename PARAMS ((FILE *, const char *));
extern void mmix_output_quoted_string PARAMS ((FILE *, const char *, int));
extern void mmix_asm_output_source_line  PARAMS ((FILE *, int));
extern void mmix_asm_output_ascii PARAMS ((FILE *, const char *, int));
extern void mmix_asm_output_label PARAMS ((FILE *, const char *));
extern void mmix_asm_weaken_label PARAMS ((FILE *, const char *));
extern void mmix_asm_output_labelref PARAMS ((FILE *, const char *));
extern void mmix_asm_output_internal_label
  PARAMS ((FILE *, const char *, int));
extern void mmix_asm_output_def PARAMS ((FILE *, const char *, const char *));
extern int mmix_print_operand_punct_valid_p PARAMS ((int));
extern void mmix_asm_output_reg_push PARAMS ((FILE *, int));
extern void mmix_asm_output_reg_pop PARAMS ((FILE *, int));
extern void mmix_asm_output_skip PARAMS ((FILE *, int));
extern void mmix_asm_output_align PARAMS ((FILE *, int));
extern int mmix_shiftable_wyde_value PARAMS ((unsigned HOST_WIDEST_INT));
extern void mmix_output_register_setting
  PARAMS ((FILE *, int, HOST_WIDEST_INT, int));
extern void mmix_conditional_register_usage PARAMS ((void));
extern int mmix_local_regno PARAMS ((int));
extern int mmix_dbx_register_number PARAMS ((int));
extern int mmix_use_simple_return PARAMS ((void));
extern void mmix_make_decl_one_only PARAMS ((tree));
extern int mmix_function_arg_pass_by_reference
  PARAMS ((const CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern rtx mmix_function_outgoing_value PARAMS ((tree, tree));
extern int mmix_function_value_regno_p PARAMS ((int));
extern int mmix_data_alignment PARAMS ((tree, int));
extern int mmix_constant_alignment PARAMS ((tree, int));
extern int mmix_local_alignment PARAMS ((tree, int));
extern void mmix_setup_incoming_varargs
  PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int *, int));
extern void mmix_asm_output_pool_prologue
  PARAMS ((FILE *, const char *, tree, int));
extern void mmix_asm_output_aligned_common
  PARAMS ((FILE *, const char *, int, int));
extern void mmix_asm_output_aligned_local
  PARAMS ((FILE *, const char *, int, int));
extern void mmix_asm_declare_register_global
  PARAMS ((FILE *, tree, int, const char *));
extern rtx mmix_function_arg
  PARAMS ((const CUMULATIVE_ARGS *, enum machine_mode, tree, int, int));
extern rtx mmix_expand_builtin_va_arg PARAMS ((tree, tree));
extern void mmix_asm_output_addr_diff_elt PARAMS ((FILE *, rtx, int, int));
extern void mmix_asm_output_addr_vec_elt PARAMS ((FILE *, int));
extern enum reg_class mmix_preferred_reload_class
  PARAMS ((rtx, enum reg_class));
extern enum reg_class mmix_preferred_output_reload_class
  PARAMS ((rtx, enum reg_class));
extern enum reg_class mmix_secondary_reload_class
  PARAMS ((enum reg_class, enum machine_mode, rtx, int));
extern int mmix_const_ok_for_letter_p PARAMS ((HOST_WIDE_INT, int));
extern int mmix_const_double_ok_for_letter_p PARAMS ((rtx, int));
extern int mmix_extra_constraint PARAMS ((rtx, int, int));
extern rtx mmix_dynamic_chain_address PARAMS ((rtx));
extern rtx mmix_return_addr_rtx PARAMS ((int, rtx));
extern rtx mmix_eh_return_stackadj_rtx PARAMS ((void));
extern rtx mmix_eh_return_handler_rtx PARAMS ((void));
extern void mmix_initialize_trampoline PARAMS ((rtx, rtx, rtx));
extern int mmix_constant_address_p PARAMS ((rtx));
extern int mmix_legitimate_address PARAMS ((enum machine_mode, rtx, int));
extern int mmix_legitimate_constant_p PARAMS ((rtx));
extern int mmix_address_cost PARAMS ((rtx));
extern void mmix_print_operand PARAMS ((FILE *, rtx, int));
extern void mmix_print_operand_address PARAMS ((FILE *, rtx));
extern void mmix_machine_dependent_reorg PARAMS ((rtx));
extern void mmix_expand_prologue PARAMS ((void));
extern void mmix_expand_epilogue PARAMS ((void));
extern rtx mmix_get_hard_reg_initial_val PARAMS ((enum machine_mode, int));
extern int mmix_asm_preferred_eh_data_format PARAMS ((int, int));
extern void mmix_setup_frame_addresses PARAMS ((void));

#ifdef RTX_CODE
/* Needs to be ifdef:d for sake of enum rtx_code.  */
extern enum machine_mode mmix_select_cc_mode PARAMS ((enum rtx_code, rtx, rtx));
extern void mmix_canonicalize_comparison PARAMS ((enum rtx_code *, rtx *, rtx *));
extern int mmix_rtx_cost_recalculated
  PARAMS ((rtx, enum rtx_code, enum rtx_code, int *));
extern int mmix_valid_comparison PARAMS ((enum rtx_code, enum machine_mode, rtx));
extern rtx mmix_gen_compare_reg PARAMS ((enum rtx_code, rtx, rtx));
#endif

/*
 * Local variables:
 * eval: (c-set-style "gnu")
 * indent-tabs-mode: t
 * End:
 */
