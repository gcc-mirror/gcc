/* Definitions of target machine for GNU compiler, for IBM S/390.
   Copyright (C) 2000, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Hartmut Penner (hpenner@de.ibm.com)

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

/* Declare functions in s390.c.  */

extern void optimization_options PARAMS ((int, int));
extern void override_options PARAMS ((void));
extern int s390_arg_frame_offset PARAMS ((void));
extern void s390_emit_prologue PARAMS ((void));
extern void s390_emit_epilogue PARAMS ((void));
extern void s390_function_profiler PARAMS ((FILE *, int));

#ifdef RTX_CODE
extern int s390_address_cost PARAMS ((rtx));
extern int q_constraint PARAMS ((rtx));
extern int const0_operand PARAMS ((rtx, enum machine_mode));
extern int consttable_operand PARAMS ((rtx, enum machine_mode));
extern int larl_operand PARAMS ((rtx, enum machine_mode));
extern int s_operand PARAMS ((rtx, enum machine_mode));
extern int s_imm_operand PARAMS ((rtx, enum machine_mode));
extern int bras_sym_operand PARAMS ((rtx, enum machine_mode));
extern int load_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int store_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int s390_single_hi PARAMS ((rtx, enum machine_mode, int));
extern int s390_extract_hi PARAMS ((rtx, enum machine_mode, int));
extern int s390_single_qi PARAMS ((rtx, enum machine_mode, int));
extern int s390_extract_qi PARAMS ((rtx, enum machine_mode, int));
extern bool s390_split_ok_p PARAMS ((rtx, rtx, enum machine_mode, int));
extern int tls_symbolic_operand PARAMS ((rtx));

extern int s390_match_ccmode PARAMS ((rtx, enum machine_mode));
extern enum machine_mode s390_tm_ccmode PARAMS ((rtx, rtx, int));
extern enum machine_mode s390_select_ccmode PARAMS ((enum rtx_code, rtx, rtx));
extern int symbolic_reference_mentioned_p PARAMS ((rtx));
extern int tls_symbolic_reference_mentioned_p PARAMS ((rtx));
extern rtx s390_tls_get_offset PARAMS ((void));
extern int legitimate_la_operand_p PARAMS ((rtx));
extern int preferred_la_operand_p PARAMS ((rtx));
extern int legitimate_pic_operand_p PARAMS ((rtx));
extern int legitimate_constant_p PARAMS ((rtx));
extern int legitimate_reload_constant_p PARAMS ((rtx));
extern int legitimate_address_p PARAMS ((enum machine_mode, rtx, int));
extern rtx legitimize_pic_address PARAMS ((rtx, rtx));
extern rtx legitimize_address PARAMS ((rtx, rtx, enum machine_mode));
extern enum reg_class s390_preferred_reload_class PARAMS ((rtx, enum reg_class));
extern enum reg_class s390_secondary_input_reload_class PARAMS ((enum reg_class, enum machine_mode, rtx));
extern enum reg_class s390_secondary_output_reload_class PARAMS ((enum reg_class, enum machine_mode, rtx));
extern int s390_plus_operand PARAMS ((rtx, enum machine_mode));
extern void s390_expand_plus_operand PARAMS ((rtx, rtx, rtx));
extern void emit_symbolic_move PARAMS ((rtx *));
extern void s390_load_address PARAMS ((rtx, rtx));
extern void s390_expand_movstr PARAMS ((rtx, rtx, rtx));
extern void s390_expand_clrstr PARAMS ((rtx, rtx));
extern void s390_expand_cmpstr PARAMS ((rtx, rtx, rtx, rtx));
extern rtx s390_return_addr_rtx PARAMS ((int, rtx));

extern void s390_output_symbolic_const PARAMS ((FILE *, rtx));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern void s390_output_constant_pool PARAMS ((rtx, rtx));
extern void s390_trampoline_template PARAMS ((FILE *));
extern void s390_initialize_trampoline PARAMS ((rtx, rtx, rtx));
extern rtx s390_gen_rtx_const_DI PARAMS ((int, int));
extern rtx s390_simplify_dwarf_addr PARAMS ((rtx));
extern void s390_machine_dependent_reorg PARAMS ((rtx));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern int s390_function_arg_pass_by_reference PARAMS ((enum machine_mode, tree));
extern void s390_function_arg_advance PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern tree s390_build_va_list PARAMS ((void));
#ifdef RTX_CODE
extern rtx s390_function_arg PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern void s390_va_start PARAMS ((tree, rtx));
extern rtx s390_va_arg PARAMS ((tree, tree));
#endif /* RTX_CODE */
#endif /* TREE_CODE */

