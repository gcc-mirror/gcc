/* Definitions of target machine for GNU compiler, for IBM S/390.
   Copyright (C) 2000 Free Software Foundation, Inc.
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
extern int s390_arg_frame_offset PARAMS ((void));
extern void s390_function_prologue PARAMS ((FILE *, HOST_WIDE_INT));
extern void s390_function_epilogue PARAMS ((FILE *, HOST_WIDE_INT));

#ifdef RTX_CODE
extern int const0_operand PARAMS ((rtx, enum machine_mode));
extern int const1_operand PARAMS ((rtx, enum machine_mode));
extern int larl_operand PARAMS ((rtx, enum machine_mode));
extern int fp_operand PARAMS ((rtx, enum machine_mode));
extern int s_operand PARAMS ((rtx, enum machine_mode));
extern int r_or_s_operand PARAMS ((rtx, enum machine_mode));
extern int r_or_s_or_im8_operand PARAMS ((rtx, enum machine_mode));
extern int r_or_x_or_im16_operand PARAMS ((rtx, enum machine_mode));
extern int r_or_im8_operand PARAMS ((rtx, enum machine_mode));
extern int tmxx_operand PARAMS ((rtx, enum machine_mode));
extern int bras_sym_operand PARAMS ((rtx, enum machine_mode));
extern int load_multiple_operation PARAMS ((rtx, enum machine_mode));
extern int store_multiple_operation PARAMS ((rtx, enum machine_mode));

extern int s390_match_ccmode PARAMS ((rtx, enum machine_mode));
extern int symbolic_reference_mentioned_p PARAMS ((rtx));
extern int legitimate_pic_operand_p PARAMS ((rtx));
extern int legitimate_constant_p PARAMS ((rtx));
extern int legitimate_address_p PARAMS ((enum machine_mode, rtx, int));
extern rtx legitimize_pic_address PARAMS ((rtx, rtx));
extern rtx legitimize_address PARAMS ((rtx, rtx, enum machine_mode));
extern void emit_pic_move PARAMS ((rtx *, enum machine_mode));

extern void s390_output_symbolic_const PARAMS ((FILE *, rtx));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void print_operand PARAMS ((FILE *, rtx, int));
extern int s390_adjust_cost PARAMS ((rtx, rtx, rtx, int));
extern int s390_stop_dump_lit_p PARAMS ((rtx));
extern void s390_dump_literal_pool PARAMS ((rtx, rtx));
extern void s390_trampoline_template PARAMS ((FILE *));
extern void s390_initialize_trampoline PARAMS ((rtx, rtx, rtx));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern void s390_asm_output_pool_prologue PARAMS ((FILE *, const char *, tree, int));
extern int s390_function_arg_pass_by_reference PARAMS ((enum machine_mode, tree));
extern void s390_function_arg_advance PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern tree s390_build_va_list PARAMS ((void));
#ifdef RTX_CODE
extern rtx s390_function_arg PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern void s390_va_start PARAMS ((int, tree, rtx));
extern rtx s390_va_arg PARAMS ((tree, tree));
#endif /* RTX_CODE */
#endif /* TREE_CODE */

