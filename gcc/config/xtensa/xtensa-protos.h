/* Prototypes of target machine for GNU compiler for Xtensa.
   Copyright 2001,2002,2003 Free Software Foundation, Inc.
   Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 2, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING.  If not, write to the Free
Software Foundation, 59 Temple Place - Suite 330, Boston, MA
02111-1307, USA.  */

#ifndef __XTENSA_PROTOS_H__
#define __XTENSA_PROTOS_H__

/* Functions to test whether an immediate fits in a given field. */
extern int xtensa_simm7 PARAMS ((int));
extern int xtensa_simm8 PARAMS ((int));
extern int xtensa_simm8x256 PARAMS ((int));
extern int xtensa_simm12b PARAMS ((int));
extern int xtensa_uimm8 PARAMS ((int));
extern int xtensa_uimm8x2 PARAMS ((int));
extern int xtensa_uimm8x4 PARAMS ((int));
extern int xtensa_ai4const PARAMS ((int));
extern int xtensa_lsi4x4 PARAMS ((int));
extern int xtensa_b4const PARAMS ((int));
extern int xtensa_b4constu PARAMS ((int));
extern int xtensa_tp7 PARAMS ((int));

/* Functions within xtensa.c that we reference.  */
#ifdef RTX_CODE
extern int xt_true_regnum PARAMS ((rtx));
extern int add_operand PARAMS ((rtx, enum machine_mode));
extern int arith_operand PARAMS ((rtx, enum machine_mode));
extern int nonimmed_operand PARAMS ((rtx, enum machine_mode));
extern int mem_operand PARAMS ((rtx, enum machine_mode));
extern int xtensa_valid_move PARAMS ((enum machine_mode, rtx *operands));
extern int mask_operand PARAMS ((rtx, enum machine_mode));
extern int extui_fldsz_operand PARAMS ((rtx, enum machine_mode));
extern int sext_operand PARAMS ((rtx, enum machine_mode));
extern int sext_fldsz_operand PARAMS ((rtx, enum machine_mode));
extern int lsbitnum_operand PARAMS ((rtx, enum machine_mode));
extern int branch_operand PARAMS ((rtx, enum machine_mode));
extern int ubranch_operand PARAMS ((rtx, enum machine_mode));
extern int call_insn_operand PARAMS ((rtx, enum machine_mode));
extern int move_operand PARAMS ((rtx, enum machine_mode));
extern int smalloffset_mem_p PARAMS ((rtx));
extern int smalloffset_double_mem_p PARAMS ((rtx));
extern int constantpool_address_p PARAMS ((rtx));
extern int constantpool_mem_p PARAMS ((rtx));
extern int non_const_move_operand PARAMS ((rtx, enum machine_mode));
extern int const_float_1_operand PARAMS ((rtx, enum machine_mode));
extern int fpmem_offset_operand PARAMS ((rtx, enum machine_mode));
extern void xtensa_extend_reg PARAMS ((rtx, rtx));
extern void xtensa_load_constant PARAMS ((rtx, rtx));
extern int branch_operator PARAMS ((rtx, enum machine_mode));
extern int ubranch_operator PARAMS ((rtx, enum machine_mode));
extern int boolean_operator PARAMS ((rtx, enum machine_mode));
extern void xtensa_expand_conditional_branch PARAMS ((rtx *, enum rtx_code));
extern int xtensa_expand_conditional_move PARAMS ((rtx *, int));
extern int xtensa_expand_scc PARAMS ((rtx *));
extern int xtensa_expand_block_move PARAMS ((rtx *));
extern int xtensa_emit_move_sequence PARAMS ((rtx *, enum machine_mode));
extern bool xtensa_copy_incoming_a7 PARAMS ((rtx *, enum machine_mode));
extern void xtensa_emit_block_move PARAMS ((rtx *, rtx *, int));
extern void xtensa_expand_nonlocal_goto PARAMS ((rtx *));
extern void xtensa_emit_loop_end PARAMS ((rtx, rtx *));
extern char * xtensa_emit_call PARAMS ((int, rtx *));

#ifdef TREE_CODE
extern void init_cumulative_args PARAMS ((CUMULATIVE_ARGS *, tree, rtx));
extern void xtensa_va_start PARAMS ((tree, rtx));
extern rtx xtensa_va_arg PARAMS ((tree, tree));
#endif /* TREE_CODE */

extern void print_operand PARAMS ((FILE *, rtx, int));
extern void print_operand_address PARAMS ((FILE *, rtx));
extern void xtensa_output_literal
  PARAMS ((FILE *, rtx, enum machine_mode, int labelno));
extern void xtensa_reorg PARAMS ((rtx));
extern rtx xtensa_return_addr PARAMS ((int, rtx));
extern rtx xtensa_builtin_saveregs PARAMS ((void));
extern enum reg_class xtensa_preferred_reload_class
  PARAMS ((rtx, enum reg_class, int));
extern enum reg_class xtensa_secondary_reload_class
  PARAMS ((enum reg_class, enum machine_mode, rtx, int));
extern int a7_overlap_mentioned_p PARAMS ((rtx x));
#endif /* RTX_CODE */

#ifdef TREE_CODE
extern void function_arg_advance
  PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree));
extern struct rtx_def * function_arg
  PARAMS ((CUMULATIVE_ARGS *, enum machine_mode, tree, int));
extern tree xtensa_build_va_list PARAMS ((void));
#endif /* TREE_CODE */

extern int xtensa_mask_immediate PARAMS ((int));
extern int xtensa_mem_offset PARAMS ((unsigned, enum machine_mode));
extern void xtensa_setup_frame_addresses PARAMS ((void));
extern int xtensa_dbx_register_number PARAMS ((int));
extern void override_options PARAMS ((void));
extern long compute_frame_size PARAMS ((int));
extern int xtensa_frame_pointer_required PARAMS ((void));
extern void xtensa_function_prologue PARAMS ((FILE *, int));
extern void xtensa_function_epilogue PARAMS ((FILE *, int));
extern void order_regs_for_local_alloc PARAMS ((void));

#endif /* !__XTENSA_PROTOS_H__ */
