/* Prototypes of target machine for GNU compiler for Xtensa.
   Copyright (C) 2001-2023 Free Software Foundation, Inc.
   Contributed by Bob Wilson (bwilson@tensilica.com) at Tensilica.

This file is part of GCC.

GCC is free software; you can redistribute it and/or modify it under
the terms of the GNU General Public License as published by the Free
Software Foundation; either version 3, or (at your option) any later
version.

GCC is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with GCC; see the file COPYING3.  If not see
<http://www.gnu.org/licenses/>.  */

#ifndef __XTENSA_PROTOS_H__
#define __XTENSA_PROTOS_H__

/* Functions to test whether an immediate fits in a given field.  */
extern bool xtensa_simm8 (HOST_WIDE_INT);
extern bool xtensa_simm8x256 (HOST_WIDE_INT);
extern bool xtensa_simm12b (HOST_WIDE_INT);
extern bool xtensa_b4const_or_zero (HOST_WIDE_INT);
extern bool xtensa_b4constu (HOST_WIDE_INT);
extern bool xtensa_mask_immediate (HOST_WIDE_INT);
extern bool xtensa_mem_offset (unsigned, machine_mode);

/* Functions within xtensa.cc that we reference.  */
#ifdef RTX_CODE
extern int xt_true_regnum (rtx);
extern int xtensa_valid_move (machine_mode, rtx *);
extern int smalloffset_mem_p (rtx);
extern int constantpool_mem_p (rtx);
extern void xtensa_extend_reg (rtx, rtx);
extern void xtensa_expand_conditional_branch (rtx *, machine_mode);
extern int xtensa_expand_conditional_move (rtx *, int);
extern int xtensa_expand_scc (rtx *, machine_mode);
extern int xtensa_expand_block_move (rtx *);
extern int xtensa_expand_block_set_unrolled_loop (rtx *);
extern int xtensa_expand_block_set_small_loop (rtx *);
extern void xtensa_split_operand_pair (rtx *, machine_mode);
extern int xtensa_constantsynth (rtx, HOST_WIDE_INT);
extern int xtensa_emit_move_sequence (rtx *, machine_mode);
extern rtx xtensa_copy_incoming_a7 (rtx);
extern void xtensa_expand_nonlocal_goto (rtx *);
extern void xtensa_expand_compare_and_swap (rtx, rtx, rtx, rtx);
extern void xtensa_expand_atomic (enum rtx_code, rtx, rtx, rtx, bool);
extern void xtensa_emit_loop_end (rtx_insn *, rtx *);
extern char *xtensa_emit_branch (bool, rtx *);
extern char *xtensa_emit_movcc (bool, bool, bool, rtx *);
extern void xtensa_expand_call (int, rtx *);
extern char *xtensa_emit_call (int, rtx *);
extern char *xtensa_emit_sibcall (int, rtx *);
extern bool xtensa_tls_referenced_p (rtx);
extern enum rtx_code xtensa_shlrd_which_direction (rtx, rtx);
extern bool xtensa_split1_finished_p (void);
extern void xtensa_split_DI_reg_imm (rtx *);
extern bool xtensa_match_CLAMPS_imms_p (rtx, rtx);

#ifdef TREE_CODE
extern void init_cumulative_args (CUMULATIVE_ARGS *, int);
#endif /* TREE_CODE */

extern void print_operand (FILE *, rtx, int);
extern void print_operand_address (FILE *, rtx);
extern void xtensa_output_literal (FILE *, rtx, machine_mode, int);
extern void xtensa_set_return_address (rtx, rtx);
extern rtx xtensa_return_addr (int, rtx);
#endif /* RTX_CODE */

extern void xtensa_setup_frame_addresses (void);
extern int xtensa_debugger_regno (int);
extern long compute_frame_size (poly_int64);
extern bool xtensa_use_return_instruction_p (void);
extern void xtensa_expand_prologue (void);
extern void xtensa_expand_epilogue (bool);
extern void xtensa_adjust_reg_alloc_order (void);
extern enum reg_class xtensa_regno_to_class (int regno);
extern HOST_WIDE_INT xtensa_initial_elimination_offset (int from, int to);
extern const char **xtensa_get_config_strings (void);

#endif /* !__XTENSA_PROTOS_H__ */
