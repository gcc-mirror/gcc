/* Definitions of target machine for GNU compiler for iq2000.
   Copyright (C) 2003, 2004, 2007 Free Software Foundation, Inc.

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

#ifndef GCC_IQ2000_PROTOS_H
#define GCC_IQ2000_PROTOS_H

extern int              iq2000_check_split (rtx, enum machine_mode);
extern int              iq2000_reg_mode_ok_for_base_p (rtx, enum machine_mode, int);
extern const char *     iq2000_fill_delay_slot (const char *, enum delay_type, rtx *, rtx);
extern const char *     iq2000_move_1word (rtx *, rtx, int);
extern void             override_options (void);
extern HOST_WIDE_INT    iq2000_debugger_offset (rtx, HOST_WIDE_INT);
extern void             final_prescan_insn (rtx, rtx *, int);
extern HOST_WIDE_INT    compute_frame_size (HOST_WIDE_INT);
extern int              iq2000_initial_elimination_offset (int, int);
extern void             iq2000_expand_prologue (void);
extern void             iq2000_expand_epilogue (void);
extern void             iq2000_expand_eh_return (rtx);
extern int              iq2000_can_use_return_insn (void);
extern int              iq2000_adjust_insn_length (rtx, int);
extern char *           iq2000_output_conditional_branch (rtx, rtx *, int, int, int, int);
extern void             print_operand_address (FILE *, rtx);
extern void             print_operand (FILE *, rtx, int);

#ifdef RTX_CODE
extern rtx              gen_int_relational (enum rtx_code, rtx, rtx, rtx, int *);
extern void             gen_conditional_branch (rtx *, enum machine_mode);
#endif

#ifdef TREE_CODE
extern void             init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx);
extern void             function_arg_advance (CUMULATIVE_ARGS *, enum machine_mode, tree, int);
extern struct rtx_def * function_arg (CUMULATIVE_ARGS *, enum machine_mode, const_tree, int);
extern bool 		iq2000_function_value_regno_p (const unsigned int);
#endif

#endif /* ! GCC_IQ2000_PROTOS_H */
