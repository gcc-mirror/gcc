/* Prototypes for exported functions defined in m68hc11.c
   Copyright (C) 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2007, 2009, 2010
   Free Software Foundation, Inc.
   Contributed by Stephane Carrez (stcarrez@nerim.fr)

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


extern int hard_regno_mode_ok (int, enum machine_mode);
extern int m68hc11_hard_regno_rename_ok (int, int);

extern int m68hc11_total_frame_size (void);
extern int m68hc11_initial_frame_pointer_offset (void);
extern int m68hc11_initial_elimination_offset (int, int);

extern void expand_prologue (void);
extern void expand_epilogue (void);

#ifdef RTX_CODE
extern int m68hc11_auto_inc_p (rtx);

extern rtx m68hc11_expand_compare_and_branch (enum rtx_code, rtx, rtx, rtx);
extern enum reg_class preferred_reload_class (rtx, enum reg_class);

extern void m68hc11_notice_update_cc (rtx, rtx);
extern void m68hc11_notice_keep_cc (rtx);

extern void m68hc11_gen_movqi (rtx, rtx*);
extern void m68hc11_gen_movhi (rtx, rtx*);
extern void m68hc11_gen_rotate (enum rtx_code, rtx, rtx*);

extern void m68hc11_output_swap (rtx, rtx*);

extern int next_insn_test_reg (rtx, rtx);

extern int m68hc11_reload_operands (rtx*);

extern int dead_register_here (rtx, rtx);

extern int push_pop_operand_p (rtx);
extern void m68hc11_split_move (rtx, rtx, rtx);
extern void m68hc11_split_compare_and_branch (enum rtx_code,
                                              rtx, rtx, rtx);

extern rtx m68hc11_gen_lowpart (enum machine_mode, rtx);
extern rtx m68hc11_gen_highpart (enum machine_mode, rtx);

#ifdef HAVE_MACHINE_MODES
extern int m68hc11_memory_move_cost (enum machine_mode, enum reg_class, int);
extern int m68hc11_register_move_cost (enum machine_mode,
                                       enum reg_class, enum reg_class);

extern void m68hc11_emit_libcall (const char*, enum rtx_code,
                                  enum machine_mode, enum machine_mode,
                                  int, rtx*);
extern int m68hc11_small_indexed_indirect_p (rtx, enum machine_mode);
extern int m68hc11_symbolic_p (rtx, enum machine_mode);
extern int m68hc11_indirect_p (rtx, enum machine_mode);
extern int go_if_legitimate_address2 (rtx, enum machine_mode, int);

extern int reg_or_indexed_operand (rtx,enum machine_mode);
extern int memory_indexed_operand (rtx, enum machine_mode);

#ifdef RTX_CODE
extern void m68hc11_split_logical (enum machine_mode, enum rtx_code, rtx*);
#endif

extern int m68hc11_register_indirect_p (rtx, enum machine_mode);
extern int m68hc11_valid_addressing_p (rtx, enum machine_mode, int);

extern int symbolic_memory_operand (rtx, enum machine_mode);

extern int memory_reload_operand (rtx, enum machine_mode);
extern int arith_src_operand (rtx, enum machine_mode);
extern int soft_reg_operand (rtx, enum machine_mode);

extern void m68hc11_init_cumulative_args (CUMULATIVE_ARGS*, tree, rtx);

#ifdef ARGS_SIZE_RTX
extern enum direction m68hc11_function_arg_padding (enum machine_mode,
						    const_tree);
#endif

extern void m68hc11_function_epilogue (FILE*,int);

extern int m68hc11_is_far_symbol (rtx);
extern int m68hc11_is_trap_symbol (rtx);
extern int m68hc11_page0_symbol_p (rtx x);

extern HOST_WIDE_INT m68hc11_min_offset;
extern HOST_WIDE_INT m68hc11_max_offset;
extern int m68hc11_addr_mode;

#endif /* HAVE_MACHINE_MODES */
#endif /* RTX_CODE */
