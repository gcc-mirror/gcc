/* Prototypes for exported functions defined in m68hc11.c
   Copyright (C) 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.
   Contributed by Stephane Carrez (stcarrez@nerim.fr)

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


extern int m68hc11_override_options PARAMS((void));
extern int m68hc11_optimization_options PARAMS((int,int));
extern void m68hc11_conditional_register_usage PARAMS((void));
extern int hard_regno_mode_ok PARAMS((int, enum machine_mode));
extern int m68hc11_hard_regno_rename_ok PARAMS((int, int));

extern int m68hc11_total_frame_size PARAMS((void));
extern int m68hc11_initial_frame_pointer_offset PARAMS((void));
extern int m68hc11_initial_elimination_offset PARAMS((int, int));

extern void expand_prologue PARAMS((void));
extern void expand_epilogue PARAMS((void));

extern void m68hc11_asm_file_start PARAMS((FILE*, const char*));

#ifdef TREE_CODE
extern void m68hc11_function_arg_advance PARAMS((CUMULATIVE_ARGS*,
                                                 enum machine_mode,
                                                 tree,
                                                 int));
#endif

#ifdef RTX_CODE
extern void m68hc11_initialize_trampoline PARAMS((rtx, rtx, rtx));

extern rtx m68hc11_expand_compare_and_branch PARAMS((enum rtx_code,
                                                     rtx, rtx, rtx));
extern enum reg_class preferred_reload_class PARAMS((rtx, enum reg_class));

extern int m68hc11_go_if_legitimate_address PARAMS((rtx,
                                                    enum machine_mode,
                                                    int));

extern int m68hc11_legitimize_address PARAMS((rtx*, rtx, enum machine_mode));

extern void m68hc11_notice_update_cc PARAMS((rtx, rtx));
extern void m68hc11_notice_keep_cc PARAMS((rtx));

extern void m68hc11_reorg PARAMS((rtx));

extern void m68hc11_gen_movqi PARAMS((rtx, rtx*));
extern void m68hc11_gen_movhi PARAMS((rtx, rtx*));
extern void m68hc11_gen_rotate PARAMS((enum rtx_code, rtx, rtx*));

extern void m68hc11_output_swap PARAMS((rtx,rtx*));

extern int next_insn_test_reg PARAMS((rtx,rtx));

extern void print_operand PARAMS((FILE*,rtx,int));
extern void print_operand_address PARAMS((FILE*,rtx));

extern int m68hc11_reload_operands PARAMS((rtx*));

extern int dead_register_here PARAMS((rtx, rtx));

extern int push_pop_operand_p PARAMS((rtx));
extern void m68hc11_split_move PARAMS((rtx, rtx, rtx));
extern void m68hc11_split_compare_and_branch PARAMS((enum rtx_code,
                                                     rtx, rtx, rtx));
extern void aux_restore_IX_IY PARAMS((rtx));
extern void aux_validate_IX_IY PARAMS((rtx));

extern rtx m68hc11_gen_lowpart PARAMS((enum machine_mode, rtx));
extern rtx m68hc11_gen_highpart PARAMS((enum machine_mode, rtx));

#ifdef HAVE_MACHINE_MODES
extern int m68hc11_memory_move_cost PARAMS((enum machine_mode, enum reg_class,
                                           int));
extern int m68hc11_register_move_cost PARAMS((enum machine_mode,
					      enum reg_class, enum reg_class));
extern int m68hc11_rtx_costs PARAMS((rtx, enum rtx_code, enum rtx_code));
extern int m68hc11_address_cost PARAMS((rtx));


extern void m68hc11_emit_libcall PARAMS((const char*, enum rtx_code,
                                         enum machine_mode, enum machine_mode,
                                         int, rtx*));
extern int m68hc11_small_indexed_indirect_p PARAMS((rtx, enum machine_mode));
extern int m68hc11_symbolic_p PARAMS((rtx, enum machine_mode));
extern int m68hc11_indirect_p PARAMS((rtx, enum machine_mode));
extern int go_if_legitimate_address2 PARAMS((rtx, enum machine_mode, int));

extern int reg_or_indexed_operand PARAMS((rtx,enum machine_mode));
extern int tst_operand PARAMS((rtx,enum machine_mode));
extern int cmp_operand PARAMS((rtx,enum machine_mode));
extern int memory_indexed_operand PARAMS((rtx, enum machine_mode));

extern void m68hc11_split_logical PARAMS((enum machine_mode, int, rtx*));

extern int m68hc11_register_indirect_p PARAMS((rtx, enum machine_mode));

extern int symbolic_memory_operand PARAMS((rtx, enum machine_mode));

extern int memory_reload_operand PARAMS((rtx, enum machine_mode));
extern int stack_register_operand PARAMS((rtx, enum machine_mode));
extern int d_register_operand PARAMS((rtx, enum machine_mode));
extern int hard_addr_reg_operand PARAMS((rtx, enum machine_mode));
extern int arith_src_operand PARAMS((rtx, enum machine_mode));
extern int m68hc11_logical_operator PARAMS((rtx, enum machine_mode));
extern int m68hc11_arith_operator PARAMS((rtx, enum machine_mode));
extern int m68hc11_non_shift_operator PARAMS((rtx, enum machine_mode));
extern int m68hc11_shift_operator PARAMS((rtx, enum machine_mode));
extern int m68hc11_unary_operator PARAMS((rtx, enum machine_mode));
extern int m68hc11_eq_compare_operator PARAMS((rtx, enum machine_mode));
extern int non_push_operand PARAMS((rtx, enum machine_mode));
extern int hard_reg_operand PARAMS((rtx, enum machine_mode));
extern int soft_reg_operand PARAMS((rtx, enum machine_mode));
extern int reg_or_some_mem_operand PARAMS((rtx, enum machine_mode));

#if defined TREE_CODE
extern void m68hc11_init_cumulative_args PARAMS((CUMULATIVE_ARGS*,
                                                 tree,
                                                 rtx));

extern rtx m68hc11_function_arg PARAMS((const CUMULATIVE_ARGS* ,
                                        enum machine_mode,
                                        tree, int));
extern int m68hc11_function_arg_pass_by_reference PARAMS((const CUMULATIVE_ARGS*,
                                                          enum machine_mode,
                                                          tree,
                                                          int));
extern int m68hc11_function_arg_padding PARAMS((enum machine_mode, tree));

extern void m68hc11_function_epilogue PARAMS((FILE*,int));

extern int m68hc11_is_far_symbol PARAMS((rtx));
extern int m68hc11_is_trap_symbol PARAMS((rtx));

#endif /* TREE_CODE */

extern HOST_WIDE_INT m68hc11_min_offset;
extern HOST_WIDE_INT m68hc11_max_offset;

#endif /* HAVE_MACHINE_MODES */
#endif /* RTX_CODE */

