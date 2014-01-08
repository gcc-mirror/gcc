/* score-protos.h for Sunplus S+CORE processor
   Copyright (C) 2005-2014 Free Software Foundation, Inc.

   This file is part of GCC.

   GCC is free software; you can redistribute it and/or modify it
   under the terms of the GNU General Public License as published
   by the Free Software Foundation; either version 3, or (at your
   option) any later version.

   GCC is distributed in the hope that it will be useful, but WITHOUT
   ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
   or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
   License for more details.

   You should have received a copy of the GNU General Public License
   along with GCC; see the file COPYING3.  If not see
   <http://www.gnu.org/licenses/>.  */

#ifndef GCC_SCORE_PROTOS_H
#define GCC_SCORE_PROTOS_H

/* Machine Print.  */
enum score_mem_unit {SCORE_BYTE = 0, SCORE_HWORD = 1, SCORE_WORD = 2};

#define SCORE_ALIGN_UNIT(V, UNIT)   !(V & ((1 << UNIT) - 1))

extern void score_prologue (void);
extern void score_epilogue (int sibcall_p);
extern void score_call (rtx *ops, bool sib);
extern void score_call_value (rtx *ops, bool sib);
extern void score_movdi (rtx *ops);
extern void score_zero_extract_andi (rtx *ops);
extern const char * score_linsn (rtx *ops, enum score_mem_unit unit, bool sign);
extern const char * score_sinsn (rtx *ops, enum score_mem_unit unit);
extern const char * score_limm (rtx *ops);
extern const char * score_move (rtx *ops);
extern bool score_unaligned_load (rtx* ops);
extern bool score_unaligned_store (rtx* ops);
extern bool score_block_move (rtx* ops);
extern int score_address_cost (rtx addr, enum machine_mode mode,
			       addr_space_t as, bool speed);
extern int score_address_p (enum machine_mode mode, rtx x, int strict);
extern int score_reg_class (int regno);
extern int score_hard_regno_mode_ok (unsigned int, enum machine_mode);
extern int score_const_ok_for_letter_p (HOST_WIDE_INT value, char c);
extern int score_extra_constraint (rtx op, char c);
extern rtx score_return_addr (int count, rtx frame);
extern int score_regno_mode_ok_for_base_p (int regno, int strict);
extern void score_init_cumulative_args (CUMULATIVE_ARGS *cum,
                                        tree fntype, rtx libname);
extern void score_declare_object (FILE *stream, const char *name,
                                  const char *directive, const char *fmt, ...);
extern int score_output_external (FILE *file, tree decl, const char *name);
extern enum reg_class score_secondary_reload_class (enum reg_class rclass,
                                                    enum machine_mode mode,
                                                    rtx x);
extern rtx score_function_value (const_tree valtype, const_tree func,
                                 enum machine_mode mode);
extern enum reg_class score_preferred_reload_class (rtx x,
                                                    enum reg_class rclass);
extern HOST_WIDE_INT score_initial_elimination_offset (int from, int to);
extern void score_print_operand (FILE *file, rtx op, int letter);
extern void score_print_operand_address (FILE *file, rtx addr);
extern int score_symbolic_constant_p (rtx x,
                                      enum score_symbol_type *symbol_type);
extern void score_movsicc (rtx *ops);
extern const char * score_select_add_imm (rtx *ops, bool set_cc);
extern const char * score_select (rtx *ops, const char *inst_pre, bool commu,
                                  const char *letter, bool set_cc);
extern const char * score_output_casesi (rtx *operands);
extern const char * score_rpush (rtx *ops);
extern const char * score_rpop (rtx *ops);
extern bool score_rtx_costs (rtx x, int code, int outer_code, int opno,
			     int *total, bool speed);

#ifdef RTX_CODE
extern enum machine_mode score_select_cc_mode (enum rtx_code op, rtx x, rtx y);
#endif

extern struct extern_list *extern_head;

#endif /* GCC_SCORE_PROTOS_H  */
