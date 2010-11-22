/* Target Prototypes for R8C/M16C/M32C
   Copyright (C) 2005, 2007, 2008, 2010
   Free Software Foundation, Inc.
   Contributed by Red Hat.

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

#define MM enum machine_mode
#define UINT unsigned int

void m32c_conditional_register_usage (void);
int  m32c_const_ok_for_constraint_p (HOST_WIDE_INT, char, const char *);
UINT m32c_dwarf_frame_regnum (int);
int  m32c_eh_return_data_regno (int);
void m32c_emit_epilogue (void);
void m32c_emit_prologue (void);
int  m32c_epilogue_uses (int);
int  m32c_extra_address_constraint (char, const char *);
int  m32c_extra_memory_constraint (char, const char *);
int  m32c_function_arg_regno_p (int);
void m32c_init_expanders (void);
int  m32c_initial_elimination_offset (int, int);
void m32c_output_reg_pop (FILE *, int);
void m32c_output_reg_push (FILE *, int);
int  m32c_print_operand_punct_valid_p (int);
unsigned int  m32c_push_rounding (int);
int  m32c_reg_class_from_constraint (char, const char *);
void m32c_register_pragmas (void);
void m32c_note_pragma_address (const char *, unsigned);
int  m32c_regno_ok_for_base_p (int);
int  m32c_trampoline_alignment (void);
int  m32c_trampoline_size (void);

#ifdef RTX_CODE

int  m32c_cannot_change_mode_class (MM, MM, int);
int  m32c_class_max_nregs (int, MM);
rtx  m32c_eh_return_stackadj_rtx (void);
void m32c_emit_eh_epilogue (rtx);
int  m32c_expand_cmpstr (rtx *);
int  m32c_expand_insv (rtx *);
int  m32c_expand_movcc (rtx *);
int  m32c_expand_movmemhi (rtx *);
int  m32c_expand_movstr (rtx *);
void m32c_expand_neg_mulpsi3 (rtx *);
int  m32c_expand_setmemhi (rtx *);
int  m32c_extra_constraint_p (rtx, char, const char *);
int  m32c_extra_constraint_p2 (rtx, char, const char *);
int  m32c_hard_regno_nregs (int, MM);
int  m32c_hard_regno_ok (int, MM);
bool m32c_illegal_subreg_p (rtx);
bool m32c_immd_dbl_mov (rtx *, MM);
rtx  m32c_incoming_return_addr_rtx (void);
int  m32c_legitimate_constant_p (rtx);
int  m32c_legitimize_reload_address (rtx *, MM, int, int, int);
int  m32c_limit_reload_class (MM, int);
int  m32c_modes_tieable_p (MM, MM);
bool m32c_mov_ok (rtx *, MM);
char * m32c_output_compare (rtx, rtx *);
int  m32c_preferred_output_reload_class (rtx, int);
int  m32c_preferred_reload_class (rtx, int);
int  m32c_prepare_move (rtx *, MM);
int  m32c_prepare_shift (rtx *, int, int);
void m32c_print_operand (FILE *, rtx, int);
void m32c_print_operand_address (FILE *, rtx);
int  m32c_reg_ok_for_base_p (rtx, int);
enum reg_class m32c_regno_reg_class (int);
rtx  m32c_return_addr_rtx (int);
const char *m32c_scc_pattern (rtx *, RTX_CODE);
int  m32c_secondary_reload_class (int, MM, rtx);
int  m32c_split_move (rtx *, MM, int);
int  m32c_split_psi_p (rtx *);
int current_function_special_page_vector (rtx);

#endif

#ifdef TREE_CODE

tree m32c_gimplify_va_arg_expr (tree, tree, gimple_seq *, gimple_seq *);
void m32c_init_cumulative_args (CUMULATIVE_ARGS *, tree, rtx, tree, int);
bool m32c_promote_function_return (const_tree);
int  m32c_special_page_vector_p (tree);
void m32c_output_aligned_common (FILE *, tree, const char *,
				 int, int, int);

#endif

#undef MM
#undef UINT
