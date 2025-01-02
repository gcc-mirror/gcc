/* Target Prototypes for R8C/M16C/M32C
   Copyright (C) 2005-2025 Free Software Foundation, Inc.
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

void m32c_conditional_register_usage (void);
unsigned int m32c_dwarf_frame_regnum (int);
int  m32c_eh_return_data_regno (int);
void m32c_emit_epilogue (void);
void m32c_emit_prologue (void);
int  m32c_epilogue_uses (int);
int  m32c_function_arg_regno_p (int);
void m32c_init_expanders (void);
int  m32c_initial_elimination_offset (int, int);
void m32c_output_reg_pop (FILE *, int);
void m32c_output_reg_push (FILE *, int);
poly_int64 m32c_push_rounding (poly_int64);
void m32c_register_pragmas (void);
void m32c_note_pragma_address (const char *, unsigned);
int  m32c_regno_ok_for_base_p (int);
int  m32c_trampoline_alignment (void);
int  m32c_trampoline_size (void);

#ifdef RTX_CODE

rtx  m32c_eh_return_stackadj_rtx (void);
void m32c_emit_eh_epilogue (rtx);
int  m32c_expand_cmpstr (rtx *);
int  m32c_expand_insv (rtx *);
int  m32c_expand_movcc (rtx *);
int  m32c_expand_cpymemhi (rtx *);
int  m32c_expand_movstr (rtx *);
void m32c_expand_neg_mulpsi3 (rtx *);
int  m32c_expand_setmemhi (rtx *);
bool m32c_matches_constraint_p (rtx, int);
bool m32c_illegal_subreg_p (rtx);
bool m32c_immd_dbl_mov (rtx *, machine_mode);
rtx  m32c_incoming_return_addr_rtx (void);
int  m32c_legitimize_reload_address (rtx *, machine_mode, int, int, int);
int  m32c_limit_reload_class (machine_mode, int);
bool m32c_mov_ok (rtx *, machine_mode);
char * m32c_output_compare (rtx_insn *, rtx *);
int  m32c_prepare_move (rtx *, machine_mode);
int  m32c_prepare_shift (rtx *, int, int);
int  m32c_reg_ok_for_base_p (rtx, int);
enum reg_class m32c_regno_reg_class (int);
rtx  m32c_return_addr_rtx (int);
const char *m32c_scc_pattern (rtx *, RTX_CODE);
int  m32c_secondary_reload_class (int, machine_mode, rtx);
int  m32c_split_move (rtx *, machine_mode, int);
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
